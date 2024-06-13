// Copyright 2019 ETH Zurich and University of Bologna.
//
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.
//
// SPDX-License-Identifier: SHL-0.51

// Author: Stefan Mach <smach@iis.ee.ethz.ch>

module fpnew_opgroup_fmt_slice #(
  parameter fpnew_pkg::opgroup_e     OpGroup       = fpnew_pkg::ADDMUL,
  parameter fpnew_pkg::fp_format_e   FpFormat      = fpnew_pkg::fp_format_e'(0),
  // FPU configuration
  parameter int unsigned             Width         = 32,
  parameter logic                    EnableVectors = 1'b1,
  parameter int unsigned             NumPipeRegs   = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig    = fpnew_pkg::BEFORE,
  parameter type                     TagType       = logic,
  parameter int unsigned             TrueSIMDClass = 0,
  // Do not change
  localparam int unsigned NUM_OPERANDS = fpnew_pkg::num_operands(OpGroup),
  localparam int unsigned NUM_LANES    = fpnew_pkg::num_lanes(Width, FpFormat, EnableVectors),
  localparam type         MaskType     = logic [NUM_LANES-1:0]
) (
  input logic                               clk_i,
  input logic                               rst_ni,
  // Input signals
  input logic [NUM_OPERANDS-1:0][Width-1:0] operands_i,
  input logic [NUM_OPERANDS-1:0]            is_boxed_i,
  input fpnew_pkg::roundmode_e              rnd_mode_i,
  input fpnew_pkg::operation_e              op_i,
  input logic                               op_mod_i,
  input logic                               vectorial_op_i,
  input TagType                             tag_i,
  input MaskType                            simd_mask_i,
  // Input Handshake
  input  logic                              in_valid_i,
  output logic                              in_ready_o,
  input  logic                              flush_i,
  // Output signals
  output logic [Width-1:0]                  result_o,
  output fpnew_pkg::status_t                status_o,
  output logic                              extension_bit_o,
  output TagType                            tag_o,
  // Output handshake
  output logic                              out_valid_o,
  input  logic                              out_ready_i,
  // Indication of valid data in flight
  output logic                              busy_o
);

  localparam int unsigned FP_WIDTH  = fpnew_pkg::fp_width(FpFormat);
  localparam int unsigned SIMD_WIDTH = unsigned'(Width/NUM_LANES);

  logic                 vectorial_op, cmp_op;

  logic [NUM_LANES*FP_WIDTH-1:0] slice_result;
  logic [Width-1:0]              slice_regular_result, slice_class_result, slice_vec_class_result;

  fpnew_pkg::status_t    [NUM_LANES-1:0] lane_status;
  logic                  [NUM_LANES-1:0] lane_ext_bit; // only the first one is actually used
  fpnew_pkg::classmask_e [NUM_LANES-1:0] lane_class_mask;
  logic                  [NUM_LANES-1:0] lane_masks;
  logic                  [NUM_LANES-1:0] lane_is_class; // only the first one is actually used

  logic result_is_vector, result_is_class, result_is_cmp;

  // -----------
  // Input Side
  // -----------
  assign vectorial_op = vectorial_op_i & EnableVectors; // only do vectorial stuff if enabled

  // ---------------
  // Generate Aux Chain
  // ---------------
  // Signals to transmit reg enable to other modules
  logic [NUM_LANES-1:0] in_lane_active, out_lane_active;
  logic [NUM_LANES-1:0][NumPipeRegs-1:0] lane_reg_enable;

  fpnew_aux #(
    .NumPipeRegs( NumPipeRegs          ),
    .TagType    ( TagType              ),
    .AuxType    ( logic                ),
    .NumLanes   ( NUM_LANES            )
  ) i_aux (
    .clk_i,
    .rst_ni,
    .tag_i,
    .aux_i               ( cmp_op             ),
    .is_vector_i         ( vectorial_op       ),
    .lane_active_i       ( in_lane_active     ),
    .in_valid_i,
    .in_ready_o,
    .flush_i,
    .tag_o,
    .aux_o               ( result_is_cmp      ),
    .is_vector_o         ( result_is_vector   ),
    .lane_active_o       ( out_lane_active    ),
    .out_valid_o,
    .out_ready_i,
    .busy_o,
    .reg_enable_o        ( /* Unused */       ),
    .vector_reg_enable_o ( /* Unused */       ),
    .lane_reg_enable_o   ( lane_reg_enable    )
  );

  // ---------------
  // Generate Lanes
  // ---------------
  for (genvar lane = 0; lane < int'(NUM_LANES); lane++) begin : gen_num_lanes
    logic [FP_WIDTH-1:0] local_result; // lane-local results
    logic                local_sign;

    // Generate instances only if needed, lane 0 always generated
    if ((lane == 0) || EnableVectors) begin : active_lane

      logic [NUM_OPERANDS-1:0][FP_WIDTH-1:0] local_operands; // lane-local operands
      logic [FP_WIDTH-1:0]                   op_result;      // lane-local results
      fpnew_pkg::status_t                    op_status;

      assign in_lane_active[lane] = (lane == 0) | vectorial_op; // upper lanes only for vectors

      // Slice out the operands for this lane
      always_comb begin : prepare_input
        for (int i = 0; i < int'(NUM_OPERANDS); i++) begin
          local_operands[i] = operands_i[i][(unsigned'(lane)+1)*FP_WIDTH-1:unsigned'(lane)*FP_WIDTH];
        end
      end

      // Instantiate the operation from the selected opgroup
      if (OpGroup == fpnew_pkg::ADDMUL) begin : lane_instance
        fpnew_fma #(
          .FpFormat    ( FpFormat             ),
          .NumPipeRegs ( NumPipeRegs          ),
          .PipeConfig  ( PipeConfig           )
        ) i_fma (
          .clk_i,
          .rst_ni,
          .operands_i      ( local_operands               ),
          .is_boxed_i      ( is_boxed_i[NUM_OPERANDS-1:0] ),
          .rnd_mode_i,
          .op_i,
          .op_mod_i,
          .mask_i          ( simd_mask_i[lane]            ),
          .result_o        ( op_result                    ),
          .status_o        ( op_status                    ),
          .extension_bit_o ( lane_ext_bit[lane]           ),
          .mask_o          ( lane_masks[lane]             ),
          .reg_enable_i    ( lane_reg_enable[lane]        )
        );
        assign lane_is_class[lane]   = 1'b0;
        assign lane_class_mask[lane] = fpnew_pkg::NEGINF;
      end else if (OpGroup == fpnew_pkg::NONCOMP) begin : lane_instance
        fpnew_noncomp #(
          .FpFormat   ( FpFormat             ),
          .NumPipeRegs( NumPipeRegs          ),
          .PipeConfig ( PipeConfig           )
        ) i_noncomp (
          .clk_i,
          .rst_ni,
          .operands_i      ( local_operands               ),
          .is_boxed_i      ( is_boxed_i[NUM_OPERANDS-1:0] ),
          .rnd_mode_i,
          .op_i,
          .op_mod_i,
          .mask_i          ( simd_mask_i[lane]            ),
          .result_o        ( op_result                    ),
          .status_o        ( op_status                    ),
          .extension_bit_o ( lane_ext_bit[lane]           ),
          .class_mask_o    ( lane_class_mask[lane]        ),
          .is_class_o      ( lane_is_class[lane]          ),
          .mask_o          ( lane_masks[lane]             ),
          .reg_enable_i    ( lane_reg_enable[lane]        )
        );
      end // ADD OTHER OPTIONS HERE

      // Properly NaN-box or sign-extend the slice result if not in use
      assign local_result      = out_lane_active[lane] ? op_result : '{default: lane_ext_bit[0]};
      assign lane_status[lane] = out_lane_active[lane] ? op_status : '0;

    // Otherwise generate constant sign-extension
    end else begin
      assign local_result         = '{default: lane_ext_bit[0]}; // sign-extend/nan box
      assign lane_status[lane]    = '0;
      assign lane_is_class[lane]  = 1'b0;
      assign in_lane_active[lane] = 1'b0; // Lane does not exist, it can never be active
    end

    // Insert lane result into slice result
    assign slice_result[(unsigned'(lane)+1)*FP_WIDTH-1:unsigned'(lane)*FP_WIDTH] = local_result;

    // Create Classification results
    if (TrueSIMDClass && SIMD_WIDTH >= 10) begin : vectorial_true_class // true vectorial class blocks are 10bits in size
      assign slice_vec_class_result[lane*SIMD_WIDTH +: 10] = lane_class_mask[lane];
      assign slice_vec_class_result[(lane+1)*SIMD_WIDTH-1 -: SIMD_WIDTH-10] = '0;
    end else if ((lane+1)*8 <= Width) begin : vectorial_class // vectorial class blocks are 8bits in size
      assign local_sign = (lane_class_mask[lane] == fpnew_pkg::NEGINF ||
                           lane_class_mask[lane] == fpnew_pkg::NEGNORM ||
                           lane_class_mask[lane] == fpnew_pkg::NEGSUBNORM ||
                           lane_class_mask[lane] == fpnew_pkg::NEGZERO);
      // Write the current block segment
      assign slice_vec_class_result[(lane+1)*8-1:lane*8] = {
        local_sign,  // BIT 7
        ~local_sign, // BIT 6
        lane_class_mask[lane] == fpnew_pkg::QNAN, // BIT 5
        lane_class_mask[lane] == fpnew_pkg::SNAN, // BIT 4
        lane_class_mask[lane] == fpnew_pkg::POSZERO
            || lane_class_mask[lane] == fpnew_pkg::NEGZERO, // BIT 3
        lane_class_mask[lane] == fpnew_pkg::POSSUBNORM
            || lane_class_mask[lane] == fpnew_pkg::NEGSUBNORM, // BIT 2
        lane_class_mask[lane] == fpnew_pkg::POSNORM
            || lane_class_mask[lane] == fpnew_pkg::NEGNORM, // BIT 1
        lane_class_mask[lane] == fpnew_pkg::POSINF
            || lane_class_mask[lane] == fpnew_pkg::NEGINF // BIT 0
      };
    end
  end

  // ------------
  // Output Side
  // ------------
  assign result_is_class  = lane_is_class[0];

  assign slice_regular_result = $signed({extension_bit_o, slice_result});

  localparam int unsigned CLASS_VEC_BITS = (NUM_LANES*8 > Width) ? 8 * (Width / 8) : NUM_LANES*8;

  // Pad out unused vec_class bits if each classify result is on 8 bits
  if (!(TrueSIMDClass && SIMD_WIDTH >= 10)) begin
    if (CLASS_VEC_BITS < Width) begin : pad_vectorial_class
      assign slice_vec_class_result[Width-1:CLASS_VEC_BITS] = '0;
    end
  end

  // localparam logic [Width-1:0] CLASS_VEC_MASK = 2**CLASS_VEC_BITS - 1;

  assign slice_class_result = result_is_vector ? slice_vec_class_result : lane_class_mask[0];

  // Select the proper result
  assign result_o = result_is_class ? slice_class_result : slice_regular_result;

  assign extension_bit_o = lane_ext_bit[0]; // upper lanes unused

  // Collapse the lane status
  always_comb begin : output_processing
    // Collapse the status
    automatic fpnew_pkg::status_t temp_status;
    temp_status = '0;
    for (int i = 0; i < int'(NUM_LANES); i++)
      temp_status |= lane_status[i] & {5{lane_masks[i]}};
    status_o = temp_status;
  end
endmodule
