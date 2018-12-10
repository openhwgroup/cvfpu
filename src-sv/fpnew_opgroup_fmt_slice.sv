// Copyright (c) 2018 ETH Zurich, University of Bologna
// All rights reserved.
//
// This code is under development and not yet released to the public.
// Until it is released, the code is under the copyright of ETH Zurich and
// the University of Bologna, and may contain confidential and/or unpublished
// work. Any reuse/redistribution is strictly forbidden without written
// permission from ETH Zurich.
//
// Bug fixes and contributions will eventually be released under the
// SolderPad open hardware license in the context of the PULP platform
// (http://www.pulp-platform.org), under the copyright of ETH Zurich and the
// University of Bologna.

// Author: Stefan Mach <smach@iis.ee.ethz.ch>

module fpnew_opgroup_fmt_slice #(
  parameter fpnew_pkg::opgroup_e     OpGroup       = fpnew_pkg::ADDMUL,
  parameter fpnew_pkg::fp_format_e   FpFormat      = fpnew_pkg::FP32,
  // FPU configuration
  parameter int unsigned             Width         = 32,
  parameter logic                    EnableNanBoxing = 1'b1,
  parameter logic                    EnableVectors = 1'b1,
  parameter int unsigned             NumPipeRegs   = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig    = fpnew_pkg::BEFORE,
  parameter type                     TagType       = logic,
  // Do not change
  localparam int unsigned NUM_OPERANDS = fpnew_pkg::num_operands(OpGroup),
) (
  input logic                               clk_i,
  input logic                               rst_ni,
  // Input signals
  input logic [0:NUM_OPERANDS-1][Width-1:0] operands_i,
  input logic [0:NUM_OPERANDS-1]            is_boxed_i,
  input fpnew_pkg::roundmode_e              rnd_mode_i,
  input fpnew_pkg::operation_e              op_i,
  input logic                               op_mod_i,
  input logic                               vectorial_op_i,
  input TagType                             tag_i,
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
  localparam int unsigned NUM_LANES = fpnew_pkg::num_lanes(Width, FpFormat, EnableVectors);


  logic [0:NUM_LANES-1]          lane_in_ready, lane_out_valid; // Handshake signals for the lanes
  logic                          vectorial_op;

  logic [NUM_LANES*FP_WIDTH-1:0]      slice_result;
  fpnew_pkg::status_t [0:NUM_LANES-1] lane_status;
  logic [0:NUM_LANES-1]               lane_ext_bit; // only the first one is actually used
  TagType [0:NUM_LANES-1]             lane_tags; // only the first one is actually used
  logic [0:NUM_LANES-1]               lane_vectorial, lane_busy; // dito

  logic result_is_vector;

  // -----------
  // Input Side
  // -----------
  assign in_ready_o   = lane_in_ready[0]; // Upstream ready is given by first lane
  assign vectorial_op = vectorial_op_i & EnableVectors; // only do vectorial stuff if enabled

  // ---------------
  // Generate Lanes
  // ---------------
  for (genvar lane = 0; lane < int'(NUM_LANES); lane++) begin : gen_num_lanes
    // Generate instances only if needed, lane 0 always generated
    if ((lane == 0) || EnableVectors) begin : active_lane
      logic in_valid, out_valid, out_ready; // lane-local handshake

      logic [0:NUM_OPERANDS-1][FP_WIDTH-1:0] local_operands;          // lane-local oprands
      logic                                  input_boxed;
      logic [FP_WIDTH-1:0]                   op_result, local_result; // lane-local results
      fpnew_pkg::status_t                    op_status;

      assign in_valid = in_valid_i & ((lane == 0) | vectorial_op); // upper lanes only for vectors
      // Slice out the operands for this lane
      always_comb begin : prepare_input
        for (int i = 0; i < int'(NUM_OPERANDS); i++) begin
          local_operands[i] = operands_i[i][(unsigned'(lane)+1)*FP_WIDTH-1:unsigned'(lane)*FP_WIDTH];
          input_boxed       = ((i == 0) & ~vectorial_op) ? is_boxed_i[i] : 1'b1;
        end
      end

      // Instantiate the operation from the selected opgroup
      if (OpGroup == fpnew_pkg::ADDMUL) begin : lane_instance
        fpnew_fma #(
          .FpFormat    ( FpFormat    ),
          .NumPipeRegs ( NumPipeRegs ),
          .PipeConfig  ( PipeConfig  ),
          .TagType     ( TagType     ),
          .AuxType     ( logic       )
        ) i_fma (
          .clk_i,
          .rst_ni,
          .operands_i      ( local_operands       ),
          .is_boxed_i      ( input_boxed          ),
          .rnd_mode_i,
          .op_i,
          .op_mod_i,
          .tag_i,
          .aux_i           ( vectorial_op         ), // Remember whether operation was vectorial
          .in_valid_i      ( in_valid             ),
          .in_ready_o      ( lane_in_ready[lane]  ),
          .flush_i,
          .result_o        ( op_result            ),
          .status_o        ( op_status            ),
          .extension_bit_o ( lane_ext_bit[lane]   ),
          .tag_o           ( lane_tags[lane]      ),
          .aux_o           ( lane_vectorial[lane] ),
          .out_valid_o     ( out_valid            ),
          .out_ready_i     ( out_ready            ),
          .busy_o          ( lane_busy[lane]      )
        );
      end else if (OpGroup == fpnew_pkg::DIVSQRT) begin : lane_instance
        // fpnew_divsqrt #(
        //   .FpFormat   (FpFormat),
        //   .NumPipeRegs(NumPipeRegs),
        //   .PipeConfig (PipeConfig),
        //   .TagType    (TagType),
        //   .AuxType    (logic)
        // ) i_divsqrt (
        //   .clk_i,
        //   .rst_ni,
        //   .operands_i      ( local_operands       ),
        //   .is_boxed_i      ( input_boxed          ),
        //   .rnd_mode_i,
        //   .op_i,
        //   .op_mod_i,
        //   .tag_i,
        //   .aux_i           ( vectorial_op         ), // Remember whether operation was vectorial
        //   .in_valid_i      ( in_valid             ),
        //   .in_ready_o      ( lane_in_ready[lane]  ),
        //   .flush_i,
        //   .result_o        ( op_result            ),
        //   .status_o        ( op_status            ),
        //   .extension_bit_o ( lane_ext_bit[lane]   ),
        //   .tag_o           ( lane_tags[lane]      ),
        //   .aux_o           ( lane_vectorial[lane] ),
        //   .out_valid_o     ( out_valid            ),
        //   .out_ready_i     ( out_ready            ),
        //   .busy_o          ( lane_busy[lane]      )
        // );
      end else if (OpGroup == fpnew_pkg::NONCOMP) begin : lane_instance
        fpnew_noncomp #(
          .FpFormat   (FpFormat),
          .NumPipeRegs(NumPipeRegs),
          .PipeConfig (PipeConfig),
          .TagType    (TagType),
          .AuxType    (logic)
        ) i_noncomp (
          .clk_i,
          .rst_ni,
          .operands_i      ( local_operands       ),
          .is_boxed_i      ( input_boxed          ),
          .rnd_mode_i,
          .op_i,
          .op_mod_i,
          .tag_i,
          .aux_i           ( vectorial_op         ), // Remember whether operation was vectorial
          .in_valid_i      ( in_valid             ),
          .in_ready_o      ( lane_in_ready[lane]  ),
          .flush_i,
          .result_o        ( op_result            ),
          .status_o        ( op_status            ),
          .extension_bit_o ( lane_ext_bit[lane]   ),
          .tag_o           ( lane_tags[lane]      ),
          .aux_o           ( lane_vectorial[lane] ),
          .out_valid_o     ( out_valid            ),
          .out_ready_i     ( out_ready            ),
          .busy_o          ( lane_busy[lane]      )
        );
      end // ADD OTHER OPTIONS HERE

      // Handshakes are only done if the lane is actually used
      assign out_ready            = out_ready_i & ((lane == 0) | result_is_vector);
      assign lane_out_valid[lane] = out_valid & ((lane == 0) | result_is_vector);

      // Properly NaN-box or sign-extend the slice result if not in use
      assign local_result      = lane_out_valid[lane] ? op_result : '{default: lane_ext_bit[0]};
      assign lane_status[lane] = lane_out_valid[lane] ? op_status : '0;

    // Otherwise generate constant sign-extension
    end else begin
      assign lane_out_valid[lane] = 1'b0; // unused lane
      assign lane_in_ready[lane]  = 1'b0; // unused lane
      assign local_result         = '{default: lane_ext_bit[0]}; // sign-extend/nan box
      assign lane_status[lane]    = '0;
      assign lane_busy[lane]      = 1'b0;
    end

    // Insert lane result into slice result
    assign slice_result[(unsigned'(lane)+1)*FP_WIDTH-1:unsigned'(lane)*FP_WIDTH] = local_result;
  end

  // ------------
  // Output Side
  // ------------
  assign result_is_vector = lane_vectorial[0];

  assign result_o[NUM_LANES*FP_WIDTH-1:0] = slice_result;
  assign extension_bit_o                  = lane_ext_bit[0]; // don't care about upper ones
  assign tag_o                            = lane_tags[0];    // don't care about upper ones
  assign busy_o                           = (| lane_busy);

  assign out_valid_o                      = lane_out_valid[0]; // don't care about upper ones

  // If needed, extend the result to the full width and collapse the status
  always_comb begin : output_processing
    // Collapse the status
    automatic fpnew_pkg::status_t temp_status;
    temp_status = '0;
    for (int i = 0; i < int'(NUM_LANES); i++)
      temp_status |= lane_status[i];
    status_o = temp_status;
    // Extend result if wider than what is filled by lanes
    if (Width > NUM_LANES*FP_WIDTH) begin
      result_o[Width-1:NUM_LANES*FP_WIDTH] = '{default: extension_bit_o};
    end
  end
endmodule
