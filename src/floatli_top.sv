// Copyright 2019-2020 ETH Zurich and University of Bologna.
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

// Authors: Luca Bertaccini <lbertaccini@iis.ee.ethz.ch>
//          Stefan Mach <smach@iis.ee.ethz.ch>

module floatli_top #(
  // FPU configuration
  parameter fpnew_pkg::fpu_features_t       Features       = fpnew_pkg::RV32F,
  parameter fpnew_pkg::fpu_implementation_t Implementation = fpnew_pkg::DEFAULT_NOREGS,
  parameter type                            TagType        = logic,
  // Do not change
  localparam int unsigned WIDTH        = Features.Width,
  localparam int unsigned NUM_OPERANDS = 3
) (
  input logic                               clk_i,
  input logic                               rst_ni,
  // Input signals
  input logic [NUM_OPERANDS-1:0][WIDTH-1:0] operands_i,
  input fpnew_pkg::roundmode_e              rnd_mode_i,
  input fpnew_pkg::operation_e              op_i,
  input logic                               op_mod_i,
  input fpnew_pkg::fp_format_e              src_fmt_i,
  input fpnew_pkg::fp_format_e              dst_fmt_i,
  input fpnew_pkg::int_format_e             int_fmt_i,
  input TagType                             tag_i,
  // Input Handshake
  input  logic                              in_valid_i,
  output logic                              in_ready_o,
  input  logic                              flush_i,
  // Output signals
  output logic [WIDTH-1:0]                  result_o,
  output fpnew_pkg::status_t                status_o,
  output TagType                            tag_o,
  // Output handshake
  output logic                              out_valid_o,
  input  logic                              out_ready_i,
  // Indication of valid data in flight
  output logic                              busy_o
);

  localparam int unsigned NUM_FORMATS  = 2; // fpnew_pkg::NUM_FP_FORMATS
                                            // just FP32 and FP64 are allowed

  localparam fpnew_pkg::fp_format_e  FpFormat = fpnew_pkg::get_format(Features);
  logic [NUM_FORMATS-1:0][NUM_OPERANDS-1:0] is_boxed_tmp;
  logic                  [NUM_OPERANDS-1:0] is_boxed;

  // NaN-boxing check
  for (genvar fmt = 0; fmt < int'(NUM_FORMATS); fmt++) begin : gen_nanbox_check
    localparam int unsigned FP_WIDTH = fpnew_pkg::fp_width(fpnew_pkg::fp_format_e'(fmt));
    // NaN boxing is only generated if it's enabled and needed
    if (Features.EnableNanBox && (FP_WIDTH < WIDTH)) begin : check
      for (genvar op = 0; op < int'(NUM_OPERANDS); op++) begin : operands
        assign is_boxed_tmp[fmt][op] = operands_i[op][WIDTH-1:FP_WIDTH] == '1;
      end
    end else begin : no_check
      assign is_boxed_tmp[fmt] = '1;
    end
  end

  always_comb begin : select_is_boxed
    case (fpnew_pkg::get_opgroup(op_i))
      fpnew_pkg::ADDMUL  : is_boxed = (FpFormat == fpnew_pkg::FP32) ? is_boxed_tmp[0]
                                                                    : is_boxed_tmp[1];
      fpnew_pkg::DIVSQRT : is_boxed = '0;
      fpnew_pkg::NONCOMP : begin
                             is_boxed[2]   = 1'b1;
                             is_boxed[1:0] = (FpFormat == fpnew_pkg::FP32) ? is_boxed_tmp[0][1:0]
                                                                           : is_boxed_tmp[1][1:0];
                           end
      fpnew_pkg::CONV    : begin
                             is_boxed[2]   = '0;
                             is_boxed[1]   = is_boxed_tmp[1][0];
                             is_boxed[0]   = is_boxed_tmp[0][0];
                           end
      default : is_boxed = '0;
    endcase
  end


  if (Features.Width == 64) begin
    localparam FP64_WIDTH = 64;

    //input  FP64
    logic                                     in_valid_FP64;
    logic                                     out_ready_FP64;
    //output FP64
    logic                                     in_ready_FP64;
    logic                                     out_valid_FP64;
    TagType                                   tag_FP64;
    fpnew_pkg::status_t                       status_FP64;
    logic                                     busy_FP64;

    logic                                     extension_bit_FP64;
    fpnew_pkg::classmask_e                    class_mask_FP64;
    logic                                     is_class_FP64;

    logic [NUM_OPERANDS-1:0][FP64_WIDTH-1:0] operands_FP64;
    logic                   [FP64_WIDTH-1:0] result_FP64;

    always_comb begin : select_operands_FP64
      case (fpnew_pkg::get_opgroup(op_i))
        fpnew_pkg::ADDMUL  : begin
                               operands_FP64[2] = operands_i[2][FP64_WIDTH-1:0];
                               operands_FP64[1] = operands_i[1][FP64_WIDTH-1:0];
                               operands_FP64[0] = operands_i[0][FP64_WIDTH-1:0];
                             end
        fpnew_pkg::DIVSQRT : operands_FP64 = '0;
        fpnew_pkg::NONCOMP : begin
                               operands_FP64[2]   = '0;
                               operands_FP64[1] = operands_i[1][FP64_WIDTH-1:0];
                               operands_FP64[0] = operands_i[0][FP64_WIDTH-1:0];
                             end
        fpnew_pkg::CONV    : begin
                               operands_FP64[2:1] = '0;
                               operands_FP64[0]   = operands_i[0][FP64_WIDTH-1:0];
                             end
        default : operands_FP64 = '0;
      endcase
    end


    floatli_fma_multi #(
      .FpFormat        ( fpnew_pkg::FP64        ),
      .TagType         ( TagType                ),
      .AuxType         ( logic                  )
    ) i_fma_noncomp_cast_FP64 (
      .clk_i           ( clk_i                  ),
      .rst_ni          ( rst_ni                 ),
      // Input signals
      .operands_i      ( operands_FP64          ), // 3 operands ADDMUL, 2 operands NONCOMP, 1 operand CONV
      .is_boxed_i      ( is_boxed               ), // 3 operands ADDMUL, 2 operands NONCOMP, 1 operand CONV
      .rnd_mode_i      ( rnd_mode_i             ),
      .op_i            ( op_i                   ),
      .op_mod_i        ( op_mod_i               ),
      .tag_i           ( tag_i                  ),
      .aux_i           ( '0 ),
      // Input Handshake
      .in_valid_i      ( in_valid_FP64          ),
      .in_ready_o      ( in_ready_FP64          ),
      .flush_i         ( flush_i                ),

      .src_fmt_i       ( src_fmt_i              ),  // cast
      .dst_fmt_i       ( dst_fmt_i              ),  // cast
      .int_fmt_i       ( int_fmt_i              ),  // cast
      // Output signals
      .result_o        ( result_FP64            ),
      .status_o        ( status_FP64            ),
      .extension_bit_o ( extension_bit_FP64     ),
      .class_mask_o    ( class_mask_FP64        ),  // non_comp
      .is_class_o      ( is_class_FP64          ),  // non_comp
      .tag_o           ( tag_FP64               ),
      .aux_o           (  ),
      // Output handshake
      .out_valid_o     ( out_valid_FP64         ),
      .out_ready_i     ( out_ready_FP64         ),
      // Indication of valid data in flight
      .busy_o          ( busy_FP64              )
    );

    always_comb
    begin
      //input
      in_valid_FP64  = in_valid_i;
      out_ready_FP64 = out_ready_i;
      //output
      in_ready_o     = in_ready_FP64;
      out_valid_o    = out_valid_FP64;
      tag_o          = tag_FP64;
      result_o       = result_FP64;
      status_o       = status_FP64;
      busy_o         = busy_FP64;
    end
  end
  else begin
    localparam FP32_WIDTH = 32;
    //input  FP64
    logic                                     in_valid_FP32;
    logic                                     out_ready_FP32;
    //output FP64
    logic                                     in_ready_FP32;
    logic                                     out_valid_FP32;
    TagType                                   tag_FP32;
    fpnew_pkg::status_t                       status_FP32;
    logic                                     busy_FP32;

    logic                                     extension_bit_FP32;
    fpnew_pkg::classmask_e                    class_mask_FP32;
    logic                                     is_class_FP32;

    logic [NUM_OPERANDS-1:0][FP32_WIDTH-1:0] operands_FP32;
    logic                   [FP32_WIDTH-1:0] result_FP32;

    always_comb begin : select_operands_FP32
      case (fpnew_pkg::get_opgroup(op_i))
        fpnew_pkg::ADDMUL  : begin
                               operands_FP32[2] = operands_i[2][FP32_WIDTH-1:0];
                               operands_FP32[1] = operands_i[1][FP32_WIDTH-1:0];
                               operands_FP32[0] = operands_i[0][FP32_WIDTH-1:0];
                             end
        fpnew_pkg::DIVSQRT : operands_FP32 = '0;
        fpnew_pkg::NONCOMP : begin
                               operands_FP32[2]   = '0;
                               operands_FP32[1] = operands_i[1][FP32_WIDTH-1:0];
                               operands_FP32[0] = operands_i[0][FP32_WIDTH-1:0];
                             end
        fpnew_pkg::CONV    : begin
                               operands_FP32[2:1] = '0;
                               operands_FP32[0]   = operands_i[0][FP32_WIDTH-1:0];
                             end
        default : operands_FP32 = '0;
      endcase
    end


    floatli_fma_multi #(
      .FpFormat        ( fpnew_pkg::FP32        ),
      .TagType         ( TagType                ),
      .AuxType         ( logic                  )
    ) i_fma_noncomp_cast_FP32 (
      .clk_i           ( clk_i                  ),
      .rst_ni          ( rst_ni                 ),
      // Input signals
      .operands_i      ( operands_FP32          ), // 3 operands ADDMUL, 2 operands NONCOMP, 1 operand CONV
      .is_boxed_i      ( is_boxed               ), // 3 operands ADDMUL, 2 operands NONCOMP, 1 operand CONV
      .rnd_mode_i      ( rnd_mode_i             ),
      .op_i            ( op_i                   ),
      .op_mod_i        ( op_mod_i               ),
      .tag_i           ( tag_i                  ),
      .aux_i           ( '0 ),
      // Input Handshake
      .in_valid_i      ( in_valid_FP32          ),
      .in_ready_o      ( in_ready_FP32          ),
      .flush_i         ( flush_i                ),

      .src_fmt_i       ( src_fmt_i              ),  // cast
      .dst_fmt_i       ( dst_fmt_i              ),  // cast
      .int_fmt_i       ( int_fmt_i              ),  // cast
      // Output signals
      .result_o        ( result_FP32            ),
      .status_o        ( status_FP32            ),
      .extension_bit_o ( extension_bit_FP32     ),
      .class_mask_o    ( class_mask_FP32        ),     // non_comp
      .is_class_o      ( is_class_FP32          ),     // non_comp
      .tag_o           ( tag_FP32               ),
      .aux_o           (  ),
      // Output handshake
      .out_valid_o     ( out_valid_FP32         ),
      .out_ready_i     ( out_ready_FP32         ),
      // Indication of valid data in flight
      .busy_o          ( busy_FP32              )
    );

    always_comb
    begin
      //input
      in_valid_FP32  = in_valid_i;
      out_ready_FP32 = out_ready_i;
      //output
      in_ready_o     = in_ready_FP32;
      out_valid_o    = out_valid_FP32;
      tag_o          = tag_FP32;
      result_o       = result_FP32;
      status_o       = status_FP32;
      busy_o         = busy_FP32;
    end
  end
endmodule
