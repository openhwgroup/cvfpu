// Copyright 2019-2021 ETH Zurich and University of Bologna.
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

// Author: Gianna Paulin <pauling@iis.ee.ethz.ch>
// Author: Luca Bertaccini <lbertaccini@iis.ee.ethz.ch>
// Author: Stefan Mach <smach@iis.ee.ethz.ch>

`include "common_cells/registers.svh"

module fpnew_sdotp_multi_wrapper #(
  parameter fpnew_pkg::fmt_logic_t   FpFmtConfig = '1,
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,

  // Do not change
  localparam fpnew_pkg::fmt_logic_t FpSrcFmtConfig = FpFmtConfig[0] ? (FpFmtConfig & 6'b001111) : (FpFmtConfig & 6'b000101),
  localparam fpnew_pkg::fmt_logic_t FpDstFmtConfig = fpnew_pkg::get_dotp_dst_fmts(FpSrcFmtConfig),
  localparam int                     SRC_WIDTH     = fpnew_pkg::max_fp_width(FpSrcFmtConfig),
  localparam int                     DST_WIDTH     = 2*fpnew_pkg::max_fp_width(FpSrcFmtConfig), // do not change, current assumption of sdotpex_multi
  localparam int                     OPERAND_WIDTH = 4*fpnew_pkg::max_fp_width(FpSrcFmtConfig), // do not change, current assumption of sdotpex_multi
  localparam int unsigned NUM_FORMATS              = fpnew_pkg::NUM_FP_FORMATS
) (
  input logic                      clk_i,
  input logic                      rst_ni,
  // Input signals
  input logic [2:0][OPERAND_WIDTH-1:0] operands_i, // 3 operands
  input logic [NUM_FORMATS-1:0][2:0]   is_boxed_i, // 3 operands
  input fpnew_pkg::roundmode_e         rnd_mode_i,
  input fpnew_pkg::operation_e         op_i,
  input logic                          op_mod_i,
  input fpnew_pkg::fp_format_e         src_fmt_i,
  input fpnew_pkg::fp_format_e         dst_fmt_i,
  input TagType                        tag_i,
  input AuxType                        aux_i,
  // Input Handshake
  input  logic                         in_valid_i,
  output logic                         in_ready_o,
  input  logic                         flush_i,
  // Output signals
  output logic [OPERAND_WIDTH-1:0]     result_o,
  output fpnew_pkg::status_t           status_o,
  output logic                         extension_bit_o,
  output TagType                       tag_o,
  output AuxType                       aux_o,
  // Output handshake
  output logic                         out_valid_o,
  input  logic                         out_ready_i,
  // Indication of valid data in flight
  output logic                         busy_o
);

  // ----------
  // Constants
  // ----------
  localparam int unsigned N_SRC_FMT_OPERANDS = 4;
  localparam int unsigned N_DST_FMT_OPERANDS = 1;

  // -----------------
  // Input processing
  // -----------------
  logic                             [NUM_FORMATS-1:0][DST_WIDTH-1:0] local_src_fmt_operand_a;  // lane-local operands
  logic                             [NUM_FORMATS-1:0][SRC_WIDTH-1:0] local_src_fmt_operand_b;  // lane-local operands
  logic                             [NUM_FORMATS-1:0][DST_WIDTH-1:0] local_src_fmt_operand_c;  // lane-local operands
  logic                             [NUM_FORMATS-1:0][SRC_WIDTH-1:0] local_src_fmt_operand_d;  // lane-local operands
  logic                                              [DST_WIDTH-1:0] local_dst_fmt_operands;  // lane-local operands
  logic [NUM_FORMATS-1:0][N_SRC_FMT_OPERANDS+N_DST_FMT_OPERANDS-1:0] local_is_boxed;  // lane-local operands
  logic                                          [OPERAND_WIDTH-1:0] local_result;  // lane-local operands


  // ----------------------------------
  // assign operands with dst format
  // ----------------------------------
  assign local_dst_fmt_operands = operands_i[2][DST_WIDTH-1:0];


  // ----------------------------------
  // assign operands with src format
  // ----------------------------------
  // NaN-boxing check
  for (genvar fmt = 0; fmt < int'(NUM_FORMATS); fmt++) begin : gen_nanbox

    localparam int unsigned FP_WIDTH         = fpnew_pkg::fp_width(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned FP_WIDTH_MIN     = fpnew_pkg::minimum(SRC_WIDTH, FP_WIDTH);
    localparam int unsigned FP_WIDTH_DST_MIN = fpnew_pkg::minimum(DST_WIDTH, FP_WIDTH);

    logic [N_SRC_FMT_OPERANDS-1:0][FP_WIDTH_DST_MIN-1:0] tmp_operands;     // lane-local operands

    always_comb begin : nanbox
      // shift operands to correct position
      tmp_operands[0] = operands_i[0] >> 0*FP_WIDTH;
      tmp_operands[1] = operands_i[1] >> 0*FP_WIDTH;
      tmp_operands[2] = operands_i[0] >> 1*FP_WIDTH;
      tmp_operands[3] = operands_i[1] >> 1*FP_WIDTH;
      // nan-box if needed
      local_src_fmt_operand_a[fmt] = '1;
      local_src_fmt_operand_b[fmt] = '1;
      local_src_fmt_operand_c[fmt] = '1;
      local_src_fmt_operand_d[fmt] = '1;
      if (op_i == fpnew_pkg::VSUM) begin
        local_src_fmt_operand_a[fmt][FP_WIDTH_DST_MIN-1:0] = tmp_operands[0][FP_WIDTH_DST_MIN-1:0];
        local_src_fmt_operand_b[fmt][FP_WIDTH_MIN-1:0]     = '1;
        local_src_fmt_operand_c[fmt][FP_WIDTH_DST_MIN-1:0] = tmp_operands[2][FP_WIDTH_DST_MIN-1:0];
        local_src_fmt_operand_d[fmt][FP_WIDTH_MIN-1:0]     = '1;
      end else begin
        local_src_fmt_operand_a[fmt][FP_WIDTH_MIN-1:0] = tmp_operands[0][FP_WIDTH_MIN-1:0];
        local_src_fmt_operand_b[fmt][FP_WIDTH_MIN-1:0] = tmp_operands[1][FP_WIDTH_MIN-1:0];
        local_src_fmt_operand_c[fmt][FP_WIDTH_MIN-1:0] = tmp_operands[2][FP_WIDTH_MIN-1:0];
        local_src_fmt_operand_d[fmt][FP_WIDTH_MIN-1:0] = tmp_operands[3][FP_WIDTH_MIN-1:0];
      end
      // take is_boxed info from external or set to 1 if boxed for dotp operation
      local_is_boxed[fmt][0] = is_boxed_i[fmt][0];
      local_is_boxed[fmt][1] = is_boxed_i[fmt][1];
      local_is_boxed[fmt][2] = is_boxed_i[fmt][0];
      local_is_boxed[fmt][3] = is_boxed_i[fmt][1];
      if(FP_WIDTH <= SRC_WIDTH) begin
        local_is_boxed[fmt][0] = '1;
        local_is_boxed[fmt][1] = '1;
        local_is_boxed[fmt][2] = '1;
        local_is_boxed[fmt][3] = '1;
      end
      // for dst format sized operand keep is_boxed input
      local_is_boxed[fmt][4] = is_boxed_i[src_fmt_i][2];
    end
  end

  fpnew_sdotp_multi #(
    .SrcDotpFpFmtConfig ( FpSrcFmtConfig ), // FP8, FP8ALT, FP16, FP16ALT
    .DstDotpFpFmtConfig ( FpDstFmtConfig ), // FP32, FP16, FP16ALT
    .NumPipeRegs        ( NumPipeRegs    ),
    .PipeConfig         ( PipeConfig     ),
    .TagType            ( TagType        ),
    .AuxType            ( AuxType        )
  ) i_fpnew_sdotp_multi (
    .clk_i,
    .rst_ni,
    .operand_a_i     ( local_src_fmt_operand_a[src_fmt_i] ),
    .operand_b_i     ( local_src_fmt_operand_b[src_fmt_i] ),
    .operand_c_i     ( local_src_fmt_operand_c[src_fmt_i] ),
    .operand_d_i     ( local_src_fmt_operand_d[src_fmt_i] ),
    .dst_operands_i  ( local_dst_fmt_operands             ), // 1 operand
    .is_boxed_i      ( local_is_boxed                     ),
    .rnd_mode_i,
    .op_i,
    .op_mod_i,
    .src_fmt_i, // format of the multiplicands
    .dst_fmt_i, // format of the addend and result
    .tag_i,
    .aux_i,
    .in_valid_i,
    .in_ready_o ,
    .flush_i,
    .result_o        ( local_result[DST_WIDTH-1:0] ),
    .status_o,
    .extension_bit_o,
    .tag_o,
    .aux_o,
    .out_valid_o,
    .out_ready_i,
    .busy_o
  );

  assign local_result[2*DST_WIDTH-1:DST_WIDTH] = '1;
  assign result_o                              = local_result;

endmodule
