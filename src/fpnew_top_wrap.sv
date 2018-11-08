// Copyright 2018 ETH Zurich and University of Bologna.
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License.  You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.
//
// Author: Michael Schaffner <schaffner@iis.ee.ethz.ch>, ETH Zurich
// Date: 08.11.2018
// Description: SV wrapper for VHDL unit fpnew_top.vhd

//typedef enum {NONE, PARALLEL, MERGED} unitType_t;

module fpnew_top_wrap #(
  parameter WIDTH                = 64,        // Narrower width will mask out fmts
  parameter TAG_WIDTH            = 0,         // Tag is sent along with operation
  //
  parameter RV64                 = 1,         // Enables 64-bit integer formats
  parameter RVF                  = 1,         // Enables FP32 format
  parameter RVD                  = 1,         // Enables FP64 format
  parameter Xf16                 = 1,         // Enables FP16 format
  parameter Xf16alt              = 1,         // Enables FP16alt format
  parameter Xf8                  = 1,         // Enables FP8 format
  parameter Xfvec                = 1,         // Generates vector for enabled formats
  //
  parameter TYPE_ADDMUL          = 1,         // NONE:0. PARALLEL:1, MERGED:2
  parameter TYPE_DIVSQRT         = 2,         // NONE:0. PARALLEL:1, MERGED:2
  parameter TYPE_NONCOMP         = 1,         // NONE:0. PARALLEL:1, MERGED:2
  parameter TYPE_CONV            = 1,         // NONE:0. PARALLEL:1, MERGED:2
  //
  parameter LATENCY_COMP_F       = 0,         // Latency of FP32 comp. ops
  parameter LATENCY_COMP_D       = 0,         // Latency of FP64 comp. ops
  parameter LATENCY_COMP_Xf16    = 0,         // Latency of FP16 comp. ops
  parameter LATENCY_COMP_Xf16alt = 0,         // Latency of FP16alt comp. ops
  parameter LATENCY_COMP_Xf8     = 0,         // Latency of FP8 comp. ops
  parameter LATENCY_DIVSQRT      = 0,         // Latency of div/sqrt. postprocessing
  parameter LATENCY_NONCOMP      = 0,         // Latency of non-comp. ops
  parameter LATENCY_CONV         = 0,         // Latency of conversion ops
  //
  parameter ENFORCE_INPUT_NANBOX = 1          // Enforce input NaN-boxing
) (
  input                  clk_i,
  input                  rst_ni,
  //
  input [WIDTH-1:0]      a_i,
  input [WIDTH-1:0]      b_i,
  input [WIDTH-1:0]      c_i,
  input [2:0]            round_mode_i,
  input [3:0]            op_i,
  input                  op_mod_i,
  input                  vectorial_op_i,
  input [2:0]            fp_fmt_i,
  input [2:0]            fp_fmt2_i,
  input [1:0]            int_fmt_i,
  input [TAG_WIDTH-1:0]  tag_i,
  input [6:0]            prec_ctl_i,
  //
  input                  in_vld_i,
  output                 in_rdy_o,
  input                  flush_i,
  //
  output [WIDTH-1:0]     z_o,
  output [4:0]           status_o,
  output [TAG_WIDTH-1:0] tag_o,
  //
  output                 out_vld_o,
  input                  out_rdy_i
);

  fpnew_top #(
  ) i_fpu (
    .Clk_CI                  ( clk_i                ),
    .Reset_RBI               ( rst_ni               ),
    .A_DI                    ( a_i                  ),
    .B_DI                    ( b_i                  ),
    .C_DI                    ( c_i                  ),
    .RoundMode_SI            ( round_mode_i         ),
    .Op_SI                   ( op_i                 ),
    .OpMod_SI                ( op_mod_i             ),
    .VectorialOp_S           ( vectorial_op_i       ),
    .FpFmt_SI                ( fp_fmt_i             ),
    .FpFmt2_SI               ( fp_fmt2_i            ),
    .IntFmt_SI               ( int_fmt_i            ),
    .Tag_DI                  ( tag_i                ),
    .PrecCtl_SI              ( prec_ctl_i           ),
    .InValid_SI              ( in_vld_i             ),
    .InReady_SO              ( in_rdy_o             ),
    .Flush_SI                ( flush_i              ),
    .Z_DO                    ( z_o                  ),
    .Status_DO               ( status_o             ),
    .Tag_DO                  ( tag_o                ),
    .OutValid_SO             ( out_vld_o            ),
    .OutReady_SI             ( out_rdy_i            )
  );

endmodule // fpnew_top_wrap