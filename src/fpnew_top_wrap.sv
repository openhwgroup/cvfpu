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

typedef enum {NONE, PARALLEL, MERGED} unitType_t;

module fpnew_top_wrap #(
  parameter bit RV64             = 1,         // Enables 64-bit integer formats
  parameter bit RVF              = 1,         // Enables FP32 format
  parameter bit RVD              = 1,         // Enables FP64 format
  parameter bit Xf16             = 1,         // Enables FP16 format
  parameter bit Xf16alt          = 1,         // Enables FP16alt format
  parameter bit Xf8              = 1,         // Enables FP8 format
  parameter bit Xfvec            = 1,         // Generates vector for enabled formats
  //
  parameter TYPE_ADDMUL          = PARALLEL,  // NONE:0. PARALLEL:1, MERGED:2
  parameter TYPE_DIVSQRT         = MERGED,    // NONE:0. PARALLEL:1, MERGED:2
  parameter TYPE_NONCOMP         = PARALLEL,  // NONE:0. PARALLEL:1, MERGED:2
  parameter TYPE_CONV            = PARALLEL,  // NONE:0. PARALLEL:1, MERGED:2
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
  parameter bit ENFORCE_INPUT_NANBOX = 1          // Enforce input NaN-boxing
) (
  input logic                   clk_i,
  input logic                   rst_ni,
  //
  input logic [64-1:0]          a_i,
  input logic [64-1:0]          b_i,
  input logic [64-1:0]          c_i,
  input logic [2:0]             round_mode_i,
  input logic [3:0]             op_i,
  input logic                   op_mod_i,
  input logic                   vectorial_op_i,
  input logic [2:0]             fp_fmt_i,
  input logic [2:0]             fp_fmt2_i,
  input logic [1:0]             int_fmt_i,
  input logic [4-1:0]           tag_i,
  input logic [6:0]             prec_ctl_i,
  //
  input  logic                  in_vld_i,
  output logic                  in_rdy_o,
  input  logic                  flush_i,
  //
  output logic [64-1:0]         z_o,
  output logic [4:0]            status_o,
  output logic [4-1:0]          tag_o,
  //
  output logic                  out_vld_o,
  input  logic                  out_rdy_i
);

  // compatibility wrapper for Vivado
  fpnew_top_64bit_compat #(
    .RV64                    ( RV64                 ),
    .RVF                     ( RVF                  ),
    .RVD                     ( RVD                  ),
    .Xf16                    ( Xf16                 ),
    .Xf16alt                 ( Xf16alt              ),
    .Xf8                     ( Xf8                  ),
    .Xfvec                   ( Xfvec                ),
    .TYPE_ADDMUL             ( TYPE_ADDMUL          ),
    .TYPE_DIVSQRT            ( TYPE_DIVSQRT         ),
    .TYPE_NONCOMP            ( TYPE_NONCOMP         ),
    .TYPE_CONV               ( TYPE_CONV            ),
    .LATENCY_COMP_F          ( LATENCY_COMP_F       ),
    .LATENCY_COMP_D          ( LATENCY_COMP_D       ),
    .LATENCY_COMP_Xf16       ( LATENCY_COMP_Xf16    ),
    .LATENCY_COMP_Xf16alt    ( LATENCY_COMP_Xf16alt ),
    .LATENCY_COMP_Xf8        ( LATENCY_COMP_Xf8     ),
    .LATENCY_DIVSQRT         ( LATENCY_DIVSQRT      ),
    .LATENCY_NONCOMP         ( LATENCY_NONCOMP      ),
    .LATENCY_CONV            ( LATENCY_CONV         ),
    .ENFORCE_INPUT_NANBOX    ( ENFORCE_INPUT_NANBOX )
  ) i_fpu (
    .Clk_CI                  ( clk_i                ),
    .Reset_RBI               ( rst_ni               ),
    .A_DI                    ( a_i                  ),
    .B_DI                    ( b_i                  ),
    .C_DI                    ( c_i                  ),
    .RoundMode_SI            ( round_mode_i         ),
    .Op_SI                   ( op_i                 ),
    .OpMod_SI                ( op_mod_i             ),
    .VectorialOp_SI          ( vectorial_op_i       ),
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