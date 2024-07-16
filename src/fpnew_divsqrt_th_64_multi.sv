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

// Authors: Stefan Mach <smach@iis.ee.ethz.ch>
//          Roman Marquart <maroman@student.ethz.ch>
//          Maurus Item <itemm@student.ethz.ch>

`include "common_cells/registers.svh"

module fpnew_divsqrt_th_64_multi #(
  parameter fpnew_pkg::fmt_logic_t   FpFmtConfig  = '1,
  // FPU configuration
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::AFTER,
  // Do not change
  localparam int unsigned WIDTH       = fpnew_pkg::max_fp_width(FpFmtConfig),
  localparam int unsigned NUM_FORMATS = fpnew_pkg::NUM_FP_FORMATS
) (
  input  logic                        clk_i,
  input  logic                        rst_ni,
  // Input signals
  input  logic [1:0][WIDTH-1:0]       operands_i, // 2 operands
  input  logic [NUM_FORMATS-1:0][1:0] is_boxed_i, // 2 operands
  input  fpnew_pkg::roundmode_e       rnd_mode_i,
  input  fpnew_pkg::operation_e       op_i,
  input  fpnew_pkg::fp_format_e       dst_fmt_i,
  input  logic                        mask_i,
  // Output signals
  output logic [WIDTH-1:0]            result_o,
  output fpnew_pkg::status_t          status_o,
  output logic                        extension_bit_o,
  output logic                        mask_o,
  // External Register Control
  input  logic                        flush_i,
  input logic[NumPipeRegs-1:0]        reg_enable_i,
  input logic                         fsm_start_i,
  input logic                         fsm_kill_i,
  output logic                        fsm_ready_o
);

  // ----------
  // Constants
  // ----------
  // Pipelines
  localparam NUM_INP_REGS = (PipeConfig == fpnew_pkg::BEFORE)
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? (NumPipeRegs / 2) // Last to get distributed regs
                               : 0); // no regs here otherwise
  localparam NUM_OUT_REGS = (PipeConfig == fpnew_pkg::AFTER || PipeConfig == fpnew_pkg::INSIDE)
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? ((NumPipeRegs + 1) / 2) // First to get distributed regs
                               : 0); // no regs here otherwise

  // ---------------
  // Input pipeline
  // ---------------
  // Selected pipeline output signals as non-arrays
  logic [1:0][WIDTH-1:0] operands_q;
  fpnew_pkg::roundmode_e rnd_mode_q;
  fpnew_pkg::operation_e op_q;
  fpnew_pkg::fp_format_e dst_fmt_q;

  // Input pipeline signals, index i holds signal after i register stages
  logic                  [0:NUM_INP_REGS][1:0][WIDTH-1:0]       inp_pipe_operands_q;
  fpnew_pkg::roundmode_e [0:NUM_INP_REGS]                       inp_pipe_rnd_mode_q /*verilator split_var */;
  fpnew_pkg::operation_e [0:NUM_INP_REGS]                       inp_pipe_op_q;
  fpnew_pkg::fp_format_e [0:NUM_INP_REGS]                       inp_pipe_dst_fmt_q;
  logic                  [0:NUM_INP_REGS]                       inp_pipe_mask_q;

  // Input stage: First element of pipeline is taken from inputs
  assign inp_pipe_operands_q[0] = operands_i;
  assign inp_pipe_rnd_mode_q[0] = rnd_mode_i;
  assign inp_pipe_op_q[0]       = op_i;
  assign inp_pipe_dst_fmt_q[0]  = dst_fmt_i;
  assign inp_pipe_mask_q[0]     = mask_i;

  // Generate the register stages
  for (genvar i = 0; i < NUM_INP_REGS; i++) begin : gen_input_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Enable register is set externally
    assign reg_ena = reg_enable_i[i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(inp_pipe_operands_q[i+1], inp_pipe_operands_q[i], reg_ena, '0)
    `FFL(inp_pipe_rnd_mode_q[i+1], inp_pipe_rnd_mode_q[i], reg_ena, fpnew_pkg::RNE)
    `FFL(inp_pipe_op_q[i+1],       inp_pipe_op_q[i],       reg_ena, fpnew_pkg::FMADD)
    `FFL(inp_pipe_dst_fmt_q[i+1],  inp_pipe_dst_fmt_q[i],  reg_ena, fpnew_pkg::fp_format_e'(0))
    `FFL(inp_pipe_mask_q[i+1],     inp_pipe_mask_q[i],     reg_ena, '0)
  end
  // Output stage: assign selected pipe outputs to signals for later use
  assign operands_q = inp_pipe_operands_q[NUM_INP_REGS];
  assign rnd_mode_q = inp_pipe_rnd_mode_q[NUM_INP_REGS];
  assign op_q       = inp_pipe_op_q[NUM_INP_REGS];
  assign dst_fmt_q  = inp_pipe_dst_fmt_q[NUM_INP_REGS];

  // -----------------
  // Input processing
  // -----------------
  logic [3:0] divsqrt_fmt;

  // Translate fpnew formats into divsqrt formats
  if(WIDTH == 64) begin : translate_fmt_64_bits
    always_comb begin : translate_fmt
      unique case (dst_fmt_q)
        fpnew_pkg::FP64:    divsqrt_fmt = 4'b1000;
        fpnew_pkg::FP32:    divsqrt_fmt = 4'b0100;
        fpnew_pkg::FP16:    divsqrt_fmt = 4'b0010;
        fpnew_pkg::FP16ALT: divsqrt_fmt = 4'b0001;
        default:            divsqrt_fmt = 4'b1000; // 64 bit max width
      endcase
    end
  end else if(WIDTH == 32) begin : translate_fmt_32_bits
    always_comb begin : translate_fmt
      unique case (dst_fmt_q)
        fpnew_pkg::FP32:    divsqrt_fmt = 4'b0100;
        fpnew_pkg::FP16:    divsqrt_fmt = 4'b0010;
        fpnew_pkg::FP16ALT: divsqrt_fmt = 4'b0001;
        default:            divsqrt_fmt = 4'b0100; // 32 bit max width
      endcase
    end
  end else if(WIDTH == 16) begin : translate_fmt_16_bits
    always_comb begin : translate_fmt
      unique case (dst_fmt_q)
        fpnew_pkg::FP16:    divsqrt_fmt = 4'b0010;
        fpnew_pkg::FP16ALT: divsqrt_fmt = 4'b0001;
        default:            divsqrt_fmt = 4'b0010; // 16 bit max width
      endcase
    end
  end else begin
    $fatal(1, "DivSqrt THMULTI: Unsupported WIDTH (the supported width are 64, 32, 16)");
  end

  logic div_valid, sqrt_valid;  // input signalling with unit

  // Valids are gated by the FSM ready. Invalid input ops run a sqrt to not lose illegal instr.
  assign div_valid   = (op_q == fpnew_pkg::DIV) & fsm_start_i;
  assign sqrt_valid  = (op_q != fpnew_pkg::DIV) & fsm_start_i;

  // -----------------
  // DIVSQRT instance
  // -----------------
  logic unit_done;  // Unit output is valid and should be saved

  logic [63:0]        unit_result;
  fpnew_pkg::status_t unit_status;
  
  logic vfdsu_dp_fdiv_busy;
  
  // Regs to save current instruction
  fpnew_pkg::roundmode_e rm_q;
  logic[3:0] divsqrt_fmt_q;
  fpnew_pkg::operation_e divsqrt_op_q;
  logic div_op, sqrt_op;
  logic [WIDTH-1:0] srcf0_q, srcf1_q;
  logic [63:0] srcf0, srcf1;
  
  // Save operands in regs, C910 saves all the following information in its regs in the next cycle.
  `FFL(rm_q, rnd_mode_q, fsm_start_i, fpnew_pkg::RNE)
  `FFL(divsqrt_fmt_q, divsqrt_fmt, fsm_start_i, '0)
  `FFL(divsqrt_op_q, op_q, fsm_start_i, fpnew_pkg::DIV)
  `FFL(srcf0_q, operands_q[0], fsm_start_i, '0)
  `FFL(srcf1_q, operands_q[1], fsm_start_i, '0)

  // NaN-box inputs with max WIDTH
  if(WIDTH == 64) begin : gen_fmt_64_bits
    always_comb begin : NaN_box_inputs
      if(divsqrt_fmt_q == 4'b1000) begin // 64-bit
        srcf0[63:0] = srcf0_q[63:0];
        srcf1[63:0] = srcf1_q[63:0];
      end else if(divsqrt_fmt_q == 4'b0100) begin // 32-bit
        srcf0[63:32] = '1;
        srcf1[63:32] = '1;
        srcf0[31:0] = srcf0_q[31:0];
        srcf1[31:0] = srcf1_q[31:0];
      end else if((divsqrt_fmt_q == 4'b0010) || (divsqrt_fmt_q == 4'b0001)) begin //16-bit
        srcf0[63:16] = '1;
        srcf1[63:16] = '1;
        srcf0[15:0] = srcf0_q[15:0];
        srcf1[15:0] = srcf1_q[15:0];
      end else begin // Unsupported
        srcf0[63:0] = '1;
        srcf1[63:0] = '1;
      end
    end
  end else if (WIDTH == 32) begin : gen_fmt_32_bits
    always_comb begin : NaN_box_inputs
      if(divsqrt_fmt_q == 4'b0100) begin // 32-bit
        srcf0[63:32] = '1;
        srcf1[63:32] = '1;
        srcf0[31:0] = srcf0_q[31:0];
        srcf1[31:0] = srcf1_q[31:0];
      end else if((divsqrt_fmt_q == 4'b0010) || (divsqrt_fmt_q == 4'b0001)) begin // 16-bit
        srcf0[63:16] = '1;
        srcf1[63:16] = '1;
        srcf0[15:0] = srcf0_q[15:0];
        srcf1[15:0] = srcf1_q[15:0];
      end else begin // Unsupported
        srcf0[63:0] = '1;
        srcf1[63:0] = '1;
      end
    end
  end else if (WIDTH == 16) begin : gen_fmt_16_bits
    always_comb begin : NaN_box_inputs
      if((divsqrt_fmt_q == 4'b0010) || (divsqrt_fmt_q == 4'b0001)) begin // 16-bit
        srcf0[63:16] = '1;
        srcf1[63:16] = '1;
        srcf0[15:0] = srcf0_q[15:0];
        srcf1[15:0] = srcf1_q[15:0];
      end else begin // Unsupported
        srcf0[63:0] = '1;
        srcf1[63:0] = '1;
      end
    end
  end else begin
    $fatal(1, "DivSqrt THMULTI: Unsupported WIDTH (the supported width are 64, 32, 16)");
  end

  assign div_op = (divsqrt_op_q == fpnew_pkg::DIV) ? 1'b1 : 1'b0;
  assign sqrt_op = (divsqrt_op_q != fpnew_pkg::DIV) ? 1'b1 : 1'b0;
  
  // Select func 1 cycle after div issue
  logic func_sel;
  `FFLARNC(func_sel, 1'b1, fsm_start_i, func_sel, 1'b0, clk_i, rst_ni)

  // Select operands 2 cycles after div issue
  logic op_sel;
  `FFLARNC(op_sel, 1'b1, func_sel, op_sel, 1'b0, clk_i, rst_ni)

  ct_vfdsu_top i_ct_vfdsu_top (
    .cp0_vfpu_icg_en                ( 1'b0                      ), // Internal clock gating, (module enable) doesn't matter when the clk_gate module is redundant anyway
    .cp0_yy_clk_en                  ( 1'b1                      ), // Global clock enable (same as above)
    .cpurst_b                       ( rst_ni                    ), // Reset
    .dp_vfdsu_ex1_pipex_dst_ereg    ( '0                        ), // Don't care, used in C910
    .dp_vfdsu_ex1_pipex_dst_vreg    ( '0                        ), // Don't care, used in C910
    .dp_vfdsu_ex1_pipex_iid         ( '0                        ), // Don't care, used in C910
    .dp_vfdsu_ex1_pipex_imm0        ( 3'b111                    ), // Round mode, set to 3'b111 to select vfpu_yy_xx_rm signal
    .dp_vfdsu_ex1_pipex_sel         ( op_sel                    ), // 3. Select operands, start operation
    .dp_vfdsu_ex1_pipex_srcf0       ( srcf0                     ), // Input for operand 0
    .dp_vfdsu_ex1_pipex_srcf1       ( srcf1                     ), // Input for operand 1
    .dp_vfdsu_fdiv_gateclk_issue    ( 1'b1                      ), // Local clock enable (same as above)
    .dp_vfdsu_idu_fdiv_issue        ( fsm_start_i               ), // 1. Issue fdiv (FSM in ctrl)
    .forever_cpuclk                 ( clk_i                     ), // Clock input
    .idu_vfpu_rf_pipex_func         ( {3'b0, divsqrt_fmt_q, 11'b0 ,sqrt_op, div_op} ), // Defines format (bits 16,15) and operation (bits 1,0)
    .idu_vfpu_rf_pipex_gateclk_sel  ( func_sel                  ), // 2. Select func
    .pad_yy_icg_scan_en             ( 1'b0                      ), // SE signal for the redundant clock gating module
    .rtu_yy_xx_flush                ( flush_i | fsm_kill_i      ), // Flush
    .vfpu_yy_xx_dqnan               ( 1'b0                      ), // Disable qNaN, set to 1 if sNaN is used
    .vfpu_yy_xx_rm                  ( rm_q                      ), // Round mode. redundant if imm0 set to the same
    .pipex_dp_vfdsu_ereg            (                           ), // Don't care, used by C910
    .pipex_dp_vfdsu_ereg_data       ( unit_status               ), // Output: status flags
    .pipex_dp_vfdsu_freg_data       ( unit_result               ), // Output: result
    .pipex_dp_vfdsu_inst_vld        ( unit_done                 ), // The result is valid
    .pipex_dp_vfdsu_vreg            (                           ), // Don't care, used by C910
    .vfdsu_dp_fdiv_busy             ( vfdsu_dp_fdiv_busy        ), // Unit is busy, data in flight
    .vfdsu_dp_inst_wb_req           (                           ), // Don't care, used by C910
    .vfdsu_ifu_debug_ex2_wait       (                           ), // Debug output
    .vfdsu_ifu_debug_idle           (                           ), // Debug output
    .vfdsu_ifu_debug_pipe_busy      (                           )  // Debug output
  );

  assign fsm_ready_o = !vfdsu_dp_fdiv_busy;

  // ----------------
  // Hold Result
  // ----------------
  logic [63:0]        held_result, out_result;
  fpnew_pkg::status_t held_status, out_status;
  logic               out_mask;

  `FFL(held_result, unit_result, unit_done, '0);
  `FFL(held_status, unit_status, unit_done, '0);
  `FFL(out_mask, inp_pipe_mask_q[NUM_INP_REGS], fsm_start_i, '0); // Mask is stored on start -> Dont need a bypass mux

  assign out_result = unit_done ? unit_result : held_result;
  assign out_status = unit_done ? unit_status : held_status;

  // ----------------
  // Output Pipeline
  // ----------------
  // Output pipeline signals, index i holds signal after i register stages
  logic               [0:NUM_OUT_REGS][WIDTH-1:0] out_pipe_result_q;
  fpnew_pkg::status_t [0:NUM_OUT_REGS]            out_pipe_status_q;
  logic               [0:NUM_OUT_REGS]            out_pipe_mask_q;

  // Input stage: First element of pipeline is taken from inputs
  assign out_pipe_result_q[0] = out_result;
  assign out_pipe_status_q[0] = out_status;
  assign out_pipe_mask_q[0]   = out_mask;

  // Generate the register stages
  for (genvar i = 0; i < NUM_OUT_REGS; i++) begin : gen_output_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Enable register is set externally
    assign reg_ena = reg_enable_i[NUM_INP_REGS + i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(out_pipe_result_q[i+1], out_pipe_result_q[i], reg_ena, '0)
    `FFL(out_pipe_status_q[i+1], out_pipe_status_q[i], reg_ena, '0)
    `FFL(out_pipe_mask_q[i+1],   out_pipe_mask_q[i],   reg_ena, '0)
  end

  // Output stage: assign module outputs
  assign result_o        = out_pipe_result_q[NUM_OUT_REGS];
  assign status_o        = out_pipe_status_q[NUM_OUT_REGS];
  assign extension_bit_o = 1'b1; // always NaN-Box result
  assign mask_o          = out_pipe_mask_q[NUM_OUT_REGS];

endmodule
