// Copyright 2024 ETH Zurich and University of Bologna.
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

`include "common_cells/registers.svh"

module fpnew_divsqrt_8_multi_lut #(
  // FPU configuration
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::AFTER,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,
  // Do not change
  localparam int unsigned NUM_FORMATS = fpnew_pkg::NUM_FP_FORMATS,
  localparam int unsigned ExtRegEnaWidth = NumPipeRegs == 0 ? 1 : NumPipeRegs
) (
  input  logic                        clk_i,
  input  logic                        rst_ni,
  // Input signals
  input  logic [1:0][7:0]             operands_i, // 2 operands
  input  logic [NUM_FORMATS-1:0][1:0] is_boxed_i, // 2 operands
  input  fpnew_pkg::roundmode_e       rnd_mode_i,
  input  fpnew_pkg::operation_e       op_i,
  input  fpnew_pkg::fp_format_e       dst_fmt_i,
  input  TagType                      tag_i,
  input  logic                        mask_i,
  input  AuxType                      aux_i,
  input  logic                        vectorial_op_i,
  // Input Handshake
  input  logic                        in_valid_i,
  output logic                        in_ready_o,
  output logic                        divsqrt_done_o,
  input  logic                        simd_synch_done_i,
  output logic                        divsqrt_ready_o,
  input  logic                        simd_synch_rdy_i,
  input  logic                        flush_i,
  // Output signals
  output logic [7:0]                  result_o,
  output fpnew_pkg::status_t          status_o,
  output logic                        extension_bit_o,
  output TagType                      tag_o,
  output logic                        mask_o,
  output AuxType                      aux_o,
  // Output handshake
  output logic                        out_valid_o,
  input  logic                        out_ready_i,
  // Indication of valid data in flight
  output logic                        busy_o,
  // External register enable override
  input  logic [ExtRegEnaWidth-1:0]   reg_ena_i
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
  logic [1:0][7:0] operands_q;
  fpnew_pkg::roundmode_e rnd_mode_q;
  fpnew_pkg::operation_e op_q;
  fpnew_pkg::fp_format_e dst_fmt_q;

  // Input pipeline signals, index i holds signal after i register stages
  logic                  [0:NUM_INP_REGS][1:0][7:0]       inp_pipe_operands_q;
  fpnew_pkg::roundmode_e [0:NUM_INP_REGS]                 inp_pipe_rnd_mode_q;
  fpnew_pkg::operation_e [0:NUM_INP_REGS]                 inp_pipe_op_q;
  fpnew_pkg::fp_format_e [0:NUM_INP_REGS]                 inp_pipe_dst_fmt_q;
  TagType                [0:NUM_INP_REGS]                 inp_pipe_tag_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_mask_q;
  AuxType                [0:NUM_INP_REGS]                 inp_pipe_aux_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_vec_op_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_valid_q;
  // Ready signal is combinatorial for all stages
  logic [0:NUM_INP_REGS] inp_pipe_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign inp_pipe_operands_q[0] = operands_i;
  assign inp_pipe_rnd_mode_q[0] = rnd_mode_i;
  assign inp_pipe_op_q[0]       = op_i;
  assign inp_pipe_dst_fmt_q[0]  = dst_fmt_i;
  assign inp_pipe_tag_q[0]      = tag_i;
  assign inp_pipe_mask_q[0]     = mask_i;
  assign inp_pipe_aux_q[0]      = aux_i;
  assign inp_pipe_vec_op_q[0]   = vectorial_op_i;
  assign inp_pipe_valid_q[0]    = in_valid_i;
  // Input stage: Propagate pipeline ready signal to upstream circuitry
  assign in_ready_o = inp_pipe_ready[0];
  // Generate the register stages
  for (genvar i = 0; i < NUM_INP_REGS; i++) begin : gen_input_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign inp_pipe_ready[i] = inp_pipe_ready[i+1] | ~inp_pipe_valid_q[i+1];
    // Valid: enabled by ready signal, synchronous clear with the flush signal
    `FFLARNC(inp_pipe_valid_q[i+1], inp_pipe_valid_q[i], inp_pipe_ready[i], flush_i, 1'b0, clk_i, rst_ni)
    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = (inp_pipe_ready[i] & inp_pipe_valid_q[i]) | reg_ena_i[i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(inp_pipe_operands_q[i+1], inp_pipe_operands_q[i], reg_ena, '0)
    `FFL(inp_pipe_rnd_mode_q[i+1], inp_pipe_rnd_mode_q[i], reg_ena, fpnew_pkg::RNE)
    `FFL(inp_pipe_op_q[i+1],       inp_pipe_op_q[i],       reg_ena, fpnew_pkg::FMADD)
    `FFL(inp_pipe_dst_fmt_q[i+1],  inp_pipe_dst_fmt_q[i],  reg_ena, fpnew_pkg::fp_format_e'(0))
    `FFL(inp_pipe_tag_q[i+1],      inp_pipe_tag_q[i],      reg_ena, TagType'('0))
    `FFL(inp_pipe_mask_q[i+1],     inp_pipe_mask_q[i],     reg_ena, '0)
    `FFL(inp_pipe_aux_q[i+1],      inp_pipe_aux_q[i],      reg_ena, AuxType'('0))
    `FFL(inp_pipe_vec_op_q[i+1],   inp_pipe_vec_op_q[i],   reg_ena, AuxType'('0))
  end
  // Output stage: assign selected pipe outputs to signals for later use
  assign operands_q = inp_pipe_operands_q[NUM_INP_REGS];
  assign rnd_mode_q = inp_pipe_rnd_mode_q[NUM_INP_REGS];
  assign op_q       = inp_pipe_op_q[NUM_INP_REGS];
  assign dst_fmt_q  = inp_pipe_dst_fmt_q[NUM_INP_REGS];

  logic div_valid, sqrt_valid;  // input signalling with unit
  logic op_starting;            // high in the cycle a new operation starts

  // Valids are gated by the FSM ready. Invalid input ops run a sqrt to not lose illegal instr.
  assign div_valid   = inp_pipe_valid_q[NUM_INP_REGS] & (op_q == fpnew_pkg::DIV) & ~flush_i;
  assign sqrt_valid  = inp_pipe_valid_q[NUM_INP_REGS] & (op_q != fpnew_pkg::DIV) & ~flush_i;
  assign op_starting = div_valid | sqrt_valid;

  // -----------------
  // DIVSQRT instance
  // -----------------
  logic [1:0][7:0] operands_div8;
  logic [1:0][7:0] operands_div8alt;
  logic      [7:0] operand_sqrt8;
  logic      [7:0] operand_sqrt8alt;

  always_comb begin : silence_inputs_of_unused_units
    operands_div8    = '0;
    operands_div8alt = '0;
    operand_sqrt8    = '0;
    operand_sqrt8alt = '0;
    if (op_starting) begin
      if (div_valid && dst_fmt_q == fpnew_pkg::FP8) begin
        operands_div8    = operands_q;
      end else if (div_valid && dst_fmt_q == fpnew_pkg::FP8ALT) begin
        operands_div8alt = operands_q;
      end else if (sqrt_valid && dst_fmt_q == fpnew_pkg::FP8) begin
        operand_sqrt8    = operands_q[0];
      end else if (sqrt_valid && dst_fmt_q == fpnew_pkg::FP8ALT) begin
        operand_sqrt8alt = operands_q[0];
      end
    end
  end

  logic [7:0] result_div8;
  logic [7:0] result_div8alt;
  logic [7:0] result_sqrt8;
  logic [7:0] result_sqrt8alt;
  logic [4:0] status_div8, status_div8alt, status_sqrt8, status_sqrt8alt;

  fpnew_lut_div8 i_div8_lut (
    .input_i ({operands_div8[0], operands_div8[1]}),
    .out_o (result_div8),
    .status_o (status_div8)
  );

  fpnew_lut_div8alt i_div8alt_lut (
    .input_i ({operands_div8alt[0], operands_div8alt[1]}),
    .out_o (result_div8alt),
    .status_o (status_div8alt)
  );

  fpnew_lut_sqrt8 i_sqrt8_lut (
    .input_i (operand_sqrt8),
    .out_o (result_sqrt8),
    .status_o (status_sqrt8)
  );

  fpnew_lut_sqrt8alt i_sqrt8alt_lut (
    .input_i (operand_sqrt8alt),
    .out_o (result_sqrt8alt),
    .status_o (status_sqrt8alt)
  );

  // --------------
  // Output Select
  // --------------
  logic [7:0]   result_d;
  fpnew_pkg::status_t status_d;

  always_comb begin : select_output
    result_d = '0;
    if (div_valid && dst_fmt_q == fpnew_pkg::FP8) begin
      result_d = result_div8;
      status_d = status_div8;
    end else if (div_valid && dst_fmt_q == fpnew_pkg::FP8ALT) begin
      result_d = result_div8alt;
      status_d = status_div8alt;
    end else if (sqrt_valid && dst_fmt_q == fpnew_pkg::FP8) begin
      result_d = result_sqrt8;
      status_d = status_sqrt8;
    end else if (sqrt_valid && dst_fmt_q == fpnew_pkg::FP8ALT) begin
      result_d = result_sqrt8alt;
      status_d = status_sqrt8alt;
    end
  end

  // ----------------
  // Output Pipeline
  // ----------------
  // Output pipeline signals, index i holds signal after i register stages
  logic               [0:NUM_OUT_REGS][7:0] out_pipe_result_q;
  fpnew_pkg::status_t [0:NUM_OUT_REGS]            out_pipe_status_q;
  TagType             [0:NUM_OUT_REGS]            out_pipe_tag_q;
  logic               [0:NUM_OUT_REGS]            out_pipe_mask_q;
  AuxType             [0:NUM_OUT_REGS]            out_pipe_aux_q;
  logic               [0:NUM_OUT_REGS]            out_pipe_valid_q;
  // Ready signal is combinatorial for all stages
  logic [0:NUM_OUT_REGS] out_pipe_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign out_pipe_result_q[0] = result_d;
  assign out_pipe_status_q[0] = status_d;
  assign out_pipe_tag_q[0]    = inp_pipe_tag_q[NUM_INP_REGS];
  assign out_pipe_mask_q[0]   = inp_pipe_mask_q[NUM_INP_REGS];
  assign out_pipe_aux_q[0]    = inp_pipe_aux_q[NUM_INP_REGS];
  assign out_pipe_valid_q[0]  = inp_pipe_valid_q[NUM_INP_REGS];
  // Input stage: Propagate pipeline ready signal to inside pipe
  assign inp_pipe_ready[NUM_INP_REGS] = out_pipe_ready[0];
  // Generate the register stages
  for (genvar i = 0; i < NUM_OUT_REGS; i++) begin : gen_output_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign out_pipe_ready[i] = out_pipe_ready[i+1] | ~out_pipe_valid_q[i+1];
    // Valid: enabled by ready signal, synchronous clear with the flush signal
    `FFLARNC(out_pipe_valid_q[i+1], out_pipe_valid_q[i], out_pipe_ready[i], flush_i, 1'b0, clk_i, rst_ni)
    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = (out_pipe_ready[i] & out_pipe_valid_q[i]) | reg_ena_i[NUM_INP_REGS + i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(out_pipe_result_q[i+1], out_pipe_result_q[i], reg_ena, '0)
    `FFL(out_pipe_status_q[i+1], out_pipe_status_q[i], reg_ena, '0)
    `FFL(out_pipe_tag_q[i+1],    out_pipe_tag_q[i],    reg_ena, TagType'('0))
    `FFL(out_pipe_mask_q[i+1],   out_pipe_mask_q[i],   reg_ena, '0)
    `FFL(out_pipe_aux_q[i+1],    out_pipe_aux_q[i],    reg_ena, AuxType'('0))
  end
  // Output stage: Ready travels backwards from output side, driven by downstream circuitry
  assign out_pipe_ready[NUM_OUT_REGS] = out_ready_i;
  // Output stage: assign module outputs
  assign result_o        = out_pipe_result_q[NUM_OUT_REGS];
  assign status_o        = out_pipe_status_q[NUM_OUT_REGS];
  assign extension_bit_o = 1'b1; // always NaN-Box result
  assign tag_o           = out_pipe_tag_q[NUM_OUT_REGS];
  assign mask_o          = out_pipe_mask_q[NUM_OUT_REGS];
  assign aux_o           = out_pipe_aux_q[NUM_OUT_REGS];
  assign out_valid_o     = out_pipe_valid_q[NUM_OUT_REGS];
  assign busy_o          = (| {inp_pipe_valid_q, op_starting, out_pipe_valid_q});

  assign divsqrt_done_o  = 1'b1;
  assign divsqrt_ready_o = 1'b1;
endmodule

