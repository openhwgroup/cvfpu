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

// Author: Stefan Mach <smach@iis.ee.ethz.ch>

// Generate pipeline stages as given by NumPipeRegs. When NumPipeRegs is 0, no registers are
// generated.
//            +---------|---------|---------|----------|-------------------+
//            |  _d[0]  |  _d[1]  |  _d[2]  |  _d[..]  |  _d[NumPipeRegs]  |
//            |         |  _q[0]  |  _q[1]  | _q[..-1] | _q[NumPipeRegs-1] |
// inputs_i   >=========|=========|=========|====~~====|===================> inputs_o
// in_valid_i >---------|---------|---------|----~~----|-------------------> out_valid_o
// in_ready_o <---------+---------+---------+----~~----+-------------------< out_ready_i
//            |         |         |         |          |                   |
// stage #    +----0----|----1----|----2----|----..----|----NumPipeRegs----+
//
// NOTE: These registers must be retimed in synthesis for sensible pipelining. Make sure to
// optimize registers through the instantiating hierarchy.
// The ready signal is not a direct feed-through from destination to source but takes into account
// intermediate 'bubbles' in the pipeline. As such, downstream stalls can be hidden when the
// pipeline is not full.
// Enable signals on the registers will lead to clock-gated pipeline stages when this optimization
// is enabled during synthesis. Make sure to optimize clock gates through hierarchies.

// Author: Stefan Mach <smach@iis.ee.ethz.ch>

`include "registers.svh"

module fpnew_pipe_in #(
  parameter int unsigned Width         = 32,
  parameter int unsigned NumPipeRegs   = 0,
  parameter int unsigned NumOperands   = 3,
  parameter int unsigned NumFormats    = 1,
  parameter type         TagType       = logic,
  parameter type         AuxType       = logic
) (
  input  logic                                   clk_i,
  input  logic                                   rst_ni,
  // Input signals
  input  logic [NumOperands-1:0][Width-1:0]      operands_i,
  input  logic [NumFormats-1:0][NumOperands-1:0] is_boxed_i,
  input  fpnew_pkg::roundmode_e                  rnd_mode_i,
  input  fpnew_pkg::operation_e                  op_i,
  input  logic                                   op_mod_i,
  input  fpnew_pkg::fp_format_e                  src_fmt_i,
  input  fpnew_pkg::fp_format_e                  dst_fmt_i,
  input  fpnew_pkg::int_format_e                 int_fmt_i,
  input  TagType                                 tag_i,
  input  AuxType                                 aux_i,
  // Input Handshake
  input  logic                                   in_valid_i,
  output logic                                   in_ready_o,
  input  logic                                   flush_i,
  // Output signals
  output logic [NumOperands-1:0][Width-1:0]      operands_o,
  output logic [NumFormats-1:0][NumOperands-1:0] is_boxed_o,
  output fpnew_pkg::roundmode_e                  rnd_mode_o,
  output fpnew_pkg::operation_e                  op_o,
  output logic                                   op_mod_o,
  output fpnew_pkg::fp_format_e                  src_fmt_o,
  output fpnew_pkg::fp_format_e                  dst_fmt_o,
  output fpnew_pkg::int_format_e                 int_fmt_o,
  output TagType                                 tag_o,
  output AuxType                                 aux_o,
  // Output Handshake
  output logic                                   out_valid_o,
  input  logic                                   out_ready_i,
  // Status signal
  output logic                                   busy_o
);

  // Input signals for the next stage (= output signals of the previous stage)
  logic                   [0:NumPipeRegs][NumOperands-1:0][Width-1:0]      operands_d;
  logic                   [0:NumPipeRegs][NumFormats-1:0][NumOperands-1:0] is_boxed_d;
  fpnew_pkg::roundmode_e  [0:NumPipeRegs]                                  rnd_mode_d;
  fpnew_pkg::operation_e  [0:NumPipeRegs]                                  op_d;
  logic                   [0:NumPipeRegs]                                  op_mod_d;
  fpnew_pkg::fp_format_e  [0:NumPipeRegs]                                  src_fmt_d;
  fpnew_pkg::fp_format_e  [0:NumPipeRegs]                                  dst_fmt_d;
  fpnew_pkg::int_format_e [0:NumPipeRegs]                                  int_fmt_d;
  TagType                 [0:NumPipeRegs]                                  tag_d;
  AuxType                 [0:NumPipeRegs]                                  aux_d;
  logic                   [0:NumPipeRegs]                                  valid_d;
  // Ready signal is combinatorial for all stages
  logic                   [0:NumPipeRegs]                                  stage_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign operands_d[0] = operands_i;
  assign is_boxed_d[0] = is_boxed_i;
  assign rnd_mode_d[0] = rnd_mode_i;
  assign op_d[0]       = op_i;
  assign op_mod_d[0]   = op_mod_i;
  assign src_fmt_d[0]  = src_fmt_i;
  assign dst_fmt_d[0]  = dst_fmt_i;
  assign int_fmt_d[0]  = int_fmt_i;
  assign tag_d[0]      = tag_i;
  assign aux_d[0]      = aux_i;
  assign valid_d[0]    = in_valid_i;

  // Input stage: Propagate pipeline ready signal
  assign in_ready_o = stage_ready[0];

  // Generate the pipeline stages in case they are needed
  if (NumPipeRegs > 0) begin : gen_pipeline
    // Pipelined versions of signals for later stages
    logic                   [0:NumPipeRegs][NumOperands-1:0][Width-1:0]      operands_q;
    logic                   [0:NumPipeRegs][NumFormats-1:0][NumOperands-1:0] is_boxed_q;
    fpnew_pkg::roundmode_e  [0:NumPipeRegs]                                  rnd_mode_q;
    fpnew_pkg::operation_e  [0:NumPipeRegs]                                  op_q;
    logic                   [0:NumPipeRegs]                                  op_mod_q;
    fpnew_pkg::fp_format_e  [0:NumPipeRegs]                                  src_fmt_q;
    fpnew_pkg::fp_format_e  [0:NumPipeRegs]                                  dst_fmt_q;
    fpnew_pkg::int_format_e [0:NumPipeRegs]                                  int_fmt_q;
    TagType                 [0:NumPipeRegs]                                  tag_q;
    AuxType                 [0:NumPipeRegs]                                  aux_q;
    logic                   [0:NumPipeRegs]                                  valid_q;

    for (genvar i = 0; i < int'(NumPipeRegs); i++) begin : pipeline_stages
      // Internal register enable for this stage
      logic reg_ena;

      // Next state from previous register to form a shift register
      assign operands_d[i+1] = operands_q[i];
      assign is_boxed_d[i+1] = is_boxed_q[i];
      assign rnd_mode_d[i+1] = rnd_mode_q[i];
      assign op_d[i+1]       = op_q[i];
      assign op_mod_d[i+1]   = op_mod_q[i];
      assign src_fmt_d[i+1]  = src_fmt_q[i];
      assign dst_fmt_d[i+1]  = dst_fmt_q[i];
      assign int_fmt_d[i+1]  = int_fmt_q[i];
      assign tag_d[i+1]      = tag_q[i];
      assign aux_d[i+1]      = aux_q[i];
      assign valid_d[i+1]    = valid_q[i];

      // Determine the ready signal of the current stage - advance the pipeline:
      // 1. if the next stage is ready for our data
      // 2. if the next stage only holds a bubble (not valid) -> we can pop it
      assign stage_ready[i] = stage_ready[i+1] | ~valid_q[i];

      // Valid: enabled by ready signal, synchronous clear with the flush signal
      `FFLARNC(valid_q[i], valid_d[i], stage_ready[i], flush_i, 1'b0, clk_i, rst_ni)

      // Enable register if pipleine ready and a valid data item is present
      assign reg_ena = stage_ready[i] & valid_d[i];

      // Generate the pipeline registers within the stages, use enable-registers
      `FFL(operands_q[i], operands_d[i], reg_ena, '0)
      `FFL(is_boxed_q[i], is_boxed_d[i], reg_ena, '0)
      `FFL(rnd_mode_q[i], rnd_mode_d[i], reg_ena, fpnew_pkg::RNE)
      `FFL(op_q[i],       op_d[i],       reg_ena, fpnew_pkg::FMADD)
      `FFL(op_mod_q[i],   op_mod_d[i],   reg_ena, '0)
      `FFL(src_fmt_q[i],  src_fmt_d[i],  reg_ena, fpnew_pkg::FP32)
      `FFL(dst_fmt_q[i],  dst_fmt_d[i],  reg_ena, fpnew_pkg::FP32)
      `FFL(int_fmt_q[i],  int_fmt_d[i],  reg_ena, fpnew_pkg::INT8)
      `FFL(tag_q[i],      tag_d[i],      reg_ena, '0)
      `FFL(aux_q[i],      aux_d[i],      reg_ena, '0)
    end
  end

  // Output stage: bind last stage outputs to module output. Directly connects to input if no regs.
  assign operands_o  = operands_d[NumPipeRegs];
  assign is_boxed_o  = is_boxed_d[NumPipeRegs];
  assign rnd_mode_o  = rnd_mode_d[NumPipeRegs];
  assign op_o        = op_d[NumPipeRegs];
  assign op_mod_o    = op_mod_d[NumPipeRegs];
  assign dst_fmt_o   = dst_fmt_d[NumPipeRegs];
  assign src_fmt_o   = src_fmt_d[NumPipeRegs];
  assign int_fmt_o   = int_fmt_d[NumPipeRegs];
  assign tag_o       = tag_d[NumPipeRegs];
  assign aux_o       = aux_d[NumPipeRegs];
  assign out_valid_o = valid_d[NumPipeRegs];

  // Output stage: Ready travels backwards from output side
  assign stage_ready[NumPipeRegs] = out_ready_i;

  // The pipeline is considered busy if any valid data is in flight
  assign busy_o = (| valid_d);

endmodule
