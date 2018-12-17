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

`include "register_defines.svh"

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
  input  logic [0:NumOperands-1][Width-1:0]      operands_i,
  input  logic [0:NumFormats-1][0:NumOperands-1] is_boxed_i,
  input  fpnew_pkg::roundmode_e                  rnd_mode_i,
  input  fpnew_pkg::operation_e                  op_i,
  input  logic                                   op_mod_i,
  input  fpnew_pkg::fp_format_e                  fp_fmt_i,
  input  fpnew_pkg::fp_format_e                  fp_fmt2_i,
  input  fpnew_pkg::int_format_e                 int_fmt_i,
  input  TagType                                 tag_i,
  input  AuxType                                 aux_i,
  // Input Handshake
  input  logic                                   in_valid_i,
  output logic                                   in_ready_o,
  input  logic                                   flush_i,
  // Output signals
  output logic [0:NumOperands-1][Width-1:0]      operands_o,
  output logic [0:NumFormats-1][0:NumOperands-1] is_boxed_o,
  output fpnew_pkg::roundmode_e                  rnd_mode_o,
  output fpnew_pkg::operation_e                  op_o,
  output logic                                   op_mod_o,
  output fpnew_pkg::fp_format_e                  fp_fmt_o,
  output fpnew_pkg::fp_format_e                  fp_fmt2_o,
  output fpnew_pkg::int_format_e                 int_fmt_o,
  output TagType                                 tag_o,
  output AuxType                                 aux_o,
  // Output Handshake
  output logic                                   out_valid_o,
  input  logic                                   out_ready_i,
  // Status signal
  output logic                                   busy_o
);

  // Pipelined versions of signals for each stage (one more than number of registers)
  logic [0:NumPipeRegs][0:NumOperands-1][Width-1:0]      operands_q;
  logic [0:NumPipeRegs][0:NumFormats-1][0:NumOperands-1] is_boxed_q;
  fpnew_pkg::roundmode_e [0:NumPipeRegs]                 rnd_mode_q;
  fpnew_pkg::operation_e [0:NumPipeRegs]                 op_q;
  logic [0:NumPipeRegs]                                  op_mod_q;
  fpnew_pkg::fp_format_e [0:NumPipeRegs]                 fp_fmt_q;
  fpnew_pkg::fp_format_e [0:NumPipeRegs]                 fp_fmt2_q;
  fpnew_pkg::int_format_e [0:NumPipeRegs]                int_fmt_q;
  TagType [0:NumPipeRegs]                                tag_q;
  AuxType [0:NumPipeRegs]                                aux_q;
  logic [0:NumPipeRegs]                                  valid_q;
  logic [0:NumPipeRegs]                                  ready_q;

  // Input stage: First element of pipeline is taken from inputs
  assign operands_q[0] = operands_i;
  assign is_boxed_q[0] = is_boxed_i;
  assign rnd_mode_q[0] = rnd_mode_i;
  assign op_q[0]       = op_i;
  assign op_mod_q[0]   = op_mod_i;
  assign fp_fmt_q[0]   = fp_fmt_i;
  assign fp_fmt2_q[0]  = fp_fmt2_i;
  assign int_fmt_q[0]  = int_fmt_i;
  assign tag_q[0]      = tag_i;
  assign aux_q[0]      = aux_i;
  assign valid_q[0]    = in_valid_i;

  // Input stage: Propagate pipeline ready signal
  assign in_ready_o = ready_q[0];

  // Generate pipeline stages as given by NumPipeRegs. When NumPipeRegs is 0, no registers are
  // generated.
  //            +---------|---------|---------|----------|-------------+
  //            |         |         |         |          |             |
  // inputs_i   >=========|=========|=========|====~~====|=============> inputs_o
  // in_valid_i >---------|---------|---------|----~~----|-------------> out_valid_o
  // in_ready_o <---------|---------|---------|----~~----|-------------< out_ready_i
  //            |         |         |         |          |             |
  // stage #    +----0----|----1----|----2----|----..----|-NumPipeRegs-+
  //
  // NOTE: These registers must be retimed in synthesis for sensible pipelining. Make sure to
  // optimize registers through the instantiating hierarchy.
  // The ready signal is not a direct feed-through from destination to source but takes into account
  // intermediate 'bubbles' in the pipeline. As such, downstream stalls can be hidden when the
  // pipeline is not full.
  // Enable signals on the registers will lead to clock-gated pipeline stages when this optimization
  // is enabled during synthesis. Make sure to optimize clock gates through hierarchies.
  for (genvar i = 0; i < int'(NumPipeRegs); i++) begin : pipeline_stages
    // Internal register enable for this stage
    logic reg_ena;

    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign ready_q[i] = ready_q[i+1] | ~valid_q[i+1];

    // Valid: enabled by ready signal, synchronous clear with the flush signal
    `FFLARNC(valid_q[i+1], valid_q[i], ready_q[i], flush_i, 1'b0, clk_i, rst_ni)

    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = ready_q[i] & valid_q[i];

    // Generate the pipeline registers within the stages, use enable-registers wihtout reset
    `FFLNR(operands_q[i+1], operands_q[i], reg_ena, clk_i)
    `FFLNR(is_boxed_q[i+1], is_boxed_q[i], reg_ena, clk_i)
    `FFLNR(rnd_mode_q[i+1], rnd_mode_q[i], reg_ena, clk_i)
    `FFLNR(op_q[i+1],       op_q[i],       reg_ena, clk_i)
    `FFLNR(op_mod_q[i+1],   op_mod_q[i],   reg_ena, clk_i)
    `FFLNR(fp_fmt_q[i+1],   fp_fmt_q[i],   reg_ena, clk_i)
    `FFLNR(fp_fmt2_q[i+1],  fp_fmt2_q[i],  reg_ena, clk_i)
    `FFLNR(int_fmt_q[i+1],  int_fmt_q[i],  reg_ena, clk_i)
    `FFLNR(tag_q[i+1],      tag_q[i],      reg_ena, clk_i)
    `FFLNR(aux_q[i+1],      aux_q[i],      reg_ena, clk_i)
  end

  // Output stage: bind pipeline outputs to module output. Directly connects to input if no regs.
  assign operands_o  = operands_q[NumPipeRegs];
  assign is_boxed_o  = is_boxed_q[NumPipeRegs];
  assign rnd_mode_o  = rnd_mode_q[NumPipeRegs];
  assign op_o        = op_q[NumPipeRegs];
  assign op_mod_o    = op_mod_q[NumPipeRegs];
  assign fp_fmt_o    = fp_fmt_q[NumPipeRegs];
  assign fp_fmt2_o   = fp_fmt2_q[NumPipeRegs];
  assign int_fmt_o   = int_fmt_q[NumPipeRegs];
  assign tag_o       = tag_q[NumPipeRegs];
  assign aux_o       = aux_q[NumPipeRegs];
  assign out_valid_o = valid_q[NumPipeRegs];

  // Output stage: Ready travels backwards from output side
  assign ready_q[NumPipeRegs] = out_ready_i;

  // The pipeline is considered busy if any valid data is in flight
  assign busy_o         = (| valid_q);

endmodule
