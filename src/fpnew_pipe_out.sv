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

`include "registers.svh"

module fpnew_pipe_out #(
  parameter int unsigned Width         = 32,
  parameter int unsigned NumPipeRegs   = 0,
  parameter type         TagType       = logic,
  parameter type         AuxType       = logic
) (
  input  logic               clk_i,
  input  logic               rst_ni,
  // Input signals
  input  logic [Width-1:0]   result_i,
  input  fpnew_pkg::status_t status_i,
  input  logic               extension_bit_i,
  input  TagType             tag_i,
  input  AuxType             aux_i,
  // Input Handshake
  input  logic               in_valid_i,
  output logic               in_ready_o,
  input  logic               flush_i,
  // Output signals
  output logic [Width-1:0]   result_o,
  output fpnew_pkg::status_t status_o,
  output logic               extension_bit_o,
  output TagType             tag_o,
  output AuxType             aux_o,
  // Output Handshake
  output logic               out_valid_o,
  input  logic               out_ready_i,
  // Status signal
  output logic               busy_o
);

  // Pipelined versions of signals for each stage (one more than number of registers)
  logic [0:NumPipeRegs][Width-1:0]    result_q;
  fpnew_pkg::status_t [0:NumPipeRegs] status_q;
  logic [0:NumPipeRegs]               extension_bit_q;
  TagType [0:NumPipeRegs]             tag_q;
  TagType [0:NumPipeRegs]             aux_q;
  logic [0:NumPipeRegs]               valid_q;
  logic [0:NumPipeRegs]               ready_q;

  // Input stage: First element of pipeline is taken from inputs
  assign result_q[0]        = result_i;
  assign status_q[0]        = status_i;
  assign extension_bit_q[0] = extension_bit_i;
  assign tag_q[0]           = tag_i;
  assign aux_q[0]           = aux_i;
  assign valid_q[0]         = in_valid_i;

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
  for (genvar i = 0; i < NumPipeRegs; i++) begin : pipeline_stages
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

    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(result_q[i+1],        result_q[i],        reg_ena, '0)
    `FFL(status_q[i+1],        status_q[i],        reg_ena, '0)
    `FFL(extension_bit_q[i+1], extension_bit_q[i], reg_ena, '0)
    `FFL(tag_q[i+1],           tag_q[i],           reg_ena, '0)
    `FFL(aux_q[i+1],           aux_q[i],           reg_ena, '0)
  end

  // Output stage: bind pipeline outputs to module output. Directly connects to input if no regs.
  assign result_o        = result_q[NumPipeRegs];
  assign status_o        = status_q[NumPipeRegs];
  assign extension_bit_o = extension_bit_q[NumPipeRegs];
  assign tag_o           = tag_q[NumPipeRegs];
  assign aux_o           = aux_q[NumPipeRegs];
  assign out_valid_o     = valid_q[NumPipeRegs];

  // Output stage: Ready travels backwards from output side
  assign ready_q[NumPipeRegs] = out_ready_i;

  // The pipeline is considered busy if any valid data is in flight
  assign busy_o = (| valid_q);

endmodule
