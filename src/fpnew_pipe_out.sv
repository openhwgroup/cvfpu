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

`include "registers.svh"

module fpnew_pipe_out #(
  parameter int unsigned Width         = 32,
  parameter int unsigned NumPipeRegs   = 0,
  parameter type         TagType       = logic,
  parameter type         AuxType       = logic
) (
  input  logic                  clk_i,
  input  logic                  rst_ni,
  // Input signals
  input  logic [Width-1:0]      result_i,
  input  fpnew_pkg::status_t    status_i,
  input  logic                  extension_bit_i,
  input  fpnew_pkg::classmask_e class_mask_i,
  input  logic                  is_class_i,
  input  TagType                tag_i,
  input  AuxType                aux_i,
  // Input Handshake
  input  logic                  in_valid_i,
  output logic                  in_ready_o,
  input  logic                  flush_i,
  // Output signals
  output logic [Width-1:0]      result_o,
  output fpnew_pkg::status_t    status_o,
  output logic                  extension_bit_o,
  output fpnew_pkg::classmask_e class_mask_o,
  output logic                  is_class_o,
  output TagType                tag_o,
  output AuxType                aux_o,
  // Output Handshake
  output logic                  out_valid_o,
  input  logic                  out_ready_i,
  // Status signal
  output logic                  busy_o
);

  // Input signals for the next stage (= output signals of the previous stage)
  logic                  [NumPipeRegs:0][Width-1:0] result_d;
  fpnew_pkg::status_t    [NumPipeRegs:0]            status_d;
  logic                  [NumPipeRegs:0]            extension_bit_d;
  fpnew_pkg::classmask_e [NumPipeRegs:0]            class_mask_d;
  logic                  [NumPipeRegs:0]            is_class_d;
  TagType                [NumPipeRegs:0]            tag_d;
  AuxType                [NumPipeRegs:0]            aux_d;
  logic                  [NumPipeRegs:0]            valid_d;
  // Ready signal is combinatorial for all stages
  logic                  [NumPipeRegs:0]            stage_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign result_d[0]        = result_i;
  assign status_d[0]        = status_i;
  assign extension_bit_d[0] = extension_bit_i;
  assign class_mask_d[0]    = class_mask_i;
  assign is_class_d[0]      = is_class_i;
  assign tag_d[0]           = tag_i;
  assign aux_d[0]           = aux_i;
  assign valid_d[0]         = in_valid_i;

  // Input stage: Propagate ready signal from pipeline
  assign in_ready_o = stage_ready[0];

  // Generate the pipeline stages in case they are needed
  if (NumPipeRegs > 0) begin : gen_pipeline
    // Pipelined versions of signals for later stages
    logic                  [NumPipeRegs-1:0][Width-1:0] result_q;
    fpnew_pkg::status_t    [NumPipeRegs-1:0]            status_q;
    logic                  [NumPipeRegs-1:0]            extension_bit_q;
    fpnew_pkg::classmask_e [NumPipeRegs-1:0]            class_mask_q;
    logic                  [NumPipeRegs-1:0]            is_class_q;
    TagType                [NumPipeRegs-1:0]            tag_q;
    AuxType                [NumPipeRegs-1:0]            aux_q;
    logic                  [NumPipeRegs-1:0]            valid_q;

    for (genvar i = 0; i < NumPipeRegs; i++) begin : pipeline_stages
      // Internal register enable for this stage -> creates gated registers if supported in synth
      logic reg_ena;

      // Next state from previous register to form a shift register
      assign result_d[i+1]        = result_q[i];
      assign status_d[i+1]        = status_q[i];
      assign extension_bit_d[i+1] = extension_bit_q[i];
      assign class_mask_d[i+1]    = class_mask_q[i];
      assign is_class_d[i+1]      = is_class_q[i];
      assign tag_d[i+1]           = tag_q[i];
      assign aux_d[i+1]           = aux_q[i];
      assign valid_d[i+1]         = valid_q[i];

      // Determine the ready signal of the current stage - advance the pipeline:
      // 1. if the next stage is ready for our data
      // 2. if the next stage register only holds a bubble (not valid) -> we can pop it
      assign stage_ready[i] = stage_ready[i+1] | ~valid_q[i];

      // Valid registers: enabled by ready signal, synchronous clear with the flush signal
      `FFLARNC(valid_q[i], valid_d[i], stage_ready[i], flush_i, 1'b0, clk_i, rst_ni)

      // Enable the payload registers if pipleine ready and a valid data item is present (gating)
      assign reg_ena = stage_ready[i] & valid_d[i];

      // Generate the pipeline registers within the stages, use enable-registers
      `FFL(result_q[i],        result_d[i],        reg_ena, '0)
      `FFL(status_q[i],        status_d[i],        reg_ena, '0)
      `FFL(extension_bit_q[i], extension_bit_d[i], reg_ena, '0)
      `FFL(class_mask_q[i],    class_mask_d[i],    reg_ena, fpnew_pkg::QNAN)
      `FFL(is_class_q[i],      is_class_d[i],      reg_ena, '0)
      `FFL(tag_q[i],           tag_d[i],           reg_ena, '0)
      `FFL(aux_q[i],           aux_d[i],           reg_ena, '0)
    end
  end

  // Output stage: bind last stage outputs to module output. Directly connects to input if no regs.
  assign result_o        = result_d[NumPipeRegs];
  assign status_o        = status_d[NumPipeRegs];
  assign extension_bit_o = extension_bit_d[NumPipeRegs];
  assign class_mask_o    = class_mask_d[NumPipeRegs];
  assign is_class_o      = is_class_d[NumPipeRegs];
  assign tag_o           = tag_d[NumPipeRegs];
  assign aux_o           = aux_d[NumPipeRegs];
  assign out_valid_o     = valid_d[NumPipeRegs];

  // Output stage: Ready travels backwards from output side
  assign stage_ready[NumPipeRegs] = out_ready_i;

  // The pipeline is considered busy if any valid data is in flight
  assign busy_o = (| valid_d);

endmodule
