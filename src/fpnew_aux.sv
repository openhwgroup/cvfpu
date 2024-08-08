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

// Author: Maurus Item <itemm@student.ethz.ch>
//
// Description Aux chain for FPNew, handles transmitting of shared handshake and aux data
// And enables the correct lanes so they always stay in sync.

`include "common_cells/registers.svh"

module fpnew_aux #(
  parameter int unsigned             NumPipeRegs = 0,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic
) (
  input logic                                  clk_i,
  input logic                                  rst_ni,
  // Input signals
  input TagType                                tag_i,
  input AuxType                                aux_i,
  // Input Handshake
  input  logic                                 in_valid_i,
  output logic                                 in_ready_o,
  input  logic                                 flush_i,
  // Output signals
  output TagType                               tag_o,
  output AuxType                               aux_o,
  // Output handshake
  output logic                                 out_valid_o,
  input  logic                                 out_ready_i,
  // Register Enable for Lanes
  output logic [NumPipeRegs-1:0]               reg_enable_o,
  // External register enable override
  input  logic [NumPipeRegs-1:0]               reg_ena_i,
  // Indication of valid data in flight
  output logic                                 busy_o
);


  // ---------------
  // Input pipeline
  // ---------------
  // Input pipeline signals, index i holds signal after i register stages
  TagType                [0:NumPipeRegs]                 tag;
  AuxType                [0:NumPipeRegs]                 aux;
  logic                  [0:NumPipeRegs]                 valid;

  // Ready signal is combinatorial for all stages
  logic [0:NumPipeRegs] ready;

  // First element of pipeline is taken from inputs
  assign tag        [0] = tag_i;
  assign aux        [0] = aux_i;
  assign valid      [0] = in_valid_i;

  // Propagate pipeline ready signal to upstream circuitry
  assign in_ready_o = ready[0];

  // Generate the register stages
  for (genvar i = 0; i < NumPipeRegs; i++) begin : gen_input_pipeline

    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign ready[i] = ready[i+1] | ~valid[i+1];

    // Valid: enabled by ready signal, synchronous clear with the flush signal
    `FFLARNC(valid[i+1], valid[i], ready[i], flush_i, 1'b0, clk_i, rst_ni)

    // Enable register if pipeline ready and a valid data item is present
    assign reg_enable_o[i] = ready[i] & valid[i] | reg_ena_i[i];

    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(        tag[i+1],         tag[i], reg_enable_o[i], TagType'('0))
    `FFL(        aux[i+1],         aux[i], reg_enable_o[i], AuxType'('0))
  end

  // Ready travels backwards from output side, driven by downstream circuitry
  assign ready[NumPipeRegs] = out_ready_i;

  // Assign module outputs
  assign tag_o           = tag        [NumPipeRegs];
  assign aux_o           = aux        [NumPipeRegs];
  assign out_valid_o     = valid      [NumPipeRegs];

  // Assign output Flags: Busy if any element inside the pipe is valid
  assign busy_o          = |valid;
endmodule
