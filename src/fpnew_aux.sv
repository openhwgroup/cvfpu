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
  parameter type                     AuxType     = logic,
  parameter int unsigned             NumLanes    = 1
) (
  input logic                                  clk_i,
  input logic                                  rst_ni,
  // Input signals
  input TagType                                tag_i,
  input AuxType                                aux_i,
  input logic                                  is_vector_i,
  input logic [NumLanes-1:0]                   lane_active_i,        
  // Input Handshake
  input  logic                                 in_valid_i,
  output logic                                 in_ready_o,
  input  logic                                 flush_i,
  // Output signals
  output TagType                               tag_o,
  output AuxType                               aux_o,
  output logic                                 is_vector_o,
  output logic [NumLanes-1:0]                  lane_active_o,        
  // Output handshake
  output logic                                 out_valid_o,
  input  logic                                 out_ready_i,
  // Register Enable for Lanes
  output logic [NumPipeRegs-1:0]               reg_enable_o,
  output logic [NumPipeRegs-1:0]               vector_reg_enable_o,
  output logic [NumLanes-1:0][NumPipeRegs-1:0] lane_reg_enable_o,
  // Indication of valid data in flight
  output logic                                 busy_o
);


  // ---------------
  // Input pipeline
  // ---------------
  // Input pipeline signals, index i holds signal after i register stages
  TagType                [0:NumPipeRegs]                 tag;
  AuxType                [0:NumPipeRegs]                 aux;
  logic                  [0:NumPipeRegs]                 is_vector;
  logic                  [0:NumPipeRegs][NumLanes-1:0]   lane_active;
  logic                  [0:NumPipeRegs]                 valid;

  // Ready signal is combinatorial for all stages
  logic [0:NumPipeRegs] ready;

  // First element of pipeline is taken from inputs
  assign tag        [0] = tag_i;
  assign aux        [0] = aux_i;
  assign is_vector  [0] = is_vector_i;
  assign valid      [0] = in_valid_i;
  assign lane_active[0] = lane_active_i;

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

    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = ready[i] & valid[i];

    // Drive external registers with reg enable
    assign reg_enable_o[i] = reg_ena;

    // Drive external vector registers with reg enable if operation is a vector
    assign vector_reg_enable_o[i] = reg_ena & is_vector[i];
    for (genvar l = 0; l < NumLanes; l++) begin
      assign lane_reg_enable_o[l][i] = reg_ena & lane_active[i][l];
    end

    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(        tag[i+1],         tag[i], reg_ena, TagType'('0))
    `FFL(        aux[i+1],         aux[i], reg_ena, AuxType'('0))
    `FFL(  is_vector[i+1],   is_vector[i], reg_ena, '0          )
    `FFL(lane_active[i+1], lane_active[i], reg_ena, '0          )
  end

  // Ready travels backwards from output side, driven by downstream circuitry
  assign ready[NumPipeRegs] = out_ready_i;

  // Assign module outputs
  assign tag_o           = tag        [NumPipeRegs];
  assign aux_o           = aux        [NumPipeRegs];
  assign is_vector_o     = is_vector  [NumPipeRegs];
  assign out_valid_o     = valid      [NumPipeRegs];
  assign lane_active_o   = lane_active[NumPipeRegs];

  // Assign output Flags: Busy if any element inside the pipe is valid
  assign busy_o          = |valid;
endmodule
