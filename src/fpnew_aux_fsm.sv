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
// This version can be used for lanes that have some form of FSM in them and only eventually are ready

`include "common_cells/registers.svh"

module fpnew_aux_fsm #(
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,
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
  // Signals for the Lane FSMs
  // Signal to start the FSM, will be asserted for one cycle
  output logic [NumLanes-1:0]                  lane_fsm_start_o,
  // Signal that the FSM finished it's operation, should be asserted continuously
  input logic [NumLanes-1:0]                   lane_fsm_ready_i,
  // Indication of valid data in flight
  output logic                                 busy_o
);

  // ----------
  // Pipeline Distribution
  // ----------
  // This must match between this module and modules that use this module as reg enable input!
  localparam NUM_INP_REGS = (PipeConfig == fpnew_pkg::BEFORE)
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? (NumPipeRegs / 2) // Last to get distributed regs
                               : 0); // Always have one reg to use for FSM Input
  localparam NUM_OUT_REGS = (PipeConfig == fpnew_pkg::AFTER || PipeConfig == fpnew_pkg::INSIDE)
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? ((NumPipeRegs + 1) / 2) // First to get distributed regs
                               : 0); // no regs here otherwise

  // ---------------
  // Input pipeline
  // ---------------
  // Input pipeline signals, index i holds signal after i register stages
  TagType [0:NUM_INP_REGS]               in_tag;
  AuxType [0:NUM_INP_REGS]               in_aux;
  logic   [0:NUM_INP_REGS]               in_is_vector;
  logic   [0:NUM_INP_REGS][NumLanes-1:0] in_lane_active;
  logic   [0:NUM_INP_REGS]               in_valid;

  // Ready signal is combinatorial for all stages
  logic [0:NUM_INP_REGS] in_ready;

  // First element of pipeline is taken from inputs
  assign in_tag        [0] = tag_i;
  assign in_aux        [0] = aux_i;
  assign in_is_vector  [0] = is_vector_i;
  assign in_valid      [0] = in_valid_i;
  assign in_lane_active[0] = lane_active_i;

  // Propagate pipeline ready signal to upstream circuitry
  assign in_ready_o = in_ready[0];

  // Generate the register stages
  for (genvar i = 0; i < NUM_INP_REGS; i++) begin : gen_input_pipeline

    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign in_ready[i] = in_ready[i+1] | ~in_valid[i+1];

    // Valid: enabled by ready signal, synchronous clear with the flush signal
    `FFLARNC(in_valid[i+1], in_valid[i], in_ready[i], flush_i, 1'b0, clk_i, rst_ni)

    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = in_ready[i] & in_valid[i];

    // Drive external registers with reg enable
    assign reg_enable_o[i] = reg_ena;

    // Drive external vector registers with reg enable if operation is a vector
    assign vector_reg_enable_o[i] = reg_ena & in_is_vector[i];
    for (genvar l = 0; l < NumLanes; l++) begin
      assign lane_reg_enable_o[l][i] = reg_ena & in_lane_active[i][l];
    end

    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(        in_tag[i+1],         in_tag[i], reg_ena, TagType'('0))
    `FFL(        in_aux[i+1],         in_aux[i], reg_ena, AuxType'('0))
    `FFL(  in_is_vector[i+1],   in_is_vector[i], reg_ena, '0          )
    `FFL(in_lane_active[i+1], in_lane_active[i], reg_ena, '0          )
  end

  // ----------
  // Global FSM
  // ----------

  // FSM states
  typedef enum logic [1:0] {IDLE, BUSY, HOLD} fsm_state_e;
  fsm_state_e state_q, state_d;

  // Input & Output Handshake
  logic fsm_in_valid, fsm_in_ready;
  logic fsm_out_valid, fsm_out_ready;

  // Synchronisazion signals
  logic fsm_start, fsm_ready, fsm_busy;

  // Data holding signals
  TagType                held_tag;
  AuxType                held_aux;
  logic                  held_is_vector;
  logic   [NumLanes-1:0] held_lane_active;

  // Upstream Handshake Connection
  assign fsm_in_valid = in_valid[NUM_INP_REGS];
  assign in_ready[NUM_INP_REGS] = fsm_in_ready;

  // Done when all active lanes are done
  assign fsm_ready = &lane_fsm_ready_i;

  // FSM to safely apply and receive data from DIVSQRT unit
  always_comb begin : flag_fsm
    // Default assignments
    fsm_out_valid = 1'b0;
    fsm_in_ready  = 1'b0;
    fsm_start     = 1'b0;
    fsm_busy      = 1'b0;
    state_d       = state_q;

    unique case (state_q)
      IDLE: begin
        fsm_in_ready = '1;
        if (fsm_in_valid) begin
          state_d = BUSY;
          fsm_start = 1'b1;
        end
      end
      BUSY: begin
        fsm_busy = 1'b1;
        // If all active lanes are done send data down chain
        if (fsm_ready) begin
          fsm_out_valid = 1'b1;
          if (fsm_out_ready) begin
            fsm_in_ready = 1'b1;
            if (fsm_in_valid) begin
              state_d = BUSY;
              fsm_start = 1'b1;
            end else begin
              state_d = IDLE;
            end
          end else begin
            state_d = HOLD;
          end
        end
      end
      HOLD: begin
        // Exact same as BUSY, but outer condition is already given
        fsm_out_valid = 1'b1;
        if (fsm_out_ready) begin
          fsm_in_ready = 1'b1;
          if (fsm_in_valid) begin
            state_d = BUSY;
            fsm_start = 1'b1;
          end else begin
            state_d = IDLE;
          end
        end else begin
          state_d = HOLD;
        end
      end

      // fall into idle state otherwise
      default: state_d = IDLE;
    endcase

    // Flushing overrides the other actions
    if (flush_i) begin
      fsm_out_valid = 1'b0;
      state_d = IDLE; 
    end
  end

  `FF(state_q, state_d, IDLE);

  // Start Lanes when FSM starts and lane is active
  for (genvar l = 0; l < NumLanes; l++) begin
    assign lane_fsm_start_o[l] = fsm_start && in_lane_active[NUM_INP_REGS][l];
  end

  // ----------------
  // Data Holding FFs
  // ----------------

  `FFL(        held_tag,         in_tag[NUM_INP_REGS], fsm_start, TagType'('0));
  `FFL(        held_aux,         in_aux[NUM_INP_REGS], fsm_start, AuxType'('0));
  `FFL(  held_is_vector,   in_is_vector[NUM_INP_REGS], fsm_start,           '0);
  `FFL(held_lane_active, in_lane_active[NUM_INP_REGS], fsm_start,           '0);

  // ---------------
  // Output pipeline
  // ---------------

  // Output pipeline signals, index i holds signal after i register stages
  TagType [0:NUM_OUT_REGS]               out_tag;
  AuxType [0:NUM_OUT_REGS]               out_aux;
  logic   [0:NUM_OUT_REGS]               out_is_vector;
  logic   [0:NUM_OUT_REGS][NumLanes-1:0] out_lane_active;
  logic   [0:NUM_OUT_REGS]               out_valid;

  // Ready signal is combinatorial for all stages
  logic   [0:NUM_OUT_REGS]               out_ready;

  // Connect to upstream Handshake
  assign out_valid[0] = fsm_out_valid;
  assign fsm_out_ready = out_ready[0];

  // Connect to Hold Register
  assign out_tag        [0] = held_tag;
  assign out_aux        [0] = held_aux;
  assign out_is_vector  [0] = held_is_vector;
  assign out_lane_active[0] = held_lane_active;

  // Generate the register stages
  for (genvar i = 0; i < NUM_OUT_REGS; i++) begin : gen_output_pipeline

    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign out_ready[i] = out_ready[i+1] | ~out_valid[i+1];

    // Valid: enabled by ready signal, synchronous clear with the flush signal
    `FFLARNC(out_valid[i+1], out_valid[i], out_ready[i], flush_i, 1'b0, clk_i, rst_ni)

    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = out_ready[i] & out_valid[i];

    // Drive external registers with reg enable
    assign reg_enable_o[NUM_INP_REGS + i] = reg_ena;

    // Drive external vector registers with reg enable if operation is a vector
    assign vector_reg_enable_o[NUM_INP_REGS + i] = reg_ena & out_is_vector[i];
    for (genvar l = 0; l < NumLanes; l++) begin
      assign lane_reg_enable_o[l][NUM_INP_REGS + i] = reg_ena & out_lane_active[i][l];
    end

    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(        out_tag[i+1],         out_tag[i], reg_ena, TagType'('0))
    `FFL(        out_aux[i+1],         out_aux[i], reg_ena, AuxType'('0))
    `FFL(  out_is_vector[i+1],   out_is_vector[i], reg_ena, '0          )
    `FFL(out_lane_active[i+1], out_lane_active[i], reg_ena, '0          )
  end

  // Ready travels backwards from output side, driven by downstream circuitry
  assign out_ready[NUM_OUT_REGS] = out_ready_i;

  // Assign module outputs
  assign tag_o           = out_tag        [NUM_OUT_REGS];
  assign aux_o           = out_aux        [NUM_OUT_REGS];
  assign is_vector_o     = out_is_vector  [NUM_OUT_REGS];
  assign out_valid_o     = out_valid      [NUM_OUT_REGS];
  assign lane_active_o   = out_lane_active[NUM_OUT_REGS];

  // Assign output Flags: Busy if any element inside the pipe is valid
  assign busy_o          = |in_valid | |out_valid | fsm_busy;

endmodule
