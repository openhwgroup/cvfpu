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

`include "registers.svh"

module fpnew_divsqrt_multi #(
  parameter fpnew_pkg::fmt_logic_t   FpFmtConfig  = '1,
  // FPU configuration
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::AFTER,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,
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
  input  TagType                      tag_i,
  input  AuxType                      aux_i,
  // Input Handshake
  input  logic                        in_valid_i,
  output logic                        in_ready_o,
  input  logic                        flush_i,
  // Output signals
  output logic [WIDTH-1:0]            result_o,
  output fpnew_pkg::status_t          status_o,
  output logic                        extension_bit_o,
  output TagType                      tag_o,
  output AuxType                      aux_o,
  // Output handshake
  output logic                        out_valid_o,
  input  logic                        out_ready_i,
  // Indication of valid data in flight
  output logic                        busy_o
);

  // ---------------
  // Input pipeline
  // ---------------
  // Pipelined input signals
  logic [1:0][WIDTH-1:0]       operands_q;
  logic [NUM_FORMATS-1:0][1:0] is_boxed_q;
  fpnew_pkg::roundmode_e       rnd_mode_q;
  fpnew_pkg::operation_e       op_q;
  logic                        op_mod_q;
  fpnew_pkg::fp_format_e       dst_fmt_q;
  TagType                      tag_q;
  AuxType                      aux_q;
  logic                        in_valid_q, in_ready_q;
  logic                        pipe_busy;

  // Generate pipeline at input if needed
  if (PipeConfig==fpnew_pkg::BEFORE) begin : input_pipeline
    fpnew_pipe_in #(
      .Width       ( WIDTH       ),
      .NumPipeRegs ( NumPipeRegs ),
      .NumOperands ( 2           ),
      .NumFormats  ( NUM_FORMATS ),
      .TagType     ( TagType     ),
      .AuxType     ( AuxType     )
    ) i_input_pipe (
      .clk_i,
      .rst_ni,
      .operands_i,
      .is_boxed_i,
      .rnd_mode_i,
      .op_i,
      .op_mod_i    ( 1'b0            ), // unused
      .src_fmt_i   ( fpnew_pkg::FP32 ), // unused
      .dst_fmt_i,
      .int_fmt_i   ( fpnew_pkg::INT8 ), // unused,
      .tag_i,
      .aux_i,
      .in_valid_i,
      .in_ready_o,
      .flush_i,
      .operands_o  ( operands_q      ),
      .is_boxed_o  ( is_boxed_q      ),
      .rnd_mode_o  ( rnd_mode_q      ),
      .op_o        ( op_q            ),
      .op_mod_o    ( /* unused */    ),
      .src_fmt_o   ( /* unused */    ),
      .dst_fmt_o   ( dst_fmt_q       ),
      .int_fmt_o   ( /* unused */    ),
      .tag_o       ( tag_q           ),
      .aux_o       ( aux_q           ),
      .out_valid_o ( in_valid_q      ),
      .out_ready_i ( in_ready_q      ),
      .busy_o      ( pipe_busy       )
    );
  // Otherwise pass through inputs
  end else begin : no_input_pipeline
    assign operands_q = operands_i;
    assign is_boxed_q = is_boxed_i;
    assign rnd_mode_q = rnd_mode_i;
    assign op_q       = op_i;
    assign dst_fmt_q  = dst_fmt_i;
    assign tag_q      = tag_i;
    assign aux_q      = aux_i;
    assign in_valid_q = in_valid_i;
    assign in_ready_o = in_ready_q;
  end

  // -----------------
  // Input processing
  // -----------------
  logic [1:0]       divsqrt_fmt;
  logic [1:0][63:0] divsqrt_operands; // those are fixed to 64bit
  logic             input_is_fp8;

  // Translate fpnew formats into divsqrt formats
  always_comb begin : translate_fmt
    unique case (dst_fmt_q)
      fpnew_pkg::FP32:    divsqrt_fmt = 2'b00;
      fpnew_pkg::FP64:    divsqrt_fmt = 2'b01;
      fpnew_pkg::FP16:    divsqrt_fmt = 2'b10;
      fpnew_pkg::FP16ALT: divsqrt_fmt = 2'b11;
      default:            divsqrt_fmt = 2'b10; // maps also FP8 to FP16
    endcase

    // Only if FP8 is enabled
    input_is_fp8 = FpFmtConfig[fpnew_pkg::FP8] & (dst_fmt_q == fpnew_pkg::FP8);

    // If FP8 is supported, map it to an FP16 value
    divsqrt_operands[0] = input_is_fp8 ? operands_q[0] << 8 : operands_q[0];
    divsqrt_operands[1] = input_is_fp8 ? operands_q[1] << 8 : operands_q[1];
  end

  // ------------
  // Control FSM
  // ------------
  logic in_ready;               // input handshake with upstream
  logic div_valid, sqrt_valid;  // input signalling with unit
  logic unit_ready, unit_done;  // status signals from unit instance
  logic out_valid, out_ready;   // output handshake with downstream
  logic hold_result;            // whether to put result into hold register
  logic data_is_held;           // data in hold register is valid
  logic unit_busy;              // valid data in flight
  // FSM states
  typedef enum logic [1:0] {IDLE, BUSY, HOLD} fsm_state_e;
  fsm_state_e state_q, state_d;

  // Upstream ready comes from sanitization FSM
  assign in_ready_q = in_ready;

  // Valids are gated by the FSM ready. Invalid input ops run a sqrt to not lose illegal instr.
  assign div_valid  = in_valid_q & (op_q == fpnew_pkg::DIV) & in_ready & ~flush_i;
  assign sqrt_valid = in_valid_q & (op_q != fpnew_pkg::DIV) & in_ready & ~flush_i;

  // FSM to safely apply and receive data from DIVSQRT unit
  always_comb begin : flag_fsm
    // Default assignments
    in_ready     = 1'b0;
    out_valid    = 1'b0;
    hold_result  = 1'b0;
    data_is_held = 1'b0;
    unit_busy    = 1'b0;
    state_d      = state_q;

    unique case (state_q)
      // Waiting for work
      IDLE: begin
        in_ready = 1'b1; // we're ready
        if (in_valid_q && unit_ready) begin // New work arrives
          state_d = BUSY; // go into processing state
        end
      end
      // Operation in progress
      BUSY: begin
        unit_busy = 1'b1; // data in flight
        // If the unit is done with processing
        if (unit_done) begin
          out_valid = 1'b1; // try to commit result downstream
          // If downstream accepts our result
          if (out_ready) begin
            state_d = IDLE; // we anticipate going back to idling..
            if (in_valid_q && unit_ready) begin // ..unless new work comes in
              in_ready = 1'b1; // we acknowledge the instruction
              state_d  = BUSY; // and stay busy with it
            end
          // Otherwise if downstream is not ready for the result
          end else begin
            hold_result = 1'b1; // activate the hold register
            state_d     = HOLD; // wait for the pipeline to take the data
          end
        end
      end
      // Waiting with valid result for downstream
      HOLD: begin
        unit_busy    = 1'b1; // data in flight
        data_is_held = 1'b1; // data in hold register is valid
        out_valid    = 1'b1; // try to commit result downstream
        // If the result is accepted by downstream
        if (out_ready) begin
          state_d = IDLE; // go back to idle..
          if (in_valid_q && unit_ready) begin // ..unless new work comes in
            in_ready = 1'b1; // acknowledge the new transaction
            state_d  = BUSY; // will be busy with the next instruction
          end
        end
      end
      // fall into idle state otherwise
      default: state_d = IDLE;
    endcase

    // Flushing overrides the other actions
    if (flush_i) begin
      unit_busy = 1'b0; // data is invalidated
      out_valid = 1'b0; // cancel any valid data
      state_d   = IDLE; // go to default state
    end
  end

  // FSM status register (asynch active low reset)
  `FF(state_q, state_d, IDLE)

  // Hold additional information while the operation is in progress
  logic result_is_fp8_q;
  TagType result_tag_q;
  AuxType result_aux_q;

  // Fill the registers everytime a valid operation arrives (load FF, active low asynch rst)
  `FFL(result_is_fp8_q, input_is_fp8, in_valid_q, '0)
  `FFL(result_tag_q,    tag_q,        in_valid_q, '0)
  `FFL(result_aux_q,    aux_q,        in_valid_q, '0)

  // -----------------
  // DIVSQRT instance
  // -----------------
  logic [63:0]        unit_result;
  logic [WIDTH-1:0]   adjusted_result, held_result_q;
  fpnew_pkg::status_t unit_status, held_status_q;

  div_sqrt_top_mvp i_divsqrt_lei (
   .Clk_CI           ( clk_i               ),
   .Rst_RBI          ( rst_ni              ),
   .Div_start_SI     ( div_valid           ),
   .Sqrt_start_SI    ( sqrt_valid          ),
   .Operand_a_DI     ( divsqrt_operands[0] ),
   .Operand_b_DI     ( divsqrt_operands[1] ),
   .RM_SI            ( rnd_mode_q          ),
   .Precision_ctl_SI ( '0                  ),
   .Format_sel_SI    ( divsqrt_fmt         ),
   .Kill_SI          ( flush_i             ),
   .Result_DO        ( unit_result         ),
   .Fflags_SO        ( unit_status         ),
   .Ready_SO         ( unit_ready          ),
   .Done_SO          ( unit_done           )
  );

  // Adjust result width and fix FP8
  assign adjusted_result = result_is_fp8_q ? unit_result >> 8 : unit_result;

  // The Hold register (load, no reset)
  `FFLNR(held_result_q, adjusted_result, hold_result, clk_i)
  `FFLNR(held_status_q, unit_status,     hold_result, clk_i)

  // --------------
  // Output Select
  // --------------
  logic [WIDTH-1:0]   result_d;
  fpnew_pkg::status_t status_d;
  // Prioritize hold register data
  assign result_d = data_is_held ? held_result_q : adjusted_result;
  assign status_d = data_is_held ? held_status_q : unit_status;

  // ----------------
  // Output Pipeline
  // ----------------
  // Generate pipeline at output if needed
  if (PipeConfig==fpnew_pkg::AFTER) begin : output_pipline
    fpnew_pipe_out #(
      .Width       ( WIDTH       ),
      .NumPipeRegs ( NumPipeRegs ),
      .TagType     ( TagType     ),
      .AuxType     ( AuxType     )
    ) i_output_pipe (
      .clk_i,
      .rst_ni,
      .result_i        ( result_d        ),
      .status_i        ( status_d        ),
      .extension_bit_i ( 1'b1            ), // always NaN-box
      .tag_i           ( result_tag_q    ),
      .aux_i           ( result_aux_q    ),
      .in_valid_i      ( out_valid       ),
      .in_ready_o      ( out_ready       ),
      .class_mask_i    ( fpnew_pkg::QNAN ), // unused
      .is_class_i      ( 1'b0            ), // unused
      .flush_i,
      .result_o,
      .status_o,
      .extension_bit_o,
      .class_mask_o    ( /* unused */  ),
      .is_class_o      ( /* unused */  ),
      .tag_o,
      .aux_o,
      .out_valid_o,
      .out_ready_i,
      .busy_o          ( pipe_busy     )
    );
  // Otherwise pass through outputs
  end else begin : no_output_pipeline
    assign result_o        = result_d;
    assign status_o        = status_d;
    assign extension_bit_o = 1'b1; // always NaN-box
    assign tag_o           = result_tag_q;
    assign aux_o           = result_aux_q;
    assign out_valid_o     = out_valid;
    assign out_ready       = out_ready_i;
  end

  // Busy flag
  assign busy_o = in_valid_q | unit_busy | pipe_busy;
endmodule
