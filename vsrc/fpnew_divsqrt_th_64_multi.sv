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


`include "common_cells/registers.svh"

module fpnew_divsqrt_th_64_multi #(
  parameter fpnew_pkg::fmt_logic_t   FpFmtConfig  = '1,
  // FPU configuration
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::AFTER,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,
  // Do not change
  localparam int unsigned WIDTH       = fpnew_pkg::max_fp_width(FpFmtConfig),
  localparam int unsigned NUM_FORMATS = fpnew_pkg::NUM_FP_FORMATS,
  localparam int unsigned ExtRegEnaWidth = NumPipeRegs == 0 ? 1 : NumPipeRegs
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
  output logic [WIDTH-1:0]            result_o,
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
  logic [1:0][WIDTH-1:0] operands_q;
  fpnew_pkg::roundmode_e rnd_mode_q;
  fpnew_pkg::operation_e op_q;
  fpnew_pkg::fp_format_e dst_fmt_q;
  logic                  in_valid_q;

  // Input pipeline signals, index i holds signal after i register stages
  logic                  [0:NUM_INP_REGS][1:0][WIDTH-1:0]       inp_pipe_operands_q;
  fpnew_pkg::roundmode_e [0:NUM_INP_REGS]                       inp_pipe_rnd_mode_q /*verilator split_var */;
  fpnew_pkg::operation_e [0:NUM_INP_REGS]                       inp_pipe_op_q;
  fpnew_pkg::fp_format_e [0:NUM_INP_REGS]                       inp_pipe_dst_fmt_q;
  TagType                [0:NUM_INP_REGS]                       inp_pipe_tag_q;
  logic                  [0:NUM_INP_REGS]                       inp_pipe_mask_q;
  AuxType                [0:NUM_INP_REGS]                       inp_pipe_aux_q;
  logic                  [0:NUM_INP_REGS]                       inp_pipe_vec_op_q;
  logic                  [0:NUM_INP_REGS]                       inp_pipe_valid_q;
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
  assign in_valid_q = inp_pipe_valid_q[NUM_INP_REGS];

  logic last_inp_reg_ena;
  if (NUM_INP_REGS >= 1) begin : gen_last_inp_reg_ena_valid
    assign last_inp_reg_ena = reg_ena_i[NUM_INP_REGS-1];
  end else begin : gen_last_inp_reg_ena_zero
    assign last_inp_reg_ena = 1'b0;
  end

  logic ext_op_start_q;
  `FF(ext_op_start_q, last_inp_reg_ena, 1'b0)

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

  // ------------
  // Control FSM
  // ------------

  logic in_ready;               // input handshake with upstream
  logic div_valid, sqrt_valid;  // input signalling with unit
  logic unit_ready, unit_done, unit_done_q;  // status signals from unit instance
  logic op_starting;            // high in the cycle a new operation starts
  logic out_valid, out_ready;   // output handshake with downstream
  logic unit_busy;              // valid data in flight
  logic simd_synch_done;
  // FSM states
  typedef enum logic [1:0] {IDLE, BUSY, HOLD} fsm_state_e;
  fsm_state_e state_q, state_d;

  // Valids are gated by the FSM ready. Invalid input ops run a sqrt to not lose illegal instr.
  assign div_valid   = ((in_valid_q & in_ready & ~flush_i) | ext_op_start_q) & (op_q == fpnew_pkg::DIV);
  assign sqrt_valid  = ((in_valid_q & in_ready & ~flush_i) | ext_op_start_q) & (op_q != fpnew_pkg::DIV);
  assign op_starting = div_valid | sqrt_valid;

  // Hold additional information while the operation is in progress
  
  TagType result_tag_q;
  logic result_mask_q;
  AuxType result_aux_q;
  logic result_vec_op_q;

  // Fill the registers everytime a valid operation arrives (load FF, active low asynch rst)
  `FFL(result_tag_q,    inp_pipe_tag_q[NUM_INP_REGS], op_starting, '0)
  `FFL(result_mask_q,   inp_pipe_mask_q[NUM_INP_REGS],op_starting, '0)
  `FFL(result_aux_q,    inp_pipe_aux_q[NUM_INP_REGS], op_starting, '0)
  `FFL(result_vec_op_q, inp_pipe_vec_op_q[NUM_INP_REGS], op_starting, '0)

  // Wait for other lanes only if the operation is vectorial
  assign simd_synch_done = simd_synch_done_i || ~result_vec_op_q;

  // Valid synch with other lanes
  // When one divsqrt unit completes an operation, keep its done high, waiting for the other lanes
  // As soon as all the lanes are over, we can clear this FF and start with a new operation
  logic unit_done_clear;
  `FFLARNC(unit_done_q, unit_done, unit_done, unit_done_clear, 1'b0, clk_i, rst_ni);
  assign unit_done_clear = simd_synch_done | last_inp_reg_ena;
  // Tell the other units that this unit has finished now or in the past
  assign divsqrt_done_o = (unit_done_q | unit_done) & result_vec_op_q;

  // Ready synch with other lanes
  // Bring the FSM-generated ready outside the unit, to synchronize it with the other lanes
  assign divsqrt_ready_o = in_ready;
  // Upstream ready comes from sanitization FSM, and it is synched among all the lanes
  assign inp_pipe_ready[NUM_INP_REGS] = result_vec_op_q ? simd_synch_rdy_i : in_ready;

  // FSM to safely apply and receive data from DIVSQRT unit
  always_comb begin : flag_fsm
    // Default assignments
    in_ready     = 1'b0;
    out_valid    = 1'b0;
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
        // If all the lanes are done with processing
        if (simd_synch_done_i || (~result_vec_op_q && unit_done)) begin
          out_valid = 1'b1; // try to commit result downstream
          // If downstream accepts our result
          if (out_ready) begin
            state_d = IDLE; // we anticipate going back to idling..
            in_ready = 1'b1; // we acknowledge the instruction
            if (in_valid_q && unit_ready) begin // ..unless new work comes in
              state_d  = BUSY; // and stay busy with it
            end
          // Otherwise if downstream is not ready for the result
          end else begin
            state_d     = HOLD; // wait for the pipeline to take the data
          end
        end
      end
      // Waiting with valid result for downstream
      HOLD: begin
        unit_busy    = 1'b1; // data in flight
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

  // -----------------
  // DIVSQRT instance
  // -----------------
  logic [63:0]   unit_result, held_result_q;
  fpnew_pkg::status_t unit_status, held_status_q;
  logic               hold_en;
  
  logic vfdsu_dp_fdiv_busy;
  
  // Regs to save current instruction
  fpnew_pkg::roundmode_e rm_q;
  logic[3:0] divsqrt_fmt_q;
  fpnew_pkg::operation_e divsqrt_op_q;
  logic div_op, sqrt_op;
  logic [WIDTH-1:0] srcf0_q, srcf1_q;
  logic [63:0] srcf0, srcf1;
  
  // Save operands in regs, C910 saves all the following information in its regs in the next cycle.
  `FFL(rm_q, rnd_mode_q, op_starting, fpnew_pkg::RNE)
  `FFL(divsqrt_fmt_q, divsqrt_fmt, op_starting, '0)
  `FFL(divsqrt_op_q, op_q, op_starting, fpnew_pkg::DIV)
  `FFL(srcf0_q, operands_q[0], op_starting, '0)
  `FFL(srcf1_q, operands_q[1], op_starting, '0)

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
  `FFLARNC(func_sel, 1'b1, op_starting, func_sel, 1'b0, clk_i, rst_ni)

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
    .dp_vfdsu_idu_fdiv_issue        ( op_starting               ), // 1. Issue fdiv (FSM in ctrl)
    .forever_cpuclk                 ( clk_i                     ), // Clock input
    .idu_vfpu_rf_pipex_func         ( {3'b0, divsqrt_fmt_q, 11'b0 ,sqrt_op, div_op} ), // Defines format (bits 16,15) and operation (bits 1,0)
    .idu_vfpu_rf_pipex_gateclk_sel  ( func_sel                  ), // 2. Select func
    .pad_yy_icg_scan_en             ( 1'b0                      ), // SE signal for the redundant clock gating module
    .rtu_yy_xx_flush                ( flush_i | last_inp_reg_ena), // Flush
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

  assign unit_ready = !vfdsu_dp_fdiv_busy;

  // Hold the result when one lane has finished execution, except when all the lanes finish together,
  // or the operation is not vectorial, and the result can be accepted downstream
  assign hold_en = unit_done & (~simd_synch_done_i | ~out_ready) & ~(~result_vec_op_q & out_ready);
  // The Hold register (load, no reset)
  `FFLNR(held_result_q, unit_result, hold_en, clk_i)
  `FFLNR(held_status_q, unit_status, hold_en, clk_i)

  // --------------
  // Output Select
  // --------------
  logic [WIDTH-1:0]   result_d;
  fpnew_pkg::status_t status_d;
  // Prioritize hold register data
  assign result_d[WIDTH-1:0] = unit_done_q ? held_result_q[WIDTH-1:0] : unit_result[WIDTH-1:0];
  assign status_d = unit_done_q ? held_status_q : unit_status;

  // ----------------
  // Output Pipeline
  // ----------------
  // Output pipeline signals, index i holds signal after i register stages
  logic               [0:NUM_OUT_REGS][WIDTH-1:0] out_pipe_result_q;
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
  assign out_pipe_tag_q[0]    = result_tag_q;
  assign out_pipe_mask_q[0]   = result_mask_q;
  assign out_pipe_aux_q[0]    = result_aux_q;
  assign out_pipe_valid_q[0]  = out_valid;
  // Input stage: Propagate pipeline ready signal to inside pipe
  assign out_ready = out_pipe_ready[0];
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
  assign busy_o          = (| {inp_pipe_valid_q, unit_busy, out_pipe_valid_q});
endmodule

