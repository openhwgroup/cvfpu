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

`include "common_cells/registers.svh"

module fpnew_pipe_inside_fma #(
  parameter int unsigned ExpWidth    = 10,
  parameter int unsigned PrecBits    = 24,
  parameter int unsigned NumPipeRegs = 0,
  parameter type         FpType      = logic,
  parameter type         TagType     = logic,
  parameter type         AuxType     = logic,
  // Do not change
  localparam int unsigned SUM_WIDTH          = 3*PrecBits+3+1,
  localparam int unsigned SHIFT_AMOUNT_WIDTH = $clog2(3 * PrecBits + 3)
) (
  input  logic                          clk_i,
  input  logic                          rst_ni,
  // Input signals
  input  logic                          effective_subtraction_i,
  input  logic                          final_sign_i,
  input  logic signed [ExpWidth-1:0]    exponent_product_i,
  input  logic signed [ExpWidth-1:0]    exponent_difference_i,
  input  logic signed [ExpWidth-1:0]    tentative_exponent_i,
  input  logic [SHIFT_AMOUNT_WIDTH-1:0] addend_shamt_i,
  input  logic                          sticky_before_add_i,
  input  logic [SUM_WIDTH-1:0]          sum_i,
  input  fpnew_pkg::roundmode_e         rnd_mode_i,
  input  fpnew_pkg::fp_format_e         dst_fmt_i,
  input  logic                          result_is_special_i,
  input  FpType                         special_result_i,
  input  fpnew_pkg::status_t            special_status_i,
  input  TagType                        tag_i,
  input  AuxType                        aux_i,
  // Input Handshake
  input  logic                          in_valid_i,
  output logic                          in_ready_o,
  input  logic                          flush_i,
  // Output signals
  output logic                          effective_subtraction_o,
  output logic                          final_sign_o,
  output logic signed [ExpWidth-1:0]    exponent_product_o,
  output logic signed [ExpWidth-1:0]    exponent_difference_o,
  output logic signed [ExpWidth-1:0]    tentative_exponent_o,
  output logic [SHIFT_AMOUNT_WIDTH-1:0] addend_shamt_o,
  output logic                          sticky_before_add_o,
  output logic [SUM_WIDTH-1:0]          sum_o,
  output fpnew_pkg::roundmode_e         rnd_mode_o,
  output fpnew_pkg::fp_format_e         dst_fmt_o,
  output logic                          result_is_special_o,
  output FpType                         special_result_o,
  output fpnew_pkg::status_t            special_status_o,
  output TagType                        tag_o,
  output AuxType                        aux_o,
  // Output Handshake
  output logic                          out_valid_o,
  input  logic                          out_ready_i,
  // Status signal
  output logic                          busy_o
);

  // Input signals for the next stage (= output signals of the previous stage)
  logic                  [0:NumPipeRegs]                         effective_subtraction_d;
  logic                  [0:NumPipeRegs]                         final_sign_d;
  logic signed           [0:NumPipeRegs][ExpWidth-1:0]           exponent_product_d;
  logic signed           [0:NumPipeRegs][ExpWidth-1:0]           exponent_difference_d;
  logic signed           [0:NumPipeRegs][ExpWidth-1:0]           tentative_exponent_d;
  logic                  [0:NumPipeRegs][SHIFT_AMOUNT_WIDTH-1:0] addend_shamt_d;
  logic                  [0:NumPipeRegs]                         sticky_before_add_d;
  logic                  [0:NumPipeRegs][SUM_WIDTH-1:0]          sum_d;
  fpnew_pkg::roundmode_e [0:NumPipeRegs]                         rnd_mode_d;
  fpnew_pkg::fp_format_e [0:NumPipeRegs]                         dst_fmt_d;
  logic                  [0:NumPipeRegs]                         result_is_special_d;
  FpType                 [0:NumPipeRegs]                         special_result_d;
  fpnew_pkg::status_t    [0:NumPipeRegs]                         special_status_d;
  TagType                [0:NumPipeRegs]                         tag_d;
  AuxType                [0:NumPipeRegs]                         aux_d;
  logic                  [0:NumPipeRegs]                         valid_d;
  // Ready signal is combinatorial for all stages
  logic                  [0:NumPipeRegs]                         stage_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign effective_subtraction_d[0] = effective_subtraction_i;
  assign final_sign_d[0]            = final_sign_i;
  assign exponent_product_d[0]      = exponent_product_i;
  assign exponent_difference_d[0]   = exponent_difference_i;
  assign tentative_exponent_d[0]    = tentative_exponent_i;
  assign addend_shamt_d[0]          = addend_shamt_i;
  assign sticky_before_add_d[0]     = sticky_before_add_i;
  assign sum_d[0]                   = sum_i;
  assign rnd_mode_d[0]              = rnd_mode_i;
  assign dst_fmt_d[0]               = dst_fmt_i;
  assign result_is_special_d[0]     = result_is_special_i;
  assign special_result_d[0]        = special_result_i;
  assign special_status_d[0]        = special_status_i;
  assign tag_d[0]                   = tag_i;
  assign aux_d[0]                   = aux_i;
  assign valid_d[0]                 = in_valid_i;

  // Input stage: Propagate pipeline ready signal
  assign in_ready_o = stage_ready[0];

  // Generate the pipeline stages in case they are needed
  if (NumPipeRegs > 0) begin : gen_pipeline
    // Pipelined versions of signals for later stages
    logic                  [0:NumPipeRegs]                         effective_subtraction_q;
    logic                  [0:NumPipeRegs]                         final_sign_q;
    logic signed           [0:NumPipeRegs][ExpWidth-1:0]           exponent_product_q;
    logic signed           [0:NumPipeRegs][ExpWidth-1:0]           exponent_difference_q;
    logic signed           [0:NumPipeRegs][ExpWidth-1:0]           tentative_exponent_q;
    logic                  [0:NumPipeRegs][SHIFT_AMOUNT_WIDTH-1:0] addend_shamt_q;
    logic                  [0:NumPipeRegs]                         sticky_before_add_q;
    logic                  [0:NumPipeRegs][SUM_WIDTH-1:0]          sum_q;
    logic                  [0:NumPipeRegs][3*PrecBits+3:0]         product_shifted_q;
    logic                  [0:NumPipeRegs][3*PrecBits+3:0]         addend_shifted_q;
    logic                  [0:NumPipeRegs]                         inject_carry_in_q;
    fpnew_pkg::roundmode_e [0:NumPipeRegs]                         rnd_mode_q;
    fpnew_pkg::fp_format_e [0:NumPipeRegs]                         dst_fmt_q;
    logic                  [0:NumPipeRegs]                         result_is_special_q;
    FpType                 [0:NumPipeRegs]                         special_result_q;
    fpnew_pkg::status_t    [0:NumPipeRegs]                         special_status_q;
    TagType                [0:NumPipeRegs]                         tag_q;
    AuxType                [0:NumPipeRegs]                         aux_q;
    logic                  [0:NumPipeRegs]                         valid_q;

    for (genvar i = 0; i < int'(NumPipeRegs); i++) begin : pipeline_stages
      // Internal register enable for this stage
      logic reg_ena;

      // Next state from previous register to form a shift register
      assign effective_subtraction_d[i+1] = effective_subtraction_q[i];
      assign final_sign_d[i+1]            = final_sign_q[i];
      assign exponent_product_d[i+1]      = exponent_product_q[i];
      assign exponent_difference_d[i+1]   = exponent_difference_q[i];
      assign tentative_exponent_d[i+1]    = tentative_exponent_q[i];
      assign addend_shamt_d[i+1]          = addend_shamt_q[i];
      assign sticky_before_add_d[i+1]     = sticky_before_add_q[i];
      assign sum_d[i+1]                   = sum_q[i];
      assign rnd_mode_d[i+1]              = rnd_mode_q[i];
      assign dst_fmt_d[i+1]               = dst_fmt_q[i];
      assign result_is_special_d[i+1]     = result_is_special_q[i];
      assign special_result_d[i+1]        = special_result_q[i];
      assign special_status_d[i+1]        = special_status_q[i];
      assign tag_d[i+1]                   = tag_q[i];
      assign aux_d[i+1]                   = aux_q[i];
      assign valid_d[i+1]                 = valid_q[i];

      // Determine the ready signal of the current stage - advance the pipeline:
      // 1. if the next stage is ready for our data
      // 2. if the next stage only holds a bubble (not valid) -> we can pop it
      assign stage_ready[i] = stage_ready[i+1] | ~valid_q[i];

      // Valid: enabled by ready signal, synchronous clear with the flush signal
      `FFLARNC(valid_q[i], valid_d[i], stage_ready[i], flush_i, 1'b0, clk_i, rst_ni)

      // Enable register if pipleine ready and a valid data item is present
      assign reg_ena = stage_ready[i] & valid_d[i];

      // Generate the pipeline registers within the stages, use enable-registers
      `FFL(effective_subtraction_q[i], effective_subtraction_d[i], reg_ena, '0)
      `FFL(final_sign_q[i],            final_sign_d[i],            reg_ena, '0)
      `FFL(exponent_product_q[i],      exponent_product_d[i],      reg_ena, '0)
      `FFL(exponent_difference_q[i],   exponent_difference_d[i],   reg_ena, '0)
      `FFL(tentative_exponent_q[i],    tentative_exponent_d[i],    reg_ena, '0)
      `FFL(addend_shamt_q[i],          addend_shamt_d[i],          reg_ena, '0)
      `FFL(sticky_before_add_q[i],     sticky_before_add_d[i],     reg_ena, '0)
      `FFL(sum_q[i],                   sum_d[i],               reg_ena, '0)
      `FFL(rnd_mode_q[i],              rnd_mode_d[i],              reg_ena, fpnew_pkg::RNE)
      `FFL(dst_fmt_q[i],               dst_fmt_d[i],               reg_ena, fpnew_pkg::fp_format_e'(0))
      `FFL(result_is_special_q[i],     result_is_special_d[i],     reg_ena, '0)
      `FFL(special_result_q[i],        special_result_d[i],        reg_ena, '0)
      `FFL(special_status_q[i],        special_status_d[i],        reg_ena, '0)
      `FFL(tag_q[i],                   tag_d[i],                   reg_ena, '0)
      `FFL(aux_q[i],                   aux_d[i],                   reg_ena, '0)
    end
  end

  // Output stage: bind last stage outputs to module output. Directly connects to input if no regs.
  assign effective_subtraction_o = effective_subtraction_d[NumPipeRegs];
  assign final_sign_o            = final_sign_d[NumPipeRegs];
  assign exponent_product_o      = exponent_product_d[NumPipeRegs];
  assign exponent_difference_o   = exponent_difference_d[NumPipeRegs];
  assign tentative_exponent_o    = tentative_exponent_d[NumPipeRegs];
  assign addend_shamt_o          = addend_shamt_d[NumPipeRegs];
  assign sticky_before_add_o     = sticky_before_add_d[NumPipeRegs];
  assign sum_o                   = sum_d[NumPipeRegs];
  assign rnd_mode_o              = rnd_mode_d[NumPipeRegs];
  assign dst_fmt_o               = dst_fmt_d[NumPipeRegs];
  assign result_is_special_o     = result_is_special_d[NumPipeRegs];
  assign special_result_o        = special_result_d[NumPipeRegs];
  assign special_status_o        = special_status_d[NumPipeRegs];
  assign tag_o                   = tag_d[NumPipeRegs];
  assign aux_o                   = aux_d[NumPipeRegs];
  assign out_valid_o             = valid_d[NumPipeRegs];

  // Output stage: Ready travels backwards from output side
  assign stage_ready[NumPipeRegs] = out_ready_i;

  // The pipeline is considered busy if any valid data is in flight
  assign busy_o = (| valid_d);

endmodule
