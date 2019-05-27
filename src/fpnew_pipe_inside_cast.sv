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

module fpnew_pipe_inside_cast #(
  parameter int unsigned IntExpWidth = 12,
  parameter int unsigned IntManWidth = 64,
  parameter int unsigned NumPipeRegs = 0,
  parameter type         TagType     = logic,
  parameter type         AuxType     = logic
) (
  input  logic                          clk_i,
  input  logic                          rst_ni,
  // Input signals
  input  logic                          input_sign_i,
  input  logic signed [IntExpWidth-1:0] input_exp_i,
  input  logic signed [IntExpWidth-1:0] destination_exp_i,
  input  logic        [IntManWidth-1:0] input_mant_i,
  input  logic                          src_is_int_i,
  input  logic                          dst_is_int_i,
  input  fpnew_pkg::fp_info_t           info_i,
  input  logic                          mant_is_zero_i,
  input  logic                          op_mod_i,
  input  fpnew_pkg::roundmode_e         rnd_mode_i,
  input  fpnew_pkg::fp_format_e         src_fmt_i,
  input  fpnew_pkg::fp_format_e         dst_fmt_i,
  input  fpnew_pkg::int_format_e        int_fmt_i,
  input  TagType                        tag_i,
  input  AuxType                        aux_i,
  // Input Handshake
  input  logic                          in_valid_i,
  output logic                          in_ready_o,
  input  logic                          flush_i,
  // Output signals
  output logic                          input_sign_o,
  output logic signed [IntExpWidth-1:0] input_exp_o,
  output logic signed [IntExpWidth-1:0] destination_exp_o,
  output logic        [IntManWidth-1:0] input_mant_o,
  output logic                          src_is_int_o,
  output logic                          dst_is_int_o,
  output fpnew_pkg::fp_info_t           info_o,
  output logic                          mant_is_zero_o,
  output logic                          op_mod_o,
  output fpnew_pkg::roundmode_e         rnd_mode_o,
  output fpnew_pkg::fp_format_e         src_fmt_o,
  output fpnew_pkg::fp_format_e         dst_fmt_o,
  output fpnew_pkg::int_format_e        int_fmt_o,
  output TagType                        tag_o,
  output AuxType                        aux_o,
  // Output Handshake
  output logic                          out_valid_o,
  input  logic                          out_ready_i,
  // Status signal
  output logic                          busy_o
);

  // Input signals for the next stage (= output signals of the previous stage)
  logic                   [0:NumPipeRegs]                  input_sign_d;
  logic signed            [0:NumPipeRegs][IntExpWidth-1:0] input_exp_d;
  logic signed            [0:NumPipeRegs][IntExpWidth-1:0] destination_exp_d;
  logic                   [0:NumPipeRegs][IntManWidth-1:0] input_mant_d;
  logic                   [0:NumPipeRegs]                  src_is_int_d;
  logic                   [0:NumPipeRegs]                  dst_is_int_d;
  fpnew_pkg::fp_info_t    [0:NumPipeRegs]                  info_d;
  logic                   [0:NumPipeRegs]                  mant_is_zero_d;
  logic                   [0:NumPipeRegs]                  op_mod_d;
  fpnew_pkg::roundmode_e  [0:NumPipeRegs]                  rnd_mode_d;
  fpnew_pkg::fp_format_e  [0:NumPipeRegs]                  src_fmt_d;
  fpnew_pkg::fp_format_e  [0:NumPipeRegs]                  dst_fmt_d;
  fpnew_pkg::int_format_e [0:NumPipeRegs]                  int_fmt_d;
  TagType                 [0:NumPipeRegs]                  tag_d;
  AuxType                 [0:NumPipeRegs]                  aux_d;
  logic                   [0:NumPipeRegs]                  valid_d;
  // Ready signal is combinatorial for all stages
  logic                   [0:NumPipeRegs]                  stage_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign input_sign_d[0]            = input_sign_i;
  assign input_exp_d[0]             = input_exp_i;
  assign destination_exp_d[0]       = destination_exp_i;
  assign input_mant_d[0]            = input_mant_i;
  assign src_is_int_d[0]            = src_is_int_i;
  assign dst_is_int_d[0]            = dst_is_int_i;
  assign info_d[0]                  = info_i;
  assign mant_is_zero_d[0]          = mant_is_zero_i;
  assign op_mod_d[0]                = op_mod_i;
  assign rnd_mode_d[0]              = rnd_mode_i;
  assign src_fmt_d[0]               = src_fmt_i;
  assign dst_fmt_d[0]               = dst_fmt_i;
  assign int_fmt_d[0]               = int_fmt_i;
  assign tag_d[0]                   = tag_i;
  assign aux_d[0]                   = aux_i;
  assign valid_d[0]                 = in_valid_i;

  // Input stage: Propagate pipeline ready signal
  assign in_ready_o = stage_ready[0];

  // Generate the pipeline stages in case they are needed
  if (NumPipeRegs > 0) begin : gen_pipeline
    // Pipelined versions of signals for later stages
    logic                   [0:NumPipeRegs]                  input_sign_q;
    logic signed            [0:NumPipeRegs][IntExpWidth-1:0] input_exp_q;
    logic signed            [0:NumPipeRegs][IntExpWidth-1:0] destination_exp_q;
    logic                   [0:NumPipeRegs][IntManWidth-1:0] input_mant_q;
    logic                   [0:NumPipeRegs]                  src_is_int_q;
    logic                   [0:NumPipeRegs]                  dst_is_int_q;
    fpnew_pkg::fp_info_t    [0:NumPipeRegs]                  info_q;
    logic                   [0:NumPipeRegs]                  mant_is_zero_q;
    logic                   [0:NumPipeRegs]                  op_mod_q;
    fpnew_pkg::roundmode_e  [0:NumPipeRegs]                  rnd_mode_q;
    fpnew_pkg::fp_format_e  [0:NumPipeRegs]                  src_fmt_q;
    fpnew_pkg::fp_format_e  [0:NumPipeRegs]                  dst_fmt_q;
    fpnew_pkg::int_format_e [0:NumPipeRegs]                  int_fmt_q;
    TagType                 [0:NumPipeRegs]                  tag_q;
    AuxType                 [0:NumPipeRegs]                  aux_q;
    logic                   [0:NumPipeRegs]                  valid_q;

    for (genvar i = 0; i < int'(NumPipeRegs); i++) begin : pipeline_stages
      // Internal register enable for this stage
      logic reg_ena;

      // Next state from previous register to form a shift register
      assign input_sign_d[i+1]            = input_sign_q[i];
      assign input_exp_d[i+1]             = input_exp_q[i];
      assign destination_exp_d[i+1]       = destination_exp_q[i];
      assign input_mant_d[i+1]            = input_mant_q[i];
      assign src_is_int_d[i+1]            = src_is_int_q[i];
      assign dst_is_int_d[i+1]            = dst_is_int_q[i];
      assign info_d[i+1]                  = info_q[i];
      assign mant_is_zero_d[i+1]          = mant_is_zero_q[i];
      assign op_mod_d[i+1]                = op_mod_q[i];
      assign rnd_mode_d[i+1]              = rnd_mode_q[i];
      assign src_fmt_d[i+1]               = src_fmt_q[i];
      assign dst_fmt_d[i+1]               = dst_fmt_q[i];
      assign int_fmt_d[i+1]               = int_fmt_q[i];
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
      `FFL(input_sign_q[i],      input_sign_d[i],      reg_ena, '0)
      `FFL(destination_exp_q[i], destination_exp_d[i], reg_ena, '0)
      `FFL(input_exp_q[i],       input_exp_d[i],       reg_ena, '0)
      `FFL(input_mant_q[i],      input_mant_d[i],      reg_ena, '0)
      `FFL(src_is_int_q[i],      src_is_int_d[i],      reg_ena, '0)
      `FFL(dst_is_int_q[i],      dst_is_int_d[i],      reg_ena, '0)
      `FFL(info_q[i],            info_d[i],            reg_ena, '0)
      `FFL(mant_is_zero_q[i],    mant_is_zero_d[i],    reg_ena, '0)
      `FFL(op_mod_q[i],          op_mod_d[i],          reg_ena, '0)
      `FFL(rnd_mode_q[i],        rnd_mode_d[i],        reg_ena, fpnew_pkg::RNE)
      `FFL(src_fmt_q[i],         src_fmt_d[i],         reg_ena, fpnew_pkg::fp_format_e'(0))
      `FFL(dst_fmt_q[i],         dst_fmt_d[i],         reg_ena, fpnew_pkg::fp_format_e'(0))
      `FFL(int_fmt_q[i],         int_fmt_d[i],         reg_ena, fpnew_pkg::int_format_e'(0))
      `FFL(tag_q[i],             tag_d[i],             reg_ena, '0)
      `FFL(aux_q[i],             aux_d[i],             reg_ena, '0)
    end
  end

  // Output stage: bind last stage outputs to module output. Directly connects to input if no regs.
  assign input_sign_o            = input_sign_d[NumPipeRegs];
  assign input_exp_o             = input_exp_d[NumPipeRegs];
  assign destination_exp_o       = destination_exp_d[NumPipeRegs];
  assign input_mant_o            = input_mant_d[NumPipeRegs];
  assign src_is_int_o            = src_is_int_d[NumPipeRegs];
  assign dst_is_int_o            = dst_is_int_d[NumPipeRegs];
  assign info_o                  = info_d[NumPipeRegs];
  assign mant_is_zero_o          = mant_is_zero_d[NumPipeRegs];
  assign op_mod_o                = op_mod_d[NumPipeRegs];
  assign rnd_mode_o              = rnd_mode_d[NumPipeRegs];
  assign src_fmt_o               = src_fmt_d[NumPipeRegs];
  assign dst_fmt_o               = dst_fmt_d[NumPipeRegs];
  assign int_fmt_o               = int_fmt_d[NumPipeRegs];
  assign tag_o                   = tag_d[NumPipeRegs];
  assign aux_o                   = aux_d[NumPipeRegs];
  assign out_valid_o             = valid_d[NumPipeRegs];

  // Output stage: Ready travels backwards from output side
  assign stage_ready[NumPipeRegs] = out_ready_i;

  // The pipeline is considered busy if any valid data is in flight
  assign busy_o = (| valid_d);

endmodule
