// Copyright 2019-2022 ETH Zurich and University of Bologna.
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
//          Luca Bertaccini <lbertaccini@iis.ee.ethz.ch>
//          Jiang Lannan <jiangl@ethz.ch>
//          Kexin Li <likexi@ethz.ch>

`include "common_cells/registers.svh"

module fpnew_divsqrt_th_32 #(
  // FP32-only DivSqrt
  // FPU configuration
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,
  // Do not change
  localparam int unsigned WIDTH       = 32,
  localparam int unsigned NUM_FORMATS = fpnew_pkg::NUM_FP_FORMATS
) (
  input  logic                        clk_i,
  input  logic                        rst_ni,
  // Input signals
  input  logic [1:0][WIDTH-1:0]       operands_i, // 2 operands
  input  logic [NUM_FORMATS-1:0][1:0] is_boxed_i, // 2 operands
  input  fpnew_pkg::roundmode_e       rnd_mode_i,
  input  fpnew_pkg::operation_e       op_i,
  input  logic                        mask_i,
  // Output signals
  output logic [WIDTH-1:0]            result_o,
  output fpnew_pkg::status_t          status_o,
  output logic                        extension_bit_o,
  output logic                        mask_o,
  // External Register Control
  input  logic                        flush_i,
  input logic[NumPipeRegs-1:0]        reg_enable_i,
  input logic                         fsm_start_i,
  output logic                        fsm_ready_o
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

  // Input pipeline signals, index i holds signal after i register stages
  logic                  [0:NUM_INP_REGS][1:0][WIDTH-1:0]       inp_pipe_operands_q;
  fpnew_pkg::roundmode_e [0:NUM_INP_REGS]                       inp_pipe_rnd_mode_q;
  fpnew_pkg::operation_e [0:NUM_INP_REGS]                       inp_pipe_op_q;
  logic                  [0:NUM_INP_REGS]                       inp_pipe_mask_q;

  // Input stage: First element of pipeline is taken from inputs
  assign inp_pipe_operands_q[0] = operands_i;
  assign inp_pipe_rnd_mode_q[0] = rnd_mode_i;
  assign inp_pipe_op_q[0]       = op_i;
  assign inp_pipe_mask_q[0]     = mask_i;

  // Generate the register stages
  for (genvar i = 0; i < NUM_INP_REGS; i++) begin : gen_input_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Enable register is set externally
    assign reg_ena = reg_enable_i[i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(inp_pipe_operands_q[i+1], inp_pipe_operands_q[i], reg_ena, '0)
    `FFL(inp_pipe_rnd_mode_q[i+1], inp_pipe_rnd_mode_q[i], reg_ena, fpnew_pkg::RNE)
    `FFL(inp_pipe_op_q[i+1],       inp_pipe_op_q[i],       reg_ena, fpnew_pkg::FMADD)
    `FFL(inp_pipe_mask_q[i+1],     inp_pipe_mask_q[i],     reg_ena, '0)
  end
  // Output stage: assign selected pipe outputs to signals for later use
  assign operands_q = inp_pipe_operands_q[NUM_INP_REGS];
  assign rnd_mode_q = inp_pipe_rnd_mode_q[NUM_INP_REGS];
  assign op_q       = inp_pipe_op_q[NUM_INP_REGS];

  // -----------------
  // Input processing
  // -----------------
  logic div_op, sqrt_op;        // input signalling with unit
  logic op_starting;            // high in the cycle a new operation starts

  // Operations are gated by the FSM ready. Invalid input ops run a sqrt to not lose illegal instr.
  assign div_op   = (op_q == fpnew_pkg::DIV) & fsm_start_i;  //in_ready delete, valid independent of ready
  assign sqrt_op  = (op_q == fpnew_pkg::SQRT) & fsm_start_i;
  assign op_starting = div_op | sqrt_op;  //start computing or handshake, modify tb handshake right

  logic fdsu_fpu_ex1_stall, fdsu_fpu_ex1_stall_q;
  logic div_op_d, div_op_q;
  logic sqrt_op_d, sqrt_op_q;

  assign div_op_d  = (fdsu_fpu_ex1_stall) ? div_op  : 1'b0;
  assign sqrt_op_d = (fdsu_fpu_ex1_stall) ? sqrt_op : 1'b0;

  `FFL(fdsu_fpu_ex1_stall_q, fdsu_fpu_ex1_stall, 1'b1, '0)
  `FFL(div_op_q, div_op_d, 1'b1, '0)
  `FFL(sqrt_op_q, sqrt_op_d, 1'b1, '0)

  // -----------------
  // DIVSQRT instance
  // -----------------
  logic [WIDTH-1:0]   unit_result;
  fpnew_pkg::status_t unit_status;

  // thead define fdsu module's input and output
  logic        ctrl_fdsu_ex1_sel;
  logic        fdsu_fpu_ex1_cmplt;
  logic  [4:0] fdsu_fpu_ex1_fflags;
  logic  [7:0] fdsu_fpu_ex1_special_sel;
  logic  [3:0] fdsu_fpu_ex1_special_sign;
  logic        fdsu_fpu_no_op;
  logic  [2:0] idu_fpu_ex1_eu_sel;
  logic [31:0] fdsu_frbus_data;
  logic  [4:0] fdsu_frbus_fflags;
  logic        fdsu_frbus_wb_vld;

  // dp
  logic [31:0] dp_frbus_ex2_data;
  logic  [4:0] dp_frbus_ex2_fflags;
  logic  [2:0] dp_xx_ex1_cnan;
  logic  [2:0] dp_xx_ex1_id;
  logic  [2:0] dp_xx_ex1_inf;
  logic  [2:0] dp_xx_ex1_norm;
  logic  [2:0] dp_xx_ex1_qnan;
  logic  [2:0] dp_xx_ex1_snan;
  logic  [2:0] dp_xx_ex1_zero;
  logic        ex2_inst_wb;
  logic        ex2_inst_wb_vld_d, ex2_inst_wb_vld_q;

  // frbus
  logic [31:0] fpu_idu_fwd_data;
  logic  [4:0] fpu_idu_fwd_fflags;
  logic        fpu_idu_fwd_vld;

  logic unit_done;  // status signals from unit instance
  logic unit_ready_d, unit_ready_q;

  // unit_ready_q related to state machine, different under special and normal cases.
  always_comb begin
    if(op_starting && unit_ready_q) begin
      if(ex2_inst_wb && ex2_inst_wb_vld_q) begin
        unit_ready_d = 1'b1;
      end else begin
        unit_ready_d = 1'b0;
      end
    end else if(fpu_idu_fwd_vld | flush_i) begin
      unit_ready_d = 1'b1;
    end else begin
      unit_ready_d = unit_ready_q;
    end
  end

  `FFL(unit_ready_q, unit_ready_d, 1'b1, 1'b1)

  assign fsm_ready_o = unit_ready_q && !fdsu_fpu_ex1_stall;

  // determine input of time to select operands
  always_comb begin
    ctrl_fdsu_ex1_sel = 1'b0;
    idu_fpu_ex1_eu_sel = 3'h0;
    if (op_starting) begin  // time to start calculation
      ctrl_fdsu_ex1_sel = 1'b1;  // time to select operands
      idu_fpu_ex1_eu_sel = 3'h4; // time to select operands, only idu_fpu_ex1_eu_sel_i[2] works in fdsu module
    end else if (fdsu_fpu_ex1_stall_q) begin
      ctrl_fdsu_ex1_sel = 1'b1;  // time to select operands
      idu_fpu_ex1_eu_sel = 3'h4; // time to select operands, only idu_fpu_ex1_eu_sel_i[2] works in fdsu module
    end else begin
      ctrl_fdsu_ex1_sel = 1'b0;
      idu_fpu_ex1_eu_sel = 3'h0;
    end
  end

  pa_fdsu_top i_divsqrt_thead (
   .cp0_fpu_icg_en                ( 1'b0               ),  // input clock gate enable in gated_clk_cell, active 0.
   .cp0_fpu_xx_dqnan              ( 1'b0               ),  // When dqnan = 0, QNAN (0x7fc00000).
   .cp0_yy_clk_en                 ( 1'b1               ),  // clock enable in gated_clk_cell, active 1.
   .cpurst_b                      ( rst_ni             ),  // If negedge cpu reset, all state machines reset to IDLE.
   .ctrl_fdsu_ex1_sel             ( ctrl_fdsu_ex1_sel  ),  // select operands
   .ctrl_xx_ex1_cmplt_dp          ( ctrl_fdsu_ex1_sel  ),  // complete datapath
   .ctrl_xx_ex1_inst_vld          ( ctrl_fdsu_ex1_sel  ),  // instance valid
   .ctrl_xx_ex1_stall             ( fdsu_fpu_ex1_stall ),
   .ctrl_xx_ex1_warm_up           ( 1'b0               ),
   .ctrl_xx_ex2_warm_up           ( 1'b0               ),
   .ctrl_xx_ex3_warm_up           ( 1'b0               ),
   .dp_xx_ex1_cnan                ( dp_xx_ex1_cnan     ),  // Special input type determination
   .dp_xx_ex1_id                  ( dp_xx_ex1_id       ),
   .dp_xx_ex1_inf                 ( dp_xx_ex1_inf      ),
   .dp_xx_ex1_qnan                ( dp_xx_ex1_qnan     ),
   .dp_xx_ex1_rm                  ( rnd_mode_q         ),  // rounding mode
   .dp_xx_ex1_snan                ( dp_xx_ex1_snan     ),
   .dp_xx_ex1_zero                ( dp_xx_ex1_zero     ),
   .fdsu_fpu_debug_info           (                    ),  // output, not used
   .fdsu_fpu_ex1_cmplt            ( fdsu_fpu_ex1_cmplt ),  // output, ctrl_xx_ex1_cmplt_dp && idu_fpu_ex1_eu_sel_i[2]
   .fdsu_fpu_ex1_cmplt_dp         (                    ),  // output, not used
   .fdsu_fpu_ex1_fflags           ( fdsu_fpu_ex1_fflags       ),  // output, special case fflags
   .fdsu_fpu_ex1_special_sel      ( fdsu_fpu_ex1_special_sel  ),  // output, special case type selection
   .fdsu_fpu_ex1_special_sign     ( fdsu_fpu_ex1_special_sign ),  // output, special case sign determination
   .fdsu_fpu_ex1_stall            ( fdsu_fpu_ex1_stall        ),  // output, determine whether stall in ex1
   .fdsu_fpu_no_op                ( fdsu_fpu_no_op            ),  // output, if Write Back SM and fdsu SM no operation, fdsu_fpu_no_op = 1; Otherwise if busy, fdsu_fpu_no_op = 0. (not used)
   .fdsu_frbus_data               ( fdsu_frbus_data           ),  // output, normal case result
   .fdsu_frbus_fflags             ( fdsu_frbus_fflags         ),  // output, normal case fflags
   .fdsu_frbus_freg               (                           ),  // output, determined by input idu_fpu_ex1_dst_freg
   .fdsu_frbus_wb_vld             ( fdsu_frbus_wb_vld         ),  // output, determine whether write back valid
   .forever_cpuclk                ( clk_i                     ),
   .frbus_fdsu_wb_grant           ( fdsu_frbus_wb_vld         ),  // input is fdsu_frbus_wb_vld
   .idu_fpu_ex1_dst_freg          ( 5'h0f                     ),  // register index to write back (not used)
   .idu_fpu_ex1_eu_sel            ( idu_fpu_ex1_eu_sel        ),  // time to select operands
   .idu_fpu_ex1_func              ( {8'b0, div_op | div_op_q, sqrt_op | sqrt_op_q} ),
   .idu_fpu_ex1_srcf0             ( operands_q[0][31:0]       ),  // the first operand
   .idu_fpu_ex1_srcf1             ( operands_q[1][31:0]       ),  // the second operand
   .pad_yy_icg_scan_en            ( 1'b0                      ),  // input of core_top, set to 1'b0 from the beginning to end
   .rtu_xx_ex1_cancel             ( 1'b0                      ),
   .rtu_xx_ex2_cancel             ( 1'b0                      ),
   .rtu_yy_xx_async_flush         ( flush_i                   ),
   .rtu_yy_xx_flush               ( 1'b0                      )
  );

  pa_fpu_dp  x_pa_fpu_dp (
    .cp0_fpu_icg_en              ( 1'b0                       ),
    .cp0_fpu_xx_rm               ( rnd_mode_q                 ),
    .cp0_yy_clk_en               ( 1'b1                       ),
    .ctrl_xx_ex1_inst_vld        ( ctrl_fdsu_ex1_sel          ),
    .ctrl_xx_ex1_stall           ( 1'b0                       ),
    .ctrl_xx_ex1_warm_up         ( 1'b0                       ),
    .dp_frbus_ex2_data           ( dp_frbus_ex2_data          ),  // output
    .dp_frbus_ex2_fflags         ( dp_frbus_ex2_fflags        ),  // output
    .dp_xx_ex1_cnan              ( dp_xx_ex1_cnan             ),  // output
    .dp_xx_ex1_id                ( dp_xx_ex1_id               ),  // output
    .dp_xx_ex1_inf               ( dp_xx_ex1_inf              ),  // output
    .dp_xx_ex1_norm              ( dp_xx_ex1_norm             ),  // output
    .dp_xx_ex1_qnan              ( dp_xx_ex1_qnan             ),  // output
    .dp_xx_ex1_snan              ( dp_xx_ex1_snan             ),  // output
    .dp_xx_ex1_zero              ( dp_xx_ex1_zero             ),  // output
    .ex2_inst_wb                 ( ex2_inst_wb                ),  // output
    .fdsu_fpu_ex1_fflags         ( fdsu_fpu_ex1_fflags        ),
    .fdsu_fpu_ex1_special_sel    ( fdsu_fpu_ex1_special_sel   ),
    .fdsu_fpu_ex1_special_sign   ( fdsu_fpu_ex1_special_sign  ),
    .forever_cpuclk              ( clk_i                      ),
    .idu_fpu_ex1_eu_sel          ( idu_fpu_ex1_eu_sel         ),
    .idu_fpu_ex1_func            ( {8'b0, div_op, sqrt_op}    ),
    .idu_fpu_ex1_gateclk_vld     ( fdsu_fpu_ex1_cmplt         ),
    .idu_fpu_ex1_rm              ( rnd_mode_q                 ),
    .idu_fpu_ex1_srcf0           ( operands_q[0][31:0]        ),
    .idu_fpu_ex1_srcf1           ( operands_q[1][31:0]        ),
    .idu_fpu_ex1_srcf2           ( '0                         ),
    .pad_yy_icg_scan_en          ( 1'b0                       )
  );

  assign ex2_inst_wb_vld_d = ctrl_fdsu_ex1_sel;
  `FF(ex2_inst_wb_vld_q, ex2_inst_wb_vld_d, '0)

  pa_fpu_frbus x_pa_fpu_frbus (
    .ctrl_frbus_ex2_wb_req     ( ex2_inst_wb & ex2_inst_wb_vld_q ),
    .dp_frbus_ex2_data         ( dp_frbus_ex2_data   ),
    .dp_frbus_ex2_fflags       ( dp_frbus_ex2_fflags ),
    .fdsu_frbus_data           ( fdsu_frbus_data     ),
    .fdsu_frbus_fflags         ( fdsu_frbus_fflags   ),
    .fdsu_frbus_wb_vld         ( fdsu_frbus_wb_vld   ),
    .fpu_idu_fwd_data          ( fpu_idu_fwd_data    ),  // output
    .fpu_idu_fwd_fflags        ( fpu_idu_fwd_fflags  ),  // output
    .fpu_idu_fwd_vld           ( fpu_idu_fwd_vld     )   // output
  );

  always_comb begin
    unit_result[31:0] = fpu_idu_fwd_data[31:0];
    unit_status[4:0]  = fpu_idu_fwd_fflags[4:0];
    unit_done         = fpu_idu_fwd_vld;
  end

  // ----------------
  // Hold Result
  // ----------------

  // Hold additional information while the operation is in progress
  logic [WIDTH-1:0]   held_result, out_result;
  fpnew_pkg::status_t held_status, out_status;
  logic               out_mask;

  // Fill the registers everytime a valid operation arrives (load FF, active low asynch rst)
  // The Hold register (load, no reset)
  `FFL(held_result, unit_result, unit_done, '0);
  `FFL(held_status, unit_status, unit_done, '0);
  `FFL(out_mask, inp_pipe_mask_q[NUM_INP_REGS], op_starting, '0)

  assign out_result = unit_done ? unit_result : held_result;
  assign out_status = unit_done ? unit_status : held_status;

  // ----------------
  // Output Pipeline
  // ----------------
  // Output pipeline signals, index i holds signal after i register stages
  logic               [0:NUM_OUT_REGS][WIDTH-1:0] out_pipe_result_q;
  fpnew_pkg::status_t [0:NUM_OUT_REGS]            out_pipe_status_q;
  logic               [0:NUM_OUT_REGS]            out_pipe_mask_q;

  // Input stage: First element of pipeline is taken from inputs
  assign out_pipe_result_q[0] = out_result;
  assign out_pipe_status_q[0] = out_status;
  assign out_pipe_mask_q[0]   = out_mask;

  // Generate the register stages
  for (genvar i = 0; i < NUM_OUT_REGS; i++) begin : gen_output_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Enable register is set externally
    assign reg_ena = reg_enable_i[NUM_INP_REGS + i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(out_pipe_result_q[i+1], out_pipe_result_q[i], reg_ena, '0)
    `FFL(out_pipe_status_q[i+1], out_pipe_status_q[i], reg_ena, '0)
    `FFL(out_pipe_mask_q[i+1],   out_pipe_mask_q[i],   reg_ena, '0)
  end

  // Output stage: assign module outputs
  assign result_o        = out_pipe_result_q[NUM_OUT_REGS];
  assign status_o        = out_pipe_status_q[NUM_OUT_REGS];
  assign extension_bit_o = 1'b1; // always NaN-Box result
  assign mask_o          = out_pipe_mask_q[NUM_OUT_REGS];

endmodule
