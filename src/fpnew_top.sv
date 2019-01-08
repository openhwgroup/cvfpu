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

module fpnew_top #(
  // FPU configuration
  parameter fpnew_pkg::fpu_features_t       Features      = fpnew_pkg::RV64D_Xsflt,
  parameter fpnew_pkg::fpu_implementation_t Implementaion = fpnew_pkg::DEFAULT_NOREGS,
  parameter type                            TagType       = logic,
  // Do not change
  localparam int unsigned WIDTH        = Features.Width,
  localparam int unsigned NUM_OPERANDS = 3
) (
  input logic                               clk_i,
  input logic                               rst_ni,
  // Input signals
  input logic [0:NUM_OPERANDS-1][WIDTH-1:0] operands_i,
  input fpnew_pkg::roundmode_e              rnd_mode_i,
  input fpnew_pkg::operation_e              op_i,
  input logic                               op_mod_i,
  input fpnew_pkg::fp_format_e              fp_fmt_i,
  input fpnew_pkg::fp_format_e              fp_fmt2_i,
  input fpnew_pkg::int_format_e             int_fmt_i,
  input logic                               vectorial_op_i,
  input TagType                             tag_i,
  // Input Handshake
  input  logic                              in_valid_i,
  output logic                              in_ready_o,
  input  logic                              flush_i,
  // Output signals
  output logic [WIDTH-1:0]                  result_o,
  output fpnew_pkg::status_t                status_o,
  output TagType                            tag_o,
  // Output handshake
  output logic                              out_valid_o,
  input  logic                              out_ready_i,
  // Indication of valid data in flight
  output logic                              busy_o
);

  localparam int unsigned NUM_OPGROUPS = fpnew_pkg::NUM_OPGROUPS;
  localparam int unsigned NUM_FORMATS  = fpnew_pkg::NUM_FP_FORMATS;

  // ----------------
  // Type Definition
  // ----------------
  typedef struct packed {
    logic [WIDTH-1:0]   result;
    fpnew_pkg::status_t status;
    TagType             tag;
  } output_t;

  // Handshake signals for the blocks
  logic [0:NUM_OPGROUPS-1] opgrp_in_ready, opgrp_out_valid, opgrp_out_ready, opgrp_ext, opgrp_busy;
  output_t [0:NUM_OPGROUPS-1] opgrp_outputs;

  logic [0:NUM_FORMATS-1][0:NUM_OPERANDS-1] is_boxed;

  // -----------
  // Input Side
  // -----------
  assign in_ready_o = in_valid_i & opgrp_in_ready[fpnew_pkg::get_opgroup(op_i)];

  // NaN-boxing check
  for (genvar fmt = 0; fmt < int'(NUM_FORMATS); fmt++) begin : gen_nanbox_check
    localparam int unsigned FP_WIDTH = fpnew_pkg::fp_width(fpnew_pkg::fp_format_e'(fmt));
    // NaN boxing is only generated if it's enabled and needed
    if (Features.EnableNanBox && (FP_WIDTH < WIDTH)) begin : check
      for (genvar op = 0; op < int'(NUM_OPERANDS); op++) begin : operands
        assign is_boxed[fmt][op] = (!vectorial_op_i)
                                   ? operands_i[op][WIDTH-1:FP_WIDTH] == '1
                                   : 1'b1;
      end
    end else begin : no_check
      assign is_boxed[fmt] = '1;
    end
  end

  // -------------------------
  // Generate Operation Blocks
  // -------------------------
  for (genvar opgrp = 0; opgrp < int'(NUM_OPGROUPS); opgrp++) begin : gen_operation_groups
    localparam int unsigned NUM_OPS = fpnew_pkg::num_operands(fpnew_pkg::opgroup_e'(opgrp));

    logic in_valid;
    logic [0:NUM_FORMATS-1][0:NUM_OPS-1] input_boxed;

    assign in_valid = in_valid_i & (fpnew_pkg::get_opgroup(op_i) == fpnew_pkg::opgroup_e'(opgrp));

    // slice out input boxing
    always_comb begin : slice_inputs
      for (int unsigned fmt = 0; fmt < NUM_FORMATS; fmt++)
        input_boxed[fmt] = is_boxed[fmt][0:NUM_OPS-1];
    end

    fpnew_opgroup_block #(
      .OpGroup       ( fpnew_pkg::opgroup_e'(opgrp)   ),
      .Width         ( WIDTH                          ),
      .EnableVectors ( Features.EnableVectors         ),
      .FpFmtMask     ( Features.FpFmtMask             ),
      .IntFmtMask    ( Features.IntFmtMask            ),
      .FmtPipeRegs   ( Implementaion.PipeRegs[opgrp]  ),
      .FmtUnitTypes  ( Implementaion.UnitTypes[opgrp] ),
      .PipeConfig    ( Implementaion.PipeConfig       ),
      .TagType       ( TagType                        )
    ) i_opgroup_block (
      .clk_i,
      .rst_ni,
      .operands_i      ( operands_i[0:NUM_OPS-1] ),
      .is_boxed_i      ( input_boxed             ),
      .rnd_mode_i,
      .op_i,
      .op_mod_i,
      .fp_fmt_i,
      .fp_fmt2_i,
      .int_fmt_i,
      .vectorial_op_i,
      .tag_i,
      .in_valid_i      ( in_valid              ),
      .in_ready_o      ( opgrp_in_ready[opgrp] ),
      .flush_i,
      .result_o        ( opgrp_outputs[opgrp].result ),
      .status_o        ( opgrp_outputs[opgrp].status ),
      .extension_bit_o ( opgrp_ext[opgrp]            ),
      .tag_o           ( opgrp_outputs[opgrp].tag    ),
      .out_valid_o     ( opgrp_out_valid[opgrp]      ),
      .out_ready_i     ( opgrp_out_ready[opgrp]      ),
      .busy_o          ( opgrp_busy[opgrp]           )
    );
  end

  // ------------------
  // Arbitrate Outputs
  // ------------------
  output_t arbiter_output;

  // Round-Robin arbiter to decide which result to use
  stream_arbiter_flushable #(
    .DATA_T ( output_t     ),
    .N_INP  ( NUM_OPGROUPS )
  ) i_arbiter (
    .clk_i,
    .rst_ni,
    .flush_i,
    .inp_data_i  ( opgrp_outputs    ),
    .inp_valid_i ( opgrp_out_valid  ),
    .inp_ready_o ( opgrp_out_ready  ),
    .oup_data_o  ( arbiter_output   ),
    .oup_valid_o ( out_valid_o      ),
    .oup_ready_i ( out_ready_i      )
  );

  // Unpack output
  assign result_o        = arbiter_output.result;
  assign status_o        = arbiter_output.status;
  assign tag_o           = arbiter_output.tag;

  assign busy_o = (| opgrp_busy);

endmodule
