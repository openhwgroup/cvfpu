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

// Author: Stefan Mach <smach@iis.ee.ethz.ch>

`include "common_cells/registers.svh"

module fpnew_noncomp #(
  parameter fpnew_pkg::fp_format_e   FpFormat    = fpnew_pkg::fp_format_e'(0),
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,

  localparam int unsigned WIDTH = fpnew_pkg::fp_width(FpFormat) // do not change
) (
  input logic                  clk_i,
  input logic                  rst_ni,
  // Input signals
  input logic [1:0][WIDTH-1:0]     operands_i, // 2 operands
  input logic [1:0]                is_boxed_i, // 2 operands
  input fpnew_pkg::roundmode_e     rnd_mode_i,
  input fpnew_pkg::operation_e     op_i,
  input logic                      op_mod_i,
  input logic                      mask_i,
  // Output signals
  output logic [WIDTH-1:0]         result_o,
  output fpnew_pkg::status_t       status_o,
  output logic                     extension_bit_o,
  output fpnew_pkg::classmask_e    class_mask_o,
  output logic                     is_class_o,
  output logic                     mask_o,
  // External Register Control
  input logic[NumPipeRegs-1:0]     reg_enable_i
);

  // ----------
  // Constants
  // ----------
  localparam int unsigned EXP_BITS = fpnew_pkg::exp_bits(FpFormat);
  localparam int unsigned MAN_BITS = fpnew_pkg::man_bits(FpFormat);
  // Pipelines
  localparam NUM_INP_REGS = (PipeConfig == fpnew_pkg::BEFORE || PipeConfig == fpnew_pkg::INSIDE)
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? ((NumPipeRegs + 1) / 2) // First to get distributed regs
                               : 0); // no regs here otherwise
  localparam NUM_OUT_REGS = PipeConfig == fpnew_pkg::AFTER
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? (NumPipeRegs / 2) // Last to get distributed regs
                               : 0); // no regs here otherwise

  // ----------------
  // Type definition
  // ----------------
  typedef struct packed {
    logic                sign;
    logic [EXP_BITS-1:0] exponent;
    logic [MAN_BITS-1:0] mantissa;
  } fp_t;

  // ---------------
  // Input pipeline
  // ---------------
  // Input pipeline signals, index i holds signal after i register stages
  logic                  [0:NUM_INP_REGS][1:0][WIDTH-1:0] inp_pipe_operands_q;
  logic                  [0:NUM_INP_REGS][1:0]            inp_pipe_is_boxed_q;
  fpnew_pkg::roundmode_e [0:NUM_INP_REGS]                 inp_pipe_rnd_mode_q;
  fpnew_pkg::operation_e [0:NUM_INP_REGS]                 inp_pipe_op_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_op_mod_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_mask_q;

  // Input stage: First element of pipeline is taken from inputs
  assign inp_pipe_operands_q[0] = operands_i;
  assign inp_pipe_is_boxed_q[0] = is_boxed_i;
  assign inp_pipe_rnd_mode_q[0] = rnd_mode_i;
  assign inp_pipe_op_q[0]       = op_i;
  assign inp_pipe_op_mod_q[0]   = op_mod_i;
  assign inp_pipe_mask_q[0]     = mask_i;

  // Generate the register stages
  for (genvar i = 0; i < NUM_INP_REGS; i++) begin : gen_input_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Enable register is set externally
    assign reg_ena = reg_enable_i[i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(inp_pipe_operands_q[i+1], inp_pipe_operands_q[i], reg_ena, '0)
    `FFL(inp_pipe_is_boxed_q[i+1], inp_pipe_is_boxed_q[i], reg_ena, '0)
    `FFL(inp_pipe_rnd_mode_q[i+1], inp_pipe_rnd_mode_q[i], reg_ena, fpnew_pkg::RNE)
    `FFL(inp_pipe_op_q[i+1],       inp_pipe_op_q[i],       reg_ena, fpnew_pkg::FMADD)
    `FFL(inp_pipe_op_mod_q[i+1],   inp_pipe_op_mod_q[i],   reg_ena, '0)
    `FFL(inp_pipe_mask_q[i+1],     inp_pipe_mask_q[i],     reg_ena, '0)
  end

  // ---------------------
  // Input classification
  // ---------------------
  fpnew_pkg::fp_info_t [1:0] info_q;

  // Classify input
  fpnew_classifier #(
    .FpFormat    ( FpFormat ),
    .NumOperands ( 2        )
    ) i_class_a (
    .operands_i ( inp_pipe_operands_q[NUM_INP_REGS] ),
    .is_boxed_i ( inp_pipe_is_boxed_q[NUM_INP_REGS] ),
    .info_o     ( info_q                            )
  );

  fp_t                 operand_a, operand_b;
  fpnew_pkg::fp_info_t info_a,    info_b;

  // Packing-order-agnostic assignments
  assign operand_a = inp_pipe_operands_q[NUM_INP_REGS][0];
  assign operand_b = inp_pipe_operands_q[NUM_INP_REGS][1];
  assign info_a    = info_q[0];
  assign info_b    = info_q[1];

  logic any_operand_inf;
  logic any_operand_nan;
  logic signalling_nan;

  // Reduction for special case handling
  assign any_operand_inf = (| {info_a.is_inf,        info_b.is_inf});
  assign any_operand_nan = (| {info_a.is_nan,        info_b.is_nan});
  assign signalling_nan  = (| {info_a.is_signalling, info_b.is_signalling});

  logic operands_equal, operand_a_smaller;

  // Equality checks for zeroes too
  assign operands_equal    = (operand_a == operand_b) || (info_a.is_zero && info_b.is_zero);
  // Invert result if non-zero signs involved (unsigned comparison)
  assign operand_a_smaller = (operand_a < operand_b) ^ (operand_a.sign || operand_b.sign);

  // ---------------
  // Sign Injection
  // ---------------
  fp_t                sgnj_result;
  fpnew_pkg::status_t sgnj_status;
  logic               sgnj_extension_bit;

  // Sign Injection - operation is encoded in rnd_mode_q:
  // RNE = SGNJ, RTZ = SGNJN, RDN = SGNJX, RUP = Passthrough (no NaN-box check)
  always_comb begin : sign_injections
    logic sign_a, sign_b; // internal signs
    // Default assignment
    sgnj_result = operand_a; // result based on operand a

    // NaN-boxing check will treat invalid inputs as canonical NaNs
    if (!info_a.is_boxed) sgnj_result = '{sign: 1'b0, exponent: '1, mantissa: 2**(MAN_BITS-1)};

    // Internal signs are treated as positive in case of non-NaN-boxed values
    sign_a = operand_a.sign & info_a.is_boxed;
    sign_b = operand_b.sign & info_b.is_boxed;

    // Do the sign injection based on rm field
    unique case (inp_pipe_rnd_mode_q[NUM_INP_REGS])
      fpnew_pkg::RNE: sgnj_result.sign = sign_b;          // SGNJ
      fpnew_pkg::RTZ: sgnj_result.sign = ~sign_b;         // SGNJN
      fpnew_pkg::RDN: sgnj_result.sign = sign_a ^ sign_b; // SGNJX
      fpnew_pkg::RUP: sgnj_result      = operand_a;       // passthrough
      default: sgnj_result = '{default: fpnew_pkg::DONT_CARE}; // don't care
    endcase
  end

  assign sgnj_status = '0;        // sign injections never raise exceptions

  // op_mod_q enables integer sign-extension of result (for storing to integer regfile)
  assign sgnj_extension_bit = inp_pipe_op_mod_q[NUM_INP_REGS] ? sgnj_result.sign : 1'b1;

  // ------------------
  // Minimum / Maximum
  // ------------------
  fp_t                minmax_result;
  fpnew_pkg::status_t minmax_status;
  logic               minmax_extension_bit;

  // Minimum/Maximum - operation is encoded in rnd_mode_q:
  // RNE = MIN, RTZ = MAX
  always_comb begin : min_max
    // Default assignment
    minmax_status = '0;

    // Min/Max use quiet comparisons - only sNaN are invalid
    minmax_status.NV = signalling_nan;

    // Both NaN inputs cause a NaN output
    if (info_a.is_nan && info_b.is_nan)
      minmax_result = '{sign: 1'b0, exponent: '1, mantissa: 2**(MAN_BITS-1)}; // canonical qNaN
    // If one operand is NaN, the non-NaN operand is returned
    else if (info_a.is_nan) minmax_result = operand_b;
    else if (info_b.is_nan) minmax_result = operand_a;
    // Otherwise decide according to the operation
    else begin
      unique case (inp_pipe_rnd_mode_q[NUM_INP_REGS])
        fpnew_pkg::RNE: minmax_result = operand_a_smaller ? operand_a : operand_b; // MIN
        fpnew_pkg::RTZ: minmax_result = operand_a_smaller ? operand_b : operand_a; // MAX
        default: minmax_result = '{default: fpnew_pkg::DONT_CARE}; // don't care
      endcase
    end
  end

  assign minmax_extension_bit = 1'b1; // NaN-box as result is always a float value

  // ------------
  // Comparisons
  // ------------
  fp_t                cmp_result;
  fpnew_pkg::status_t cmp_status;
  logic               cmp_extension_bit;

  // Comparisons - operation is encoded in rnd_mode_q:
  // RNE = LE, RTZ = LT, RDN = EQ
  // op_mod_q inverts boolean outputs
  always_comb begin : comparisons
    // Default assignment
    cmp_result = '0; // false
    cmp_status = '0; // no flags

    // Signalling NaNs always compare as false (except for "not equal" compares) and are illegal
    if (signalling_nan) begin
      cmp_status.NV = 1'b1; // invalid operation
      cmp_result    = inp_pipe_rnd_mode_q[NUM_INP_REGS] == fpnew_pkg::RDN && inp_pipe_op_mod_q[NUM_INP_REGS];
    // Otherwise do comparisons
    end else begin
      unique case (inp_pipe_rnd_mode_q[NUM_INP_REGS])
        fpnew_pkg::RNE: begin // Less than or equal
          if (any_operand_nan) cmp_status.NV = 1'b1; // Signalling comparison: NaNs are invalid
          else cmp_result = (operand_a_smaller | operands_equal) ^ inp_pipe_op_mod_q[NUM_INP_REGS];
        end
        fpnew_pkg::RTZ: begin // Less than
          if (any_operand_nan) cmp_status.NV = 1'b1; // Signalling comparison: NaNs are invalid
          else cmp_result = (operand_a_smaller & ~operands_equal) ^ inp_pipe_op_mod_q[NUM_INP_REGS];
        end
        fpnew_pkg::RDN: begin // Equal
          if (any_operand_nan) cmp_result = inp_pipe_op_mod_q[NUM_INP_REGS]; // NaN always not equal
          else cmp_result = operands_equal ^ inp_pipe_op_mod_q[NUM_INP_REGS];
        end
        default: cmp_result = '{default: fpnew_pkg::DONT_CARE}; // don't care
      endcase
    end
  end

  assign cmp_extension_bit = 1'b0; // Comparisons always produce booleans in integer registers

  // ---------------
  // Classification
  // ---------------
  fpnew_pkg::status_t    class_status;
  logic                  class_extension_bit;
  fpnew_pkg::classmask_e class_mask_d; // the result is actually here

  // Classification - always return the classification mask on the dedicated port
  always_comb begin : classify
    if (info_a.is_normal) begin
      class_mask_d = operand_a.sign       ? fpnew_pkg::NEGNORM    : fpnew_pkg::POSNORM;
    end else if (info_a.is_subnormal) begin
      class_mask_d = operand_a.sign       ? fpnew_pkg::NEGSUBNORM : fpnew_pkg::POSSUBNORM;
    end else if (info_a.is_zero) begin
      class_mask_d = operand_a.sign       ? fpnew_pkg::NEGZERO    : fpnew_pkg::POSZERO;
    end else if (info_a.is_inf) begin
      class_mask_d = operand_a.sign       ? fpnew_pkg::NEGINF     : fpnew_pkg::POSINF;
    end else if (info_a.is_nan) begin
      class_mask_d = info_a.is_signalling ? fpnew_pkg::SNAN       : fpnew_pkg::QNAN;
    end else begin
      class_mask_d = fpnew_pkg::QNAN; // default value
    end
  end

  assign class_status        = '0;   // classification does not set flags
  assign class_extension_bit = 1'b0; // classification always produces results in integer registers

  // -----------------
  // Result selection
  // -----------------
  fp_t                   result_d;
  fpnew_pkg::status_t    status_d;
  logic                  extension_bit_d;
  logic                  is_class_d;

  // Select result
  always_comb begin : select_result
    unique case (inp_pipe_op_q[NUM_INP_REGS])
      fpnew_pkg::SGNJ: begin
        result_d        = sgnj_result;
        status_d        = sgnj_status;
        extension_bit_d = sgnj_extension_bit;
      end
      fpnew_pkg::MINMAX: begin
        result_d        = minmax_result;
        status_d        = minmax_status;
        extension_bit_d = minmax_extension_bit;
      end
      fpnew_pkg::CMP: begin
        result_d        = cmp_result;
        status_d        = cmp_status;
        extension_bit_d = cmp_extension_bit;
      end
      fpnew_pkg::CLASSIFY: begin
        result_d        = '{default: fpnew_pkg::DONT_CARE}; // unused
        status_d        = class_status;
        extension_bit_d = class_extension_bit;
      end
      default: begin
        result_d        = '{default: fpnew_pkg::DONT_CARE}; // dont care
        status_d        = '{default: fpnew_pkg::DONT_CARE}; // dont care
        extension_bit_d = fpnew_pkg::DONT_CARE;             // dont care
      end
    endcase
  end

  assign is_class_d = (inp_pipe_op_q[NUM_INP_REGS] == fpnew_pkg::CLASSIFY);

  // ----------------
  // Output Pipeline
  // ----------------
  // Output pipeline signals, index i holds signal after i register stages
  fp_t                   [0:NUM_OUT_REGS] out_pipe_result_q;
  fpnew_pkg::status_t    [0:NUM_OUT_REGS] out_pipe_status_q;
  logic                  [0:NUM_OUT_REGS] out_pipe_extension_bit_q;
  fpnew_pkg::classmask_e [0:NUM_OUT_REGS] out_pipe_class_mask_q;
  logic                  [0:NUM_OUT_REGS] out_pipe_is_class_q;
  logic                  [0:NUM_OUT_REGS] out_pipe_mask_q;

  // Input stage: First element of pipeline is taken from inputs
  assign out_pipe_result_q[0]        = result_d;
  assign out_pipe_status_q[0]        = status_d;
  assign out_pipe_extension_bit_q[0] = extension_bit_d;
  assign out_pipe_class_mask_q[0]    = class_mask_d;
  assign out_pipe_is_class_q[0]      = is_class_d;
  assign out_pipe_mask_q[0]          = inp_pipe_mask_q[NUM_INP_REGS];

  // Generate the register stages
  for (genvar i = 0; i < NUM_OUT_REGS; i++) begin : gen_output_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Enable register is set externally
    assign reg_ena = reg_enable_i[NUM_INP_REGS + i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(out_pipe_result_q[i+1],        out_pipe_result_q[i],        reg_ena, '0)
    `FFL(out_pipe_status_q[i+1],        out_pipe_status_q[i],        reg_ena, '0)
    `FFL(out_pipe_extension_bit_q[i+1], out_pipe_extension_bit_q[i], reg_ena, '0)
    `FFL(out_pipe_class_mask_q[i+1],    out_pipe_class_mask_q[i],    reg_ena, fpnew_pkg::QNAN)
    `FFL(out_pipe_is_class_q[i+1],      out_pipe_is_class_q[i],      reg_ena, '0)
    `FFL(out_pipe_mask_q[i+1],          out_pipe_mask_q[i],          reg_ena, '0)
  end

  // Output stage: assign module outputs
  assign result_o        = out_pipe_result_q[NUM_OUT_REGS];
  assign status_o        = out_pipe_status_q[NUM_OUT_REGS];
  assign extension_bit_o = out_pipe_extension_bit_q[NUM_OUT_REGS];
  assign class_mask_o    = out_pipe_class_mask_q[NUM_OUT_REGS];
  assign is_class_o      = out_pipe_is_class_q[NUM_OUT_REGS];
  assign mask_o          = out_pipe_mask_q[NUM_OUT_REGS];
endmodule
