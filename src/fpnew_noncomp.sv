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

module fpnew_noncomp #(
  parameter fpnew_pkg::fp_format_e   FpFormat    = fpnew_pkg::FP32,
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,

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
  input TagType                    tag_i,
  input AuxType                    aux_i,
  // Input Handshake
  input  logic                     in_valid_i,
  output logic                     in_ready_o,
  input  logic                     flush_i,
  // Output signals
  output logic [WIDTH-1:0]         result_o,
  output fpnew_pkg::status_t       status_o,
  output logic                     extension_bit_o,
  output fpnew_pkg::classmask_e    class_mask_o,
  output logic                     is_class_o,
  output TagType                   tag_o,
  output AuxType                   aux_o,
  // Output handshake
  output logic                     out_valid_o,
  input  logic                     out_ready_i,
  // Indication of valid data in flight
  output logic                     busy_o
);

  // ----------
  // Constants
  // ----------
  localparam int unsigned EXP_BITS = fpnew_pkg::exp_bits(FpFormat);
  localparam int unsigned MAN_BITS = fpnew_pkg::man_bits(FpFormat);

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
  // Pipelined input signals
  logic [1:0][WIDTH-1:0]     operands_q;
  logic [1:0]                is_boxed_q;
  fpnew_pkg::roundmode_e     rnd_mode_q;
  fpnew_pkg::operation_e     op_q;
  logic                      op_mod_q;

  // Generate pipeline at input if needed
  if (PipeConfig==fpnew_pkg::BEFORE) begin : input_pipeline
    fpnew_pipe_in #(
      .Width       ( WIDTH       ),
      .NumPipeRegs ( NumPipeRegs ),
      .NumOperands ( 2           ),
      .TagType     ( TagType     )
    ) i_input_pipe (
      .clk_i,
      .rst_ni,
      .operands_i,
      .is_boxed_i,
      .rnd_mode_i,
      .op_i,
      .op_mod_i,
      .src_fmt_i      ( fpnew_pkg::FP32 ), // unused
      .dst_fmt_i      ( fpnew_pkg::FP32 ), // unused
      .int_fmt_i      ( fpnew_pkg::INT8 ), // unused
      .tag_i,
      .aux_i,
      .in_valid_i,
      .in_ready_o,
      .flush_i,
      .operands_o     ( operands_q   ),
      .is_boxed_o     ( is_boxed_q   ),
      .rnd_mode_o     ( rnd_mode_q   ),
      .op_o           ( op_q         ),
      .op_mod_o       ( op_mod_q     ),
      .src_fmt_o      ( /* unused */ ),
      .dst_fmt_o      ( /* unused */ ),
      .int_fmt_o      ( /* unused */ ),
      .tag_o,
      .aux_o,
      .out_valid_o,
      .out_ready_i,
      .busy_o
    );
  // Otherwise pass through inputs
  end else begin : no_input_pipeline
    assign operands_q     = operands_i;
    assign is_boxed_q     = is_boxed_i;
    assign rnd_mode_q     = rnd_mode_i;
    assign op_q           = op_i;
    assign op_mod_q       = op_mod_i;
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
    .operands_i ( operands_q ),
    .is_boxed_i ( is_boxed_q ),
    .info_o     ( info_q     )
  );

  fp_t                 operand_a, operand_b;
  fpnew_pkg::fp_info_t info_a,    info_b;

   // Packing-order-agnostic assignments
  assign operand_a = operands_q[0];
  assign operand_b = operands_q[1];
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
    unique case (rnd_mode_q)
      fpnew_pkg::RNE: sgnj_result.sign = sign_b;          // SGNJ
      fpnew_pkg::RTZ: sgnj_result.sign = ~sign_b;         // SGNJN
      fpnew_pkg::RDN: sgnj_result.sign = sign_a ^ sign_b; // SGNJX
      fpnew_pkg::RUP: sgnj_result      = operand_a;       // passthrough
      default: sgnj_result = 'X; // propagate X
    endcase
  end

  assign sgnj_status = '0;        // sign injections never raise exceptions

  // op_mod_q enables sign-extension of result (for storing to integer regfile)
  assign sgnj_extension_bit = op_mod_q ? sgnj_result.sign : 1'b1; // NaN-box regular float results

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
      unique case (rnd_mode_q)
        fpnew_pkg::RNE: minmax_result = operand_a_smaller ? operand_a : operand_b; // MIN
        fpnew_pkg::RTZ: minmax_result = operand_a_smaller ? operand_b : operand_a; // MAX
        default: minmax_result = 'X; // propagate X
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

    // Signalling NaNs always compare as false and are illegal
    if (signalling_nan) cmp_status.NV = 1'b1; // invalid operation
    // Otherwise do comparisons
    else begin
      unique case (rnd_mode_q)
        fpnew_pkg::RNE: begin // Less than or equal
          if (any_operand_nan) cmp_status.NV = 1'b1; // Signalling comparison: NaNs are invalid
          else cmp_result = (operand_a_smaller | operands_equal) ^ op_mod_q;
        end
        fpnew_pkg::RTZ: begin // Less than
          if (any_operand_nan) cmp_status.NV = 1'b1; // Signalling comparison: NaNs are invalid
          else cmp_result = (operand_a_smaller & ~operands_equal) ^ op_mod_q; // -0 = +0, not less
        end
        fpnew_pkg::RDN: begin // Equal
          if (any_operand_nan) cmp_result = op_mod_q; // NaNs are valid, always campare as not equal
          else cmp_result = operands_equal ^ op_mod_q;
        end
        default: cmp_result = 'X; // propagate X
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
    unique case (op_q)
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
        result_d        = 'X; // unused
        status_d        = class_status;
        extension_bit_d = class_extension_bit;
      end
      default: begin
        result_d        = 'X; // propaagate X
        status_d        = 'X; // propaagate X
        extension_bit_d = 'X; // propaagate X
      end
    endcase
  end

  assign is_class_d = (op_q == fpnew_pkg::CLASSIFY);

  // ----------------
  // Output Pipeline
  // ----------------
  // Generate pipeline at output if needed
  if (PipeConfig==fpnew_pkg::AFTER) begin : output_pipline
    fpnew_pipe_out #(
      .Width       ( WIDTH       ),
      .NumPipeRegs ( NumPipeRegs ),
      .TagType     ( TagType     )
    ) i_output_pipe (
      .clk_i,
      .rst_ni,
      .result_i        ( result_d        ),
      .status_i        ( status_d        ),
      .extension_bit_i ( extension_bit_d ),
      .class_mask_i    ( class_mask_d    ),
      .is_class_i      ( is_class_d      ),
      .tag_i,
      .aux_i,
      .in_valid_i,
      .in_ready_o,
      .flush_i,
      .result_o,
      .status_o,
      .extension_bit_o,
      .class_mask_o,
      .is_class_o,
      .tag_o,
      .aux_o,
      .out_valid_o,
      .out_ready_i,
      .busy_o
    );
  // Otherwise pass through outputs
  end else begin : no_output_pipeline
    assign result_o        = result_d;
    assign status_o        = status_d;
    assign extension_bit_o = extension_bit_d;
    assign class_mask_o    = class_mask_d;
    assign is_class_o      = is_class_d;
  end

endmodule
