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

module fpnew_f2fcast #(
  parameter fpnew_pkg::fp_format_e   SrcFpFormat = fpnew_pkg::FP32,
  parameter fpnew_pkg::fp_format_e   DstFpFormat = fpnew_pkg::FP32,
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,
  // Do not change
  localparam int unsigned SRC_WIDTH = fpnew_pkg::fp_width(SrcFpFormat),
  localparam int unsigned DST_WIDTH = fpnew_pkg::fp_width(DstFpFormat)
) (
  input  logic                  clk_i,
  input  logic                  rst_ni,
  // Input signals
  input  logic [SRC_WIDTH-1:0]  operands_i, // 1 operand
  input  logic                  is_boxed_i, // 1 operand
  input  fpnew_pkg::roundmode_e rnd_mode_i,
  input  TagType                tag_i,
  input  AuxType                aux_i,
  // Input Handshake
  input  logic                  in_valid_i,
  output logic                  in_ready_o,
  input  logic                  flush_i,
  // Output signals
  output logic [DST_WIDTH-1:0]  result_o,
  output fpnew_pkg::status_t    status_o,
  output logic                  extension_bit_o,
  output TagType                tag_o,
  output AuxType                aux_o,
  // Output handshake
  output logic                  out_valid_o,
  input  logic                  out_ready_i,
  // Indication of valid data in flight
  output logic                  busy_o
);

  // ----------
  // Constants
  // ----------
  localparam int SRC_EXP_BITS = fpnew_pkg::exp_bits(SrcFpFormat);
  localparam int SRC_MAN_BITS = fpnew_pkg::man_bits(SrcFpFormat);
  localparam int SRC_BIAS     = fpnew_pkg::bias(SrcFpFormat);
  localparam int DST_EXP_BITS = fpnew_pkg::exp_bits(DstFpFormat);
  localparam int DST_MAN_BITS = fpnew_pkg::man_bits(DstFpFormat);
  localparam int DST_BIAS     = fpnew_pkg::bias(DstFpFormat);

  // If needed, there will be a LZC for renormalization
  localparam int unsigned LZC_RESULT_WIDTH = $clog2(SRC_MAN_BITS + 1);
  // The wider of both exponent widths
  localparam int unsigned SUPER_EXP_BITS = fpnew_pkg::maximum(SRC_EXP_BITS, DST_EXP_BITS);
  // The internal exponent must be able to represent the smallest denormal input value as signed
  localparam int unsigned INT_EXP_WIDTH =
      fpnew_pkg::maximum(SUPER_EXP_BITS, $clog2(SRC_BIAS + SRC_MAN_BITS)) + 1; // +1 for signed
  // The wider of both mantissa widhts, includes normal bit
  localparam int unsigned INT_MAN_WIDTH = fpnew_pkg::maximum(SRC_MAN_BITS, DST_MAN_BITS) + 1;

  // ----------------
  // Type definition
  // ----------------
  typedef struct packed {
    logic                    sign;
    logic [SRC_EXP_BITS-1:0] exponent;
    logic [SRC_MAN_BITS-1:0] mantissa;
  } src_fp_t;

  typedef struct packed {
    logic                    sign;
    logic [DST_EXP_BITS-1:0] exponent;
    logic [DST_MAN_BITS-1:0] mantissa;
  } dst_fp_t;

  // ---------------
  // Input pipeline
  // ---------------
  // Pipelined input signals
  logic [SRC_WIDTH-1:0]  operands_q;
  logic                  is_boxed_q;
  fpnew_pkg::roundmode_e rnd_mode_q;

  // Generate pipeline at input if needed
  if (PipeConfig==fpnew_pkg::BEFORE) begin : input_pipeline
    fpnew_pipe_in #(
      .Width       ( SRC_WIDTH   ),
      .NumPipeRegs ( NumPipeRegs ),
      .NumOperands ( 1           ),
      .TagType     ( TagType     )
    ) i_input_pipe (
      .clk_i,
      .rst_ni,
      .operands_i,
      .is_boxed_i,
      .rnd_mode_i,
      .op_i           ( fpnew_pkg::FMADD ), // unused
      .op_mod_i       ( 1'b0             ), // unused
      .src_fmt_i      ( fpnew_pkg::FP32  ), // unused
      .dst_fmt_i      ( fpnew_pkg::FP32  ), // unused
      .int_fmt_i      ( fpnew_pkg::INT8  ), // unused
      .tag_i,
      .aux_i,
      .in_valid_i,
      .in_ready_o,
      .flush_i,
      .operands_o     ( operands_q   ),
      .is_boxed_o     ( is_boxed_q   ),
      .rnd_mode_o     ( rnd_mode_q   ),
      .op_o           ( /* unused */ ),
      .op_mod_o       ( /* unused */ ),
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
    assign operands_q = operands_i;
    assign is_boxed_q = is_boxed_i;
    assign rnd_mode_q = rnd_mode_i;
  end

  // -----------------
  // Input processing
  // -----------------
  src_fp_t             operand_a;
  fpnew_pkg::fp_info_t info_a;

  logic signed [SRC_EXP_BITS:0]    encoded_exp;  // biased encoded exponent
  logic signed [INT_EXP_WIDTH-1:0] input_exp;    // unbiased true exponent
  logic        [SRC_MAN_BITS:0]    encoded_mant; // as encoded, includes normal bit
  logic        [INT_MAN_WIDTH-1:0] input_mant;   // normalized input mantissa

  assign operand_a = operands_q;

  // Classify input
  fpnew_classifier #(
    .FpFormat    ( SrcFpFormat ),
    .NumOperands ( 1           )
    ) i_class_a (
    .operands_i ( operands_q ),
    .is_boxed_i ( is_boxed_q ),
    .info_o     ( info_a     )
  );

  assign encoded_mant = {info_a.is_normal, operand_a.mantissa};
  assign encoded_exp  = signed'({1'b0, operand_a.exponent});

  // In case of growing exponent size, denormal values need to be normalized
  if (DST_EXP_BITS > SRC_EXP_BITS) begin : renormalize_mantissa
    logic [LZC_RESULT_WIDTH-1:0] renorm_shamt;     // renormalization shift amount
    logic [LZC_RESULT_WIDTH:0]   renorm_shamt_sgn; // signed form for calculations

    // Leading-zero counter is needed for renormalization
    lzc #(
      .WIDTH ( SRC_MAN_BITS + 1 ),
      .MODE  ( 1                ) // MODE = 1 counts leading zeroes
    ) i_lzc (
      .in_i    ( encoded_mant ),
      .cnt_o   ( renorm_shamt ),
      .empty_o ( /* unused */ )
    );

    assign renorm_shamt_sgn = signed'({1'b0, renorm_shamt});

    // Realign input mantissa, append zeroes if destination is wider
    assign input_mant = {>> {encoded_mant << renorm_shamt, '0}};
    // Unbias exponent and compensate for shift
    assign input_exp = signed'(encoded_exp - renorm_shamt_sgn + info_a.is_subnormal - SRC_BIAS);
  // Otherwise we leave the denormals be because they cannot become normal
  end else begin : normalized_mantissa
    assign input_mant = encoded_mant;
    assign input_exp  = signed'(encoded_exp + info_a.is_subnormal - SRC_BIAS);
  end

  // ----------------------
  // Special case handling
  // ----------------------
  dst_fp_t            special_result;
  fpnew_pkg::status_t special_status;
  logic               result_is_special;


  // We handle zero and NaN inputs separately
  assign result_is_special = info_a.is_zero | info_a.is_nan | ~info_a.is_boxed;

  // Signalling NaNs raise invalid flag, otherwise no flags set
  assign special_status = '{NV: info_a.is_signalling, default: 1'b0};

  // Assemble result according to special case
  assign special_result = info_a.is_zero
                          ? '{sign: operand_a.sign, exponent: '0, mantissa: '0} // signed zero
                          : '{sign: 1'b0, exponent: '1, mantissa: 2**(DST_MAN_BITS-1)}; // qNaN

  // --------
  // Casting
  // --------
  logic signed [INT_EXP_WIDTH-1:0] destination_exp;  // re-biased exponent for destination
  logic        [DST_EXP_BITS-1:0]  final_exp;        // after eventual adjustments

  logic [INT_MAN_WIDTH+DST_MAN_BITS:0] preshift_mant;    // mantissa before final shift
  logic [INT_MAN_WIDTH+DST_MAN_BITS:0] destination_mant; // mantissa from shifter, with rnd bit
  logic [DST_MAN_BITS-1:0]             final_mant;       // mantissa after adjustments

  logic [$clog2(DST_MAN_BITS+1)-1:0] denorm_shamt; // shift amount for denormalization

  logic [1:0] round_sticky_bits;
  logic       of_before_round, uf_before_round;

  // Rebias the exponent
  assign destination_exp = signed'(input_exp + DST_BIAS);

  // Perform adjustments to mantissa and exponent
  always_comb begin : cast_value
    // Default assignment
    final_exp       = unsigned'(destination_exp); // take exponent as is, only look at lower bits
    preshift_mant   = '0;  // initialize mantissa container with zeroes
    denorm_shamt    = '0;
    of_before_round = 1'b0;
    uf_before_round = 1'b0;

    // Place mantissa to the left of the shifter
    preshift_mant = {>> {input_mant, '0}};

    // Handle overflows or infinities (for proper rounding)
    if ((destination_exp >= 2**DST_EXP_BITS-1) || info_a.is_inf) begin
      final_exp       = unsigned'(2**DST_EXP_BITS-2); // largest normal value
      preshift_mant   = '1;                           // largest normal value and RS bits set
      of_before_round = 1'b1;
    // In case the destination exponent is smaller, we need to denormalize the underflow
    end else if (DST_EXP_BITS < SRC_EXP_BITS) begin // STATIC
      // Denormalize underflowing values
      if (destination_exp < 1 && destination_exp >= -DST_MAN_BITS) begin
        final_exp       = '0; // denormal result
        denorm_shamt    = unsigned'(1 - destination_exp); // adjust mantissa by right shifting
        uf_before_round = 1'b1;
      // Limit the shift to retain sticky bits
      end else if (destination_exp < -signed'(DST_MAN_BITS)) begin
        final_exp       = '0; // denormal result
        denorm_shamt    = unsigned'(1 + DST_MAN_BITS); // shift mantissa into sticky bits
        uf_before_round = 1'b1;
      end
    // Otherwise, only previously denormal values can be denormal with exponent 0
    end else if (destination_exp == 0) begin
      denorm_shamt    = 1;    // the shifter becomes a trivial 2-input mux
      uf_before_round = 1'b1;
    end
  end

  // Mantissa adjustment shift
  assign destination_mant = preshift_mant >> denorm_shamt;
  // Extract final mantissa and round bit, discard the normal bit
  assign {final_mant, round_sticky_bits[1]} =
      destination_mant[INT_MAN_WIDTH+DST_MAN_BITS-1:INT_MAN_WIDTH-1];
  // Collapse sticky bits
  assign round_sticky_bits[0] = (| {destination_mant[INT_MAN_WIDTH-2:0]}); // unused bits are sticky

  // ----------------------------
  // Rounding and classification
  // ----------------------------
  logic [DST_EXP_BITS+DST_MAN_BITS-1:0] pre_round_abs; // absolute value of result before rounding

  logic of_after_round; // overflow
  logic uf_after_round; // underflow

  logic                                 rounded_sign;
  logic [DST_EXP_BITS+DST_MAN_BITS-1:0] rounded_abs; // absolute value of result after rounding

  assign pre_round_abs = {final_exp, final_mant};

  // Rounding is only needed if exponent size or mantissa became smaller
  if ((DST_MAN_BITS < SRC_MAN_BITS) || (DST_EXP_BITS < SRC_EXP_BITS)) begin : gen_rounding

    fpnew_rounding #(
      .AbsWidth ( DST_WIDTH - 1 )
    ) i_fpnew_rounding (
      .abs_value_i             ( pre_round_abs     ),
      .sign_i                  ( operand_a.sign    ),
      .round_sticky_bits_i     ( round_sticky_bits ),
      .rnd_mode_i              ( rnd_mode_q        ),
      .effective_subtraction_i ( 1'b0              ), // no operation happened
      .abs_rounded_o           ( rounded_abs       ),
      .sign_o                  ( rounded_sign      ),
      .exact_zero_o            ( /* unused */      )
    );

  end else begin : no_rounding
    assign rounded_abs  = pre_round_abs;
    assign rounded_sign = operand_a.sign;
  end

  // Classification after rounding
  assign uf_after_round = rounded_abs[DST_EXP_BITS+DST_MAN_BITS-1:DST_MAN_BITS] == '0; // denormal
  assign of_after_round = rounded_abs[DST_EXP_BITS+DST_MAN_BITS-1:DST_MAN_BITS] == '1; // inf exp.

  // -----------------
  // Result selection
  // -----------------
  logic [DST_WIDTH-1:0] regular_result;
  fpnew_pkg::status_t   regular_status;

  // Assemble regular result
  assign regular_result = {rounded_sign, rounded_abs};
  assign regular_status = '{
    NV: 1'b0, // only valid cases are handled in regular path
    DZ: 1'b0, // no divisions
    OF: of_before_round | of_after_round,         // rounding can introduce new overflow
    UF: uf_after_round,                           // true zero results don't count as underflow
    NX: (| round_sticky_bits) | of_before_round | of_after_round // RS bits mean loss in precision
  };

  // Final results for output pipeline
  logic [DST_WIDTH-1:0] result_d;
  fpnew_pkg::status_t   status_d;

  // Select output depending on special case detection
  assign result_d = result_is_special ? special_result : regular_result;
  assign status_d = result_is_special ? special_status : regular_status;

  // ----------------
  // Output Pipeline
  // ----------------
  // Generate pipeline at output if needed
  if (PipeConfig==fpnew_pkg::AFTER) begin : output_pipline
    fpnew_pipe_out #(
      .Width       ( DST_WIDTH   ),
      .NumPipeRegs ( NumPipeRegs ),
      .TagType     ( TagType     )
    ) i_output_pipe (
      .clk_i,
      .rst_ni,
      .result_i        ( result_d        ),
      .status_i        ( status_d        ),
      .extension_bit_i ( 1'b1            ), // always NaN-Box result
      .class_mask_i    ( fpnew_pkg::QNAN ), // unused
      .is_class_i      ( 1'b0            ), // unused
      .tag_i,
      .aux_i,
      .in_valid_i,
      .in_ready_o,
      .flush_i,
      .result_o,
      .status_o,
      .extension_bit_o,
      .class_mask_o    ( /* unused */ ),
      .is_class_o      ( /* unused */ ),
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
    assign extension_bit_o = 1'b1; // always NaN-Box result
  end

endmodule
