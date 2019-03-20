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

module fpnew_f2icast #(
  parameter fpnew_pkg::fp_format_e   SrcFpFormat  = fpnew_pkg::FP32,
  parameter fpnew_pkg::ifmt_logic_t  IntFmtConfig = '{default: 1'b1},
  parameter int unsigned             NumPipeRegs  = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig   = fpnew_pkg::BEFORE,
  parameter type                     TagType      = logic,
  parameter type                     AuxType      = logic,
  // Do not change
  localparam int unsigned SRC_WIDTH = fpnew_pkg::fp_width(SrcFpFormat),
  localparam int unsigned DST_WIDTH = fpnew_pkg::max_int_width(IntFmtConfig)
) (
  input  logic                   clk_i,
  input  logic                   rst_ni,
  // Input signals
  input  logic [SRC_WIDTH-1:0]   operands_i, // 1 operand
  input  logic                   is_boxed_i, // 1 operand
  input  fpnew_pkg::roundmode_e  rnd_mode_i,
  input  logic                   op_mod_i,
  input  fpnew_pkg::int_format_e int_fmt_i,
  input  TagType                 tag_i,
  input  AuxType                 aux_i,
  // Input Handshake
  input  logic                   in_valid_i,
  output logic                   in_ready_o,
  input  logic                   flush_i,
  // Output signals
  output logic [DST_WIDTH-1:0]   result_o,
  output fpnew_pkg::status_t     status_o,
  output logic                   extension_bit_o,
  output TagType                 tag_o,
  output AuxType                 aux_o,
  // Output handshake
  output logic                   out_valid_o,
  input  logic                   out_ready_i,
  // Indication of valid data in flight
  output logic                   busy_o
);

  // ----------
  // Constants
  // ----------
  localparam int SRC_EXP_BITS = fpnew_pkg::exp_bits(SrcFpFormat);
  localparam int SRC_MAN_BITS = fpnew_pkg::man_bits(SrcFpFormat);
  localparam int SRC_BIAS     = fpnew_pkg::bias(SrcFpFormat);

  // The internal exponent is signed
  localparam int unsigned INT_EXP_WIDTH = SRC_EXP_BITS + 1; // +1 for signed
  // The internal mantissa contains the normal bit
  localparam int unsigned INT_MAN_WIDTH = SRC_MAN_BITS + 1;

  // ----------------
  // Type definition
  // ----------------
  typedef struct packed {
    logic                    sign;
    logic [SRC_EXP_BITS-1:0] exponent;
    logic [SRC_MAN_BITS-1:0] mantissa;
  } src_fp_t;

  // ---------------
  // Input pipeline
  // ---------------
  // Pipelined input signals
  logic [SRC_WIDTH-1:0]   operands_q;
  logic                   is_boxed_q;
  fpnew_pkg::roundmode_e  rnd_mode_q;
  logic                   op_mod_q;
  fpnew_pkg::int_format_e int_fmt_q;

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
      .op_mod_i,
      .src_fmt_i      ( fpnew_pkg::FP32  ), // unused
      .dst_fmt_i      ( fpnew_pkg::FP32  ), // unused
      .int_fmt_i,
      .tag_i,
      .aux_i,
      .in_valid_i,
      .in_ready_o,
      .flush_i,
      .operands_o     ( operands_q   ),
      .is_boxed_o     ( is_boxed_q   ),
      .rnd_mode_o     ( rnd_mode_q   ),
      .op_o           ( /* unused */ ),
      .op_mod_o       ( op_mod_q     ),
      .src_fmt_o      ( /* unused */ ),
      .dst_fmt_o      ( /* unused */ ),
      .int_fmt_o      ( int_fmt_q    ),
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
    assign op_mod_q   = op_mod_i;
    assign int_fmt_q  = int_fmt_i;
  end

  // -----------------
  // Input processing
  // -----------------
  src_fp_t             operand_a;
  fpnew_pkg::fp_info_t info_a;

  logic signed [SRC_EXP_BITS:0]    encoded_exp;  // biased encoded exponent
  logic signed [INT_EXP_WIDTH-1:0] input_exp;    // unbiased true exponent
  logic        [SRC_MAN_BITS:0]    encoded_mant; // as encoded, includes normal bit

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

  assign encoded_mant = {1'b1, operand_a.mantissa}; // we don't care about denormals
  assign encoded_exp  = signed'({1'b0, operand_a.exponent});

  assign input_exp  = signed'(encoded_exp - SRC_BIAS); // Only handle normal cases

  // --------
  // Casting
  // --------
  logic [INT_MAN_WIDTH+DST_WIDTH:0] preshift_mant;    // mantissa before final shift with rnd bit
  logic [INT_MAN_WIDTH+DST_WIDTH:0] destination_mant; // mantissa from shifter with rnd bit
  logic [DST_WIDTH-1:0]             final_mant;       // final integer value after adjustments

  logic [$clog2(DST_WIDTH+1)-1:0] mant_shamt; // shift amount for mantissa

  logic [1:0] round_sticky_bits;
  logic       of_before_round, uf_before_round;

  // Perform adjustments to mantissa and exponent
  always_comb begin : cast_value
    // Default assignment
    preshift_mant   = '0;  // initialize mantissa container with zeroes
    // Mantissa with implicit bit can be right shifted to represent integer value
    mant_shamt      = unsigned'(DST_WIDTH - 1 - input_exp);
    of_before_round = 1'b0;
    uf_before_round = 1'b0;

    // Place mantissa to the left of the shifter
    preshift_mant[INT_MAN_WIDTH+DST_WIDTH:DST_WIDTH+1] = encoded_mant;

    // Detect overflows. Range for conversions to unsigned is larger by one.
    if (input_exp >= signed'(fpnew_pkg::int_width(int_fmt_q) - 1 + op_mod_q)) begin
      mant_shamt      = '0;   // prevent shifting
      of_before_round = 1'b1;
    // Handle underflows: all bits to the sticky.
    end else if (input_exp < -1) begin
      mant_shamt      = DST_WIDTH + 1; // Limit shift range to
      uf_before_round = 1'b1;
    end
  end

  // Mantissa shift
  assign destination_mant = preshift_mant >> mant_shamt;

  // Extract final mantissa and round/sticky bits
  always_comb begin : assemble_result
    logic [DST_WIDTH-1:0] fmt_mant[fpnew_pkg::NUM_INT_FORMATS]; // integer value for each format

    // Assemble result for each format
    for (int unsigned i = 0; i < fpnew_pkg::NUM_INT_FORMATS; i++) begin
      if (IntFmtConfig[i]) begin // only active formats
        logic [DST_WIDTH-1:0] sign_ext_mask;   // mask for the locations of sign extension bits
        logic                 sign_ext_val;    // the sign extension bits for this format
        logic [DST_WIDTH-1:0] sign_ext_vector; // full of sign extension bits

        // Default assignment: the result mantissa
        fmt_mant[i] = destination_mant[INT_MAN_WIDTH+DST_WIDTH:INT_MAN_WIDTH+1];
        // Set up sign extension mask and value
        sign_ext_mask   = '1 << fpnew_pkg::int_width(fpnew_pkg::int_format_e'(i));
        sign_ext_val    = fmt_mant[i][fpnew_pkg::int_width(fpnew_pkg::int_format_e'(i))-1];
        sign_ext_vector = '{default: sign_ext_val};

        // Combine result and sign extension vectors
        fmt_mant[i] = (sign_ext_mask & sign_ext_vector) | (~sign_ext_mask & fmt_mant[i]);
      end
    end
    // Select result according to format chosen
    final_mant = fmt_mant[int_fmt_q];
  end

  assign round_sticky_bits[1] = destination_mant[INT_MAN_WIDTH];           // rnd bit
  assign round_sticky_bits[0] = (| {destination_mant[INT_MAN_WIDTH-1:0]}); // unused bits are sticky

  // ----------------------------
  // Rounding and classification
  // ----------------------------
  logic [DST_WIDTH-1:0] pre_round_abs; // absolute value of result before rounding

  logic                 rounded_sign;
  logic [DST_WIDTH-1:0] rounded_abs;      // absolute value of result after rounding
  logic                 res_zero;

  assign pre_round_abs = final_mant;

  fpnew_rounding #(
    .AbsWidth ( DST_WIDTH )
  ) i_fpnew_rounding (
    .abs_value_i             ( pre_round_abs     ),
    .sign_i                  ( operand_a.sign    ),
    .round_sticky_bits_i     ( round_sticky_bits ),
    .rnd_mode_i              ( rnd_mode_q        ),
    .effective_subtraction_i ( 1'b0              ), // no operation happened
    .abs_rounded_o           ( rounded_abs       ),
    .sign_o                  ( rounded_sign      ),
    .exact_zero_o            ( res_zero          )
  );

  // ----------------------
  // Special case handling
  // ----------------------
  logic [DST_WIDTH-1:0] special_result;
  fpnew_pkg::status_t   special_status;
  logic                 result_is_special;

  // We handle Inf, NaN, overflows and negative unsigned values separately
  assign result_is_special = info_a.is_nan | info_a.is_inf | of_before_round | ~info_a.is_boxed |
                             (operand_a.sign & op_mod_q & ~res_zero);

  // All special cases are invalid
  assign special_status = '{NV: 1'b1, default: 1'b0};

  // Assemble result according to special case
  always_comb begin : special_cases
    logic [DST_WIDTH-1:0] fmt_special_result[fpnew_pkg::NUM_INT_FORMATS];

    // Determine special result for each format separately
    for (int unsigned i = 0; i < fpnew_pkg::NUM_INT_FORMATS; i++) begin
      if (IntFmtConfig[i]) begin // only active formats
        logic [DST_WIDTH-1:0] sign_ext_mask;   // mask for the locations of sign extension bits
        logic [DST_WIDTH-1:0] sign_ext_vector; // full of sign extension bits
        // Default assignment: set all ones
        fmt_special_result[i] = '1;
        // MSB of special result depends on signed/unsigned
        fmt_special_result[i][fpnew_pkg::int_width(fpnew_pkg::int_format_e'(i))-1] = op_mod_q;

        // In case of a negative result, flip the bits to procude -max or 0
        if (!info_a.is_nan && operand_a.sign)
          fmt_special_result[i] = ~fmt_special_result[i];

        // Set up sign extension mask and value
        sign_ext_mask   = '1 << fpnew_pkg::int_width(fpnew_pkg::int_format_e'(i));
        sign_ext_vector = '{default: op_mod_q ^ (~info_a.is_nan & operand_a.sign)};

        // Combine result and sign extension bits
        fmt_special_result[i] = (sign_ext_mask & sign_ext_vector) |
                                (~sign_ext_mask & fmt_special_result[i]);

      end
    end
    // Select result according to format
    special_result = fmt_special_result[int_fmt_q];

  end

  // -----------------
  // Result selection
  // -----------------
  logic [DST_WIDTH-1:0] regular_result;
  fpnew_pkg::status_t   regular_status;

  // Invert regular result depending on sign
  assign regular_result   = rounded_sign ? unsigned'(-rounded_abs) : rounded_abs;
  assign regular_status   = '{NX: (| round_sticky_bits), default: 1'b0}; // only NX can be raised

  // Final results for output pipeline
  logic [DST_WIDTH-1:0] result_d;
  fpnew_pkg::status_t   status_d;
  logic                 extension_bit;

  // Select output depending on special case detection
  assign result_d = result_is_special ? special_result : regular_result;
  assign status_d = result_is_special ? special_status : regular_status;

  // MSB of result decides extension
  assign extension_bit = result_d[DST_WIDTH-1];

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
      .extension_bit_i ( extension_bit   ),
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
      .class_mask_o    ( /* unused */  ),
      .is_class_o      ( /* unused */  ),
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
    assign extension_bit_o = extension_bit;
  end

endmodule
