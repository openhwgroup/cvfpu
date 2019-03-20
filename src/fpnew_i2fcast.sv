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

module fpnew_i2fcast #(
  parameter fpnew_pkg::fp_format_e   DstFpFormat  = fpnew_pkg::FP32,
  parameter fpnew_pkg::ifmt_logic_t  IntFmtConfig = '1,
  parameter int unsigned             NumPipeRegs  = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig   = fpnew_pkg::BEFORE,
  parameter type                     TagType      = logic,
  parameter type                     AuxType      = logic,

  localparam int unsigned SRC_WIDTH = fpnew_pkg::max_int_width(IntFmtConfig), // do not change
  localparam int unsigned DST_WIDTH = fpnew_pkg::fp_width(DstFpFormat)  // do not change
) (
  input logic                    clk_i,
  input logic                    rst_ni,
  // Input signals
  input  logic [SRC_WIDTH-1:0]   operands_i, // 1 operand
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
  localparam int DST_EXP_BITS = fpnew_pkg::exp_bits(DstFpFormat);
  localparam int DST_MAN_BITS = fpnew_pkg::man_bits(DstFpFormat);
  localparam int DST_BIAS     = fpnew_pkg::bias(DstFpFormat);

  // The internal mantissa contains the normal and RS bits and must also be able to hold the integer
  localparam int unsigned INT_MAN_WIDTH = fpnew_pkg::maximum(DST_MAN_BITS + 3, SRC_WIDTH);

  // There is a LZC for normalization
  localparam int unsigned LZC_RESULT_WIDTH = $clog2(SRC_WIDTH);

  // The internal exponent is signed and must be wide enough to hold readjustment shift
  localparam int unsigned INT_EXP_WIDTH =
      fpnew_pkg::maximum(DST_EXP_BITS, LZC_RESULT_WIDTH) + 1; // +1 for signed

  // ----------------
  // Type definition
  // ----------------
  typedef struct packed {
    logic                    sign;
    logic [DST_EXP_BITS-1:0] exponent;
    logic [DST_MAN_BITS-1:0] mantissa;
  } dst_fp_t;

  // ---------------
  // Input pipeline
  // ---------------
  // Pipelined input signals
  logic [SRC_WIDTH-1:0]   operands_q;
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
      .is_boxed_i     ( 'X               ), // unused
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
      .is_boxed_o     ( /* unused */ ),
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
    assign rnd_mode_q = rnd_mode_i;
    assign op_mod_q   = op_mod_i;
    assign int_fmt_q  = int_fmt_i;
  end

  // -----------------
  // Input processing
  // -----------------
  logic [SRC_WIDTH-1:0] operand_a;
  logic [SRC_WIDTH-1:0] input_val;
  logic                 input_sign;
  logic [SRC_WIDTH-1:0] input_mag;

  assign operand_a = operands_q;

  // Sign-extend input value
  always_comb begin : sign_ext_input
    logic [SRC_WIDTH-1:0] fmt_input_val[fpnew_pkg::NUM_INT_FORMATS]; // per-format input value

    // sign-extend input for each format
    for (int unsigned i = 0; i < fpnew_pkg::NUM_INT_FORMATS; i++) begin
      if (IntFmtConfig[i]) begin // only active formats
        logic [SRC_WIDTH-1:0] sign_ext_mask;   // mask for the locations of sign extension bits
        logic                 sign_ext_val;    // the sign extension bits for this format
        logic [SRC_WIDTH-1:0] sign_ext_vector; // full of sign extension bits

        // Set up sign extension mask and value
        sign_ext_mask   = '1 << fpnew_pkg::int_width(fpnew_pkg::int_format_e'(i));
        sign_ext_val    = operand_a[fpnew_pkg::int_width(fpnew_pkg::int_format_e'(i))-1];
        sign_ext_vector = '{default: sign_ext_val & ~op_mod_q}; // only for signed casts

        // Combine result and sign extension vectors
        fmt_input_val[i] = (sign_ext_mask & sign_ext_vector) | (~sign_ext_mask & operand_a);
      end
    end

    // Select input according to format chosen
    input_val = fmt_input_val[int_fmt_q];
  end

  // Get the sign for signed casts
  assign input_sign = input_val[SRC_WIDTH-1] & ~op_mod_q;
  // Obtain the input's magnitude
  assign input_mag = input_sign ? unsigned'(-input_val) : input_val;

  // --------
  // Casting
  // --------
  logic signed [INT_EXP_WIDTH-1:0] destination_exp;  // re-biased exponent for destination
  logic        [DST_EXP_BITS-1:0]  final_exp;        // after eventual adjustments

  logic [INT_MAN_WIDTH-1:0] preshift_mant;    // mantissa before normalization shift
  logic [INT_MAN_WIDTH-1:0] destination_mant; // mantissa from shifter
  logic [DST_MAN_BITS-1:0]  final_mant;       // mantissa after adjustments

  logic        [LZC_RESULT_WIDTH-1:0] norm_shamt;   // shift amount for denormalization
  logic signed [LZC_RESULT_WIDTH:0] norm_shamt_sgn; // shift amount in signed form

  logic        result_zero;

  logic [1:0] round_sticky_bits;
  logic       of_before_round;

  // Leading-zero counter is needed for normalization
  lzc #(
    .WIDTH ( SRC_WIDTH ),
    .MODE  ( 1         ) // MODE = 1 counts leading zeroes
  ) i_lzc (
    .in_i    ( input_mag   ),
    .cnt_o   ( norm_shamt  ),
    .empty_o ( result_zero )
  );
  assign norm_shamt_sgn = signed'({1'b0, norm_shamt});

  // Place integer to the left of the shifter space (only matters if DST_MAN_BITS > SRC_WIDTH)
  assign preshift_mant = input_mag << (INT_MAN_WIDTH - SRC_WIDTH); // constant shift
  // Perform the Normalization shift
  assign destination_mant = preshift_mant << norm_shamt;

  // Exponent is calculated from source width and the leading zeroes, bias is added
  assign destination_exp = signed'(SRC_WIDTH - 1 - norm_shamt_sgn + DST_BIAS);

  // Handle the only special case we have: OF
  always_comb begin : detect_overflow
    // Default assignment
    final_exp            = unsigned'(destination_exp); // take exponent as is
    final_mant           = destination_mant[INT_MAN_WIDTH-2 -: DST_MAN_BITS];
    round_sticky_bits[1] = destination_mant[INT_MAN_WIDTH-DST_MAN_BITS-2];
    round_sticky_bits[0] = (| {destination_mant[INT_MAN_WIDTH-DST_MAN_BITS-3:0]}); // reduce sticky
    of_before_round      = 1'b0;

    // Properly set the exponent for zeroes
    if (result_zero) begin
      final_exp = '0;
    // Handle overflows or infinities (for proper rounding)
    end else if (destination_exp >= 2**DST_EXP_BITS-1) begin
      final_exp         = unsigned'(2**DST_EXP_BITS-2); // largest normal value
      final_mant        = '1;                           // largest normal value and RS bits set
      round_sticky_bits = '1;                           // RS set
      of_before_round   = 1'b1;
    end
  end

  // ----------------------------
  // Rounding and classification
  // ----------------------------
  logic [DST_EXP_BITS+DST_MAN_BITS-1:0] pre_round_abs; // absolute value of result before rounding

  logic of_after_round; // overflow

  logic [DST_EXP_BITS+DST_MAN_BITS-1:0] rounded_abs; // absolute value of result after rounding
  logic                                 rounded_sign;

  assign pre_round_abs = {final_exp, final_mant};

  // Rounding is only needed if mantissa smaller than integer or integer has larger range
  if ((DST_MAN_BITS + 1 < SRC_WIDTH) || (SRC_WIDTH > DST_BIAS)) begin : gen_rounding

    fpnew_rounding #(
      .AbsWidth ( DST_WIDTH - 1 )
    ) i_fpnew_rounding (
      .abs_value_i             ( pre_round_abs     ),
      .sign_i                  ( input_sign        ),
      .round_sticky_bits_i     ( round_sticky_bits ),
      .rnd_mode_i              ( rnd_mode_q        ),
      .effective_subtraction_i ( 1'b0              ), // no operation happened
      .abs_rounded_o           ( rounded_abs       ),
      .sign_o                  ( rounded_sign      ),
      .exact_zero_o            ( /* unused */      )
    );

  end else begin : no_rounding
    assign rounded_abs  = pre_round_abs;
    assign rounded_sign = input_sign;
  end

  // Classification after rounding
  assign of_after_round = rounded_abs[DST_EXP_BITS+DST_MAN_BITS-1:DST_MAN_BITS] == '1; // inf exp.

  // -----------------
  // Result selection
  // -----------------
  // Final results for output pipeline
  logic [DST_WIDTH-1:0] result_d;
  fpnew_pkg::status_t   status_d;

  // Assemble final result
  assign result_d = {rounded_sign, rounded_abs};
  assign status_d = '{
    NV: of_before_round | of_after_round, // Overflowing values are invalid for casts
    DZ: 1'b0, // no divisions
    OF: 1'b0, // no overflow per se => it's invalid to cast too large integers
    UF: 1'b0, // no underflow
    NX: (| round_sticky_bits) // RS bits mean loss in precision
  };

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
