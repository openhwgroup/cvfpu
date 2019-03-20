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

module fpnew_cast_multi #(
  parameter fpnew_pkg::fmt_logic_t   FpFmtConfig  = '1,
  parameter fpnew_pkg::ifmt_logic_t  IntFmtConfig = '1,
  // FPU configuration
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,
  // Do not change
  localparam int unsigned WIDTH = fpnew_pkg::maximum(fpnew_pkg::max_fp_width(FpFmtConfig),
                                                     fpnew_pkg::max_int_width(IntFmtConfig)),
  localparam int unsigned NUM_FORMATS = fpnew_pkg::NUM_FP_FORMATS
) (
  input  logic                   clk_i,
  input  logic                   rst_ni,
  // Input signals
  input  logic [WIDTH-1:0]       operands_i, // 1 operand
  input  logic [NUM_FORMATS-1:0] is_boxed_i, // 1 operand
  input  fpnew_pkg::roundmode_e  rnd_mode_i,
  input  fpnew_pkg::operation_e  op_i,
  input  logic                   op_mod_i,
  input  fpnew_pkg::fp_format_e  src_fmt_i,
  input  fpnew_pkg::fp_format_e  dst_fmt_i,
  input  fpnew_pkg::int_format_e int_fmt_i,
  input  TagType                 tag_i,
  input  AuxType                 aux_i,
  // Input Handshake
  input  logic                   in_valid_i,
  output logic                   in_ready_o,
  input  logic                   flush_i,
  // Output signals
  output logic [WIDTH-1:0]       result_o,
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
  localparam int unsigned NUM_INT_FORMATS = fpnew_pkg::NUM_INT_FORMATS;
  localparam int unsigned MAX_INT_WIDTH   = fpnew_pkg::max_int_width(IntFmtConfig);

  localparam fpnew_pkg::fp_encoding_t SUPER_FORMAT = fpnew_pkg::super_format(FpFmtConfig);


  localparam int unsigned SUPER_EXP_BITS = SUPER_FORMAT.exp_bits;
  localparam int unsigned SUPER_MAN_BITS = SUPER_FORMAT.man_bits;
  localparam int unsigned SUPER_BIAS     = 2**(SUPER_EXP_BITS - 1) - 1;

  // The internal mantissa includes normal bit or an entire integer
  localparam int unsigned INT_MAN_WIDTH = fpnew_pkg::maximum(SUPER_MAN_BITS + 1, MAX_INT_WIDTH);
  // If needed, there will be a LZC for renormalization
  localparam int unsigned LZC_RESULT_WIDTH = $clog2(INT_MAN_WIDTH);
  // The internal exponent must be able to represent the smallest denormal input value as signed
  // or the number of bits in an integer
  localparam int unsigned INT_EXP_WIDTH = fpnew_pkg::maximum($clog2(MAX_INT_WIDTH),
      fpnew_pkg::maximum(SUPER_EXP_BITS, $clog2(SUPER_BIAS + SUPER_MAN_BITS))) + 1;

  // ---------------
  // Input pipeline
  // ---------------
  // Pipelined input signals
  logic [WIDTH-1:0]       operands_q;
  logic [NUM_FORMATS-1:0] is_boxed_q;
  fpnew_pkg::roundmode_e  rnd_mode_q;
  fpnew_pkg::operation_e  op_q;
  logic                   op_mod_q;
  fpnew_pkg::fp_format_e  src_fmt_q;
  fpnew_pkg::fp_format_e  dst_fmt_q;
  fpnew_pkg::int_format_e int_fmt_q;

  // Generate pipeline at input if needed
  if (PipeConfig==fpnew_pkg::BEFORE) begin : input_pipeline
    fpnew_pipe_in #(
      .Width       ( WIDTH       ),
      .NumPipeRegs ( NumPipeRegs ),
      .NumOperands ( 1           ),
      .NumFormats  ( NUM_FORMATS ),
      .TagType     ( TagType     ),
      .AuxType     ( AuxType     )
    ) i_input_pipe (
      .clk_i,
      .rst_ni,
      .operands_i,
      .is_boxed_i,
      .rnd_mode_i,
      .op_i,
      .op_mod_i,
      .src_fmt_i,
      .dst_fmt_i,
      .int_fmt_i,
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
      .src_fmt_o      ( src_fmt_q    ),
      .dst_fmt_o      ( dst_fmt_q    ),
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
    assign op_q       = op_i;
    assign op_mod_q   = op_mod_i;
    assign src_fmt_q  = src_fmt_i;
    assign dst_fmt_q  = dst_fmt_i;
    assign int_fmt_q  = int_fmt_i;
  end

  // -----------------
  // Input processing
  // -----------------
  logic src_is_int, dst_is_int; // if 0, it's a float

  assign src_is_int = (op_q == fpnew_pkg::I2F);
  assign dst_is_int = (op_q == fpnew_pkg::F2I);

  logic [INT_MAN_WIDTH-1:0] encoded_mant; // input mantissa with implicit bit

  logic        [NUM_FORMATS-1:0]                    fmt_sign;
  logic signed [NUM_FORMATS-1:0][INT_EXP_WIDTH-1:0] fmt_exponent;
  logic        [NUM_FORMATS-1:0][INT_MAN_WIDTH-1:0] fmt_mantissa;
  logic signed [NUM_FORMATS-1:0][INT_EXP_WIDTH-1:0] fmt_shift_compensation; // for LZC

  fpnew_pkg::fp_info_t [NUM_FORMATS-1:0] info_q;

  logic [NUM_INT_FORMATS-1:0][INT_MAN_WIDTH-1:0] ifmt_input_val;
  logic                                          int_sign;
  logic [INT_MAN_WIDTH-1:0]                      int_value, int_mantissa;

  // FP Input initialization
  for (genvar fmt = 0; fmt < int'(NUM_FORMATS); fmt++) begin : fmt_init_inputs
    // Set up some constants
    localparam int unsigned FP_WIDTH = fpnew_pkg::fp_width(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned EXP_BITS = fpnew_pkg::exp_bits(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned MAN_BITS = fpnew_pkg::man_bits(fpnew_pkg::fp_format_e'(fmt));

    if (FpFmtConfig[fmt]) begin : active_format
      // Classify input
      fpnew_classifier #(
        .FpFormat    ( fpnew_pkg::fp_format_e'(fmt) ),
        .NumOperands ( 1                            )
      ) i_fpnew_classifier (
        .operands_i ( operands_q[FP_WIDTH-1:0] ),
        .is_boxed_i ( is_boxed_q[fmt]          ),
        .info_o     ( info_q[fmt]              )
      );

      assign fmt_sign[fmt]     = operands_q[FP_WIDTH-1];
      assign fmt_exponent[fmt] = signed'({1'b0, operands_q[MAN_BITS+:EXP_BITS]});
      assign fmt_mantissa[fmt] = {info_q[fmt].is_normal, operands_q[MAN_BITS-1:0]}; // zero pad
      // Compensation for the difference in mantissa widths used for leading-zero count
      assign fmt_shift_compensation[fmt] = signed'(INT_MAN_WIDTH - 1 - MAN_BITS);
    end else begin : inactive_format
      assign info_q[fmt]                 = 'X; // propagate don't care (format disabled)
      assign fmt_sign[fmt]               = 'x; // propagate don't care (format disabled)
      assign fmt_exponent[fmt]           = 'X; // propagate don't care (format disabled)
      assign fmt_mantissa[fmt]           = 'X; // propagate don't care (format disabled)
      assign fmt_shift_compensation[fmt] = 'X; // propagate don't care (format disabled)
    end
  end

  // Sign-extend INT input
  for (genvar ifmt = 0; ifmt < int'(NUM_INT_FORMATS); ifmt++) begin : gen_sign_extend_int
    // Set up some constants
    localparam int unsigned INT_WIDTH = fpnew_pkg::int_width(fpnew_pkg::int_format_e'(ifmt));

    if (IntFmtConfig[ifmt]) begin : active_format // only active formats
      always_comb begin : sign_ext_input
        // sign-extend value only if it's signed
        ifmt_input_val[ifmt]                = '{default: operands_q[INT_WIDTH-1] & ~op_mod_q};
        ifmt_input_val[ifmt][INT_WIDTH-1:0] = operands_q[INT_WIDTH-1:0];
      end
    end else begin : inactive_format
      assign ifmt_input_val[ifmt] = 'X; // don't care about disabled formats
    end
  end

  // Construct input mantissa from integer
  assign int_value    = ifmt_input_val[int_fmt_q];
  assign int_sign     = int_value[INT_MAN_WIDTH-1] & ~op_mod_q; // only signed ints are negative
  assign int_mantissa = int_sign ? unsigned'(-int_value) : int_value; // get magnitude of negative

  // select mantissa with source format
  assign encoded_mant = src_is_int ? int_mantissa : fmt_mantissa[src_fmt_q];

  // --------------
  // Normalization
  // --------------
  logic signed [INT_EXP_WIDTH-1:0] src_bias;      // src format bias
  logic signed [INT_EXP_WIDTH-1:0] src_exp;       // src format exponent (biased)
  logic signed [INT_EXP_WIDTH-1:0] src_subnormal; // src is subnormal
  logic signed [INT_EXP_WIDTH-1:0] src_offset;    // src offset within mantissa

  assign src_bias      = signed'(fpnew_pkg::bias(src_fmt_q));
  assign src_exp       = fmt_exponent[src_fmt_q];
  assign src_subnormal = signed'({1'b0, info_q[src_fmt_q].is_subnormal});
  assign src_offset    = fmt_shift_compensation[src_fmt_q];

  logic                            input_sign;   // input sign
  logic signed [INT_EXP_WIDTH-1:0] input_exp;    // unbiased true exponent
  logic        [INT_MAN_WIDTH-1:0] input_mant;   // normalized input mantissa
  logic                            mant_is_zero; // for integer zeroes

  logic signed [INT_EXP_WIDTH-1:0] fp_input_exp;
  logic signed [INT_EXP_WIDTH-1:0] int_input_exp;

  // Input mantissa needs to be normalized
  logic [LZC_RESULT_WIDTH-1:0] renorm_shamt;     // renormalization shift amount
  logic [LZC_RESULT_WIDTH:0]   renorm_shamt_sgn; // signed form for calculations

  // Leading-zero counter is needed for renormalization
  lzc #(
    .WIDTH ( INT_MAN_WIDTH ),
    .MODE  ( 1             ) // MODE = 1 counts leading zeroes
  ) i_lzc (
    .in_i    ( encoded_mant ),
    .cnt_o   ( renorm_shamt ),
    .empty_o ( mant_is_zero )
  );
  assign renorm_shamt_sgn = signed'({1'b0, renorm_shamt});

  // Get the sign from the proper source
  assign input_sign = src_is_int ? int_sign : fmt_sign[src_fmt_q];
  // Realign input mantissa, append zeroes if destination is wider
  assign input_mant = encoded_mant << renorm_shamt;
  // Unbias exponent and compensate for shift
  assign fp_input_exp  = signed'(src_exp + src_subnormal - src_bias -
                                 renorm_shamt_sgn + src_offset); // compensate for shift
  assign int_input_exp = signed'(INT_MAN_WIDTH - 1 - renorm_shamt_sgn);

  assign input_exp     = src_is_int ? int_input_exp : fp_input_exp;

  // --------
  // Casting
  // --------
  logic signed [INT_EXP_WIDTH-1:0] destination_exp;  // re-biased exponent for destination
  logic        [INT_EXP_WIDTH-1:0] final_exp;        // after eventual adjustments

  logic signed [INT_EXP_WIDTH-1:0] dst_bias;      // dst format bias
  assign dst_bias = signed'(fpnew_pkg::bias(dst_fmt_q));

  logic [2*INT_MAN_WIDTH:0]  preshift_mant;    // mantissa before final shift
  logic [2*INT_MAN_WIDTH:0]  destination_mant; // mantissa from shifter, with rnd bit
  logic [SUPER_MAN_BITS-1:0] final_mant;       // mantissa after adjustments
  logic [MAX_INT_WIDTH-1:0]  final_int;        // integer shifted in position

  logic [$clog2(INT_MAN_WIDTH+1)-1:0] denorm_shamt; // shift amount for denormalization

  logic [1:0] fp_round_sticky_bits, int_round_sticky_bits, round_sticky_bits;
  logic       of_before_round, uf_before_round;

  // Rebias the exponent
  assign destination_exp = input_exp + dst_bias;

  // Perform adjustments to mantissa and exponent
  always_comb begin : cast_value
    // Default assignment
    final_exp       = unsigned'(destination_exp); // take exponent as is, only look at lower bits
    preshift_mant   = '0;  // initialize mantissa container with zeroes
    denorm_shamt    = SUPER_MAN_BITS - fpnew_pkg::man_bits(dst_fmt_q); // right of mantissa
    of_before_round = 1'b0;
    uf_before_round = 1'b0;

    // Place mantissa to the left of the shifter
    preshift_mant = {>> {input_mant, '0}};

    // Handle INT casts
    if (dst_is_int) begin
      // By default right shift mantissa to be an integer
      denorm_shamt = unsigned'(MAX_INT_WIDTH - 1 - input_exp);
      // overflow: when converting to unsigned the range is larger by one
      if (input_exp >= signed'(fpnew_pkg::int_width(int_fmt_q) - 1 + op_mod_q)) begin
        denorm_shamt    = '0; // prevent shifting
        of_before_round = 1'b1;
      // underflow
      end else if (input_exp < -1) begin
        denorm_shamt    = MAX_INT_WIDTH + 1; // all bits go to the sticky
        uf_before_round = 1'b1;
      end
    // Handle FP over-/underflows
    end else begin
      // Overflow or infinities (for proper rounding)
      if ((destination_exp >= 2**fpnew_pkg::exp_bits(dst_fmt_q)-1) ||
          (~src_is_int && info_q[src_fmt_q].is_inf)) begin
        final_exp       = unsigned'(2**fpnew_pkg::exp_bits(dst_fmt_q)-2); // largest normal value
        preshift_mant   = '1;                           // largest normal value and RS bits set
        of_before_round = 1'b1;
      // Denormalize underflowing values
      end else if (destination_exp < 1 &&
                   destination_exp >= -signed'(fpnew_pkg::man_bits(dst_fmt_q))) begin
        final_exp       = '0; // denormal result
        denorm_shamt    = unsigned'(denorm_shamt + 1 - destination_exp); // adjust right shifting
        uf_before_round = 1'b1;
      // Limit the shift to retain sticky bits
      end else if (destination_exp < -signed'(fpnew_pkg::man_bits(dst_fmt_q))) begin
        final_exp       = '0; // denormal result
        denorm_shamt    = unsigned'(denorm_shamt + 2 + fpnew_pkg::man_bits(dst_fmt_q)); // to sticky
        uf_before_round = 1'b1;
      end
    end
  end

  localparam NUM_FP_STICKY  = 2 * INT_MAN_WIDTH - SUPER_MAN_BITS - 1; // removed mantissa, 1. and R
  localparam NUM_INT_STICKY = 2 * INT_MAN_WIDTH - MAX_INT_WIDTH; // removed int and R

  // Mantissa adjustment shift
  assign destination_mant = preshift_mant >> denorm_shamt;
  // Extract final mantissa and round bit, discard the normal bit (for FP)
  assign {final_mant, fp_round_sticky_bits[1]} =
      destination_mant[2*INT_MAN_WIDTH-1-:SUPER_MAN_BITS+1];
  assign {final_int, int_round_sticky_bits[1]} = destination_mant[2*INT_MAN_WIDTH-:MAX_INT_WIDTH+1];
  // Collapse sticky bits
  assign fp_round_sticky_bits[0]  = (| {destination_mant[NUM_FP_STICKY-1:0]});
  assign int_round_sticky_bits[0] = (| {destination_mant[NUM_INT_STICKY-1:0]});

  // select RS bits for destination operation
  assign round_sticky_bits = dst_is_int ? int_round_sticky_bits : fp_round_sticky_bits;

  // ----------------------------
  // Rounding and classification
  // ----------------------------
  logic [WIDTH-1:0] pre_round_abs;  // absolute value of result before rnd
  logic             of_after_round; // overflow
  logic             uf_after_round; // underflow

  logic [NUM_FORMATS-1:0][WIDTH-1:0] fmt_pre_round_abs; // per format
  logic [NUM_FORMATS-1:0]            fmt_of_after_round;
  logic [NUM_FORMATS-1:0]            fmt_uf_after_round;

  logic [NUM_INT_FORMATS-1:0][WIDTH-1:0] ifmt_pre_round_abs; // per format

  logic             rounded_sign;
  logic [WIDTH-1:0] rounded_abs; // absolute value of result after rounding
  logic             result_true_zero;

  logic [WIDTH-1:0] rounded_int_res; // after possible inversion
  logic             rounded_int_res_zero; // after rounding


  // Pack exponent and mantissa into proper rounding form
  for (genvar fmt = 0; fmt < int'(NUM_FORMATS); fmt++) begin : gen_res_assemble
    // Set up some constants
    localparam int unsigned EXP_BITS = fpnew_pkg::exp_bits(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned MAN_BITS = fpnew_pkg::man_bits(fpnew_pkg::fp_format_e'(fmt));

    if (FpFmtConfig[fmt]) begin : active_format
      always_comb begin : assemble_result
        fmt_pre_round_abs[fmt] = {final_exp[EXP_BITS-1:0], final_mant[MAN_BITS-1:0]}; // 0-extend
      end
    end else begin : inactive_format
      assign fmt_pre_round_abs[fmt] = 'X;
    end
  end

  // Sign-extend integer result
  for (genvar ifmt = 0; ifmt < int'(NUM_INT_FORMATS); ifmt++) begin : gen_int_res_sign_ext
    // Set up some constants
    localparam int unsigned INT_WIDTH = fpnew_pkg::int_width(fpnew_pkg::int_format_e'(ifmt));

    if (IntFmtConfig[ifmt]) begin : active_format
      always_comb begin : assemble_result
        // sign-extend reusult
        ifmt_pre_round_abs[ifmt]                = '{default: final_int[INT_WIDTH-1]};
        ifmt_pre_round_abs[ifmt][INT_WIDTH-1:0] = final_int[INT_WIDTH-1:0];
      end
    end else begin : inactive_format
      assign ifmt_pre_round_abs[ifmt] = 'X;
    end
  end

  // Select output with destination format and operation
  assign pre_round_abs = dst_is_int ? ifmt_pre_round_abs[int_fmt_q] : fmt_pre_round_abs[dst_fmt_q];

  fpnew_rounding #(
    .AbsWidth ( WIDTH )
  ) i_fpnew_rounding (
    .abs_value_i             ( pre_round_abs     ),
    .sign_i                  ( input_sign        ), // source format
    .round_sticky_bits_i     ( round_sticky_bits ),
    .rnd_mode_i              ( rnd_mode_q        ),
    .effective_subtraction_i ( 1'b0              ), // no operation happened
    .abs_rounded_o           ( rounded_abs       ),
    .sign_o                  ( rounded_sign      ),
    .exact_zero_o            ( result_true_zero  )
  );

  logic [NUM_FORMATS-1:0][WIDTH-1:0] fmt_result;

  // Detect overflows and inject sign
  for (genvar fmt = 0; fmt < int'(NUM_FORMATS); fmt++) begin : gen_sign_inject
    // Set up some constants
    localparam int unsigned FP_WIDTH = fpnew_pkg::fp_width(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned EXP_BITS = fpnew_pkg::exp_bits(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned MAN_BITS = fpnew_pkg::man_bits(fpnew_pkg::fp_format_e'(fmt));

    if (FpFmtConfig[fmt]) begin : active_format
      always_comb begin : post_process
        // detect of / uf
        fmt_uf_after_round[fmt] = rounded_abs[EXP_BITS+MAN_BITS-1:MAN_BITS] == '0; // denormal
        fmt_of_after_round[fmt] = rounded_abs[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // inf exp.

        // Assemble regular result, nan box short ones. Int zeroes need to be detected`
        fmt_result[fmt]               = '1;
        fmt_result[fmt][FP_WIDTH-1:0] = src_is_int & mant_is_zero
                                        ? '0
                                        : {rounded_sign, rounded_abs[EXP_BITS+MAN_BITS-1:0]};
      end
    end else begin : inactive_format
      assign fmt_uf_after_round[fmt] = 'X;
      assign fmt_of_after_round[fmt] = 'X;
      assign fmt_result[fmt]         = 'X;
    end
  end

  // Classification after rounding select by destination format
  assign uf_after_round = fmt_uf_after_round[dst_fmt_q] & ~result_true_zero; // zero is not UF
  assign of_after_round = fmt_of_after_round[dst_fmt_q];

  // Negative integer result needs to be brought into two's complement
  assign rounded_int_res      = rounded_sign ? unsigned'(-rounded_abs) : rounded_abs;
  assign rounded_int_res_zero = (rounded_int_res == '0);

  // -------------------------
  // FP Special case handling
  // -------------------------
  logic [WIDTH-1:0]   fp_special_result;
  fpnew_pkg::status_t fp_special_status;
  logic               fp_result_is_special;

  logic [NUM_FORMATS-1:0][WIDTH-1:0] fmt_special_result;

  // Special result construction
  for (genvar fmt = 0; fmt < int'(NUM_FORMATS); fmt++) begin : gen_special_results
    // Set up some constants
    localparam int unsigned FP_WIDTH = fpnew_pkg::fp_width(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned EXP_BITS = fpnew_pkg::exp_bits(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned MAN_BITS = fpnew_pkg::man_bits(fpnew_pkg::fp_format_e'(fmt));

    localparam logic [EXP_BITS-1:0] QNAN_EXPONENT = '1;
    localparam logic [MAN_BITS-1:0] QNAN_MANTISSA = 2**(MAN_BITS-1);

    if (FpFmtConfig[fmt]) begin : active_format
      always_comb begin : special_results
        logic [FP_WIDTH-1:0] special_res;

        special_res = info_q[src_fmt_q].is_zero
                      ? fmt_sign[src_fmt_q] << FP_WIDTH-1 // signed zero
                      : {1'b0, QNAN_EXPONENT, QNAN_MANTISSA}; // qNaN

        // Initialize special result with ones (NaN-box)
        fmt_special_result[fmt]               = '1;
        fmt_special_result[fmt][FP_WIDTH-1:0] = special_res;
      end
    end else begin : inactive_format
      assign fmt_special_result[fmt] = 'X;
    end
  end

  // Detect special case from source format, I2F casts don't produce a special result
  assign fp_result_is_special = ~src_is_int & (info_q[src_fmt_q].is_zero |
                                               info_q[src_fmt_q].is_nan |
                                               ~info_q[src_fmt_q].is_boxed);

  // Signalling input NaNs raise invalid flag, otherwise no flags set
  assign fp_special_status = '{NV: info_q[src_fmt_q].is_signalling, default: 1'b0};

  // Assemble result according to destination format
  assign fp_special_result = fmt_special_result[dst_fmt_q]; // destination format

  // --------------------------
  // INT Special case handling
  // --------------------------
  logic [WIDTH-1:0]   int_special_result;
  fpnew_pkg::status_t int_special_status;
  logic               int_result_is_special;

  logic [NUM_INT_FORMATS-1:0][WIDTH-1:0] ifmt_special_result;

  // Special result construction
  for (genvar ifmt = 0; ifmt < int'(NUM_INT_FORMATS); ifmt++) begin : gen_special_results_int
    // Set up some constants
    localparam int unsigned INT_WIDTH = fpnew_pkg::int_width(fpnew_pkg::int_format_e'(ifmt));

    if (IntFmtConfig[ifmt]) begin : active_format
      always_comb begin : special_results
        automatic logic [INT_WIDTH-1:0] special_res;

        // Default is overflow to positive max, which is 2**INT_WIDTH-1 or 2**(INT_WIDTH-1)-1
        special_res[INT_WIDTH-2:0] = '1;       // alone yields 2**(INT_WIDTH-1)-1
        special_res[INT_WIDTH-1]   = op_mod_q; // for unsigned casts yields 2**INT_WIDTH-1

        // Negative special case (except for nans) tie to -max or 0
        if (fmt_sign[src_fmt_q] && !info_q[src_fmt_q].is_nan)
          special_res = ~special_res;

        // Initialize special result with sign-extension
        ifmt_special_result[ifmt]                = '{default: special_res[INT_WIDTH-1]};
        ifmt_special_result[ifmt][INT_WIDTH-1:0] = special_res;
      end
    end else begin : inactive_format
      assign ifmt_special_result[ifmt] = 'X;
    end
  end

  // Detect special case from source format (inf, nan, overflow, nan-boxing or negative unsigned)
  assign int_result_is_special = info_q[src_fmt_q].is_nan | info_q[src_fmt_q].is_inf |
                                 of_before_round | ~info_q[src_fmt_q].is_boxed |
                                 (fmt_sign[src_fmt_q] & op_mod_q & ~rounded_int_res_zero);

  // All integer special cases are invalid
  assign int_special_status = '{NV: 1'b1, default: 1'b0};

  // Assemble result according to destination format
  assign int_special_result = ifmt_special_result[int_fmt_q]; // destination format

  // -----------------
  // Result selection
  // -----------------
  fpnew_pkg::status_t int_regular_status, fp_regular_status;

  logic [WIDTH-1:0]   fp_result, int_result;
  fpnew_pkg::status_t fp_status, int_status;

  assign fp_regular_status = '{
    NV: src_is_int & (of_before_round | of_after_round), // overflow is invalid for I2F casts
    DZ: 1'b0, // no divisions
    OF: ~src_is_int & (of_before_round | of_after_round), // in F2F casts, the OF flag is raised
    UF: uf_after_round,
    NX: src_is_int ? (| fp_round_sticky_bits) // overflow is invalid in i2f
                   : (| fp_round_sticky_bits) | of_before_round | of_after_round
  };
  assign int_regular_status = '{NX: (| int_round_sticky_bits), default: 1'b0};

  assign fp_result  = fp_result_is_special  ? fp_special_result  : fmt_result[dst_fmt_q];
  assign fp_status  = fp_result_is_special  ? fp_special_status  : fp_regular_status;
  assign int_result = int_result_is_special ? int_special_result : rounded_int_res;
  assign int_status = int_result_is_special ? int_special_status : int_regular_status;

  // Final results for output pipeline
  logic [WIDTH-1:0]   result_d;
  fpnew_pkg::status_t status_d;
  logic               extension_bit;

  // Select output depending on special case detection
  assign result_d = dst_is_int ? int_result : fp_result;
  assign status_d = dst_is_int ? int_status : fp_status;

  // MSB of int result decides extension, otherwise NaN box
  assign extension_bit = dst_is_int ? int_result[WIDTH-1] : 1'b1;

  // ----------------
  // Output Pipeline
  // ----------------
  // Generate pipeline at output if needed
  if (PipeConfig==fpnew_pkg::AFTER) begin : output_pipline
    fpnew_pipe_out #(
      .Width       ( WIDTH       ),
      .NumPipeRegs ( NumPipeRegs ),
      .TagType     ( TagType     ),
      .AuxType     ( AuxType     )
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
