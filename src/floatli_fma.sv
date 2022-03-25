// Copyright 2019-2020 ETH Zurich and University of Bologna.
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

// Authors: Luca Bertaccini <lbertaccini@iis.ee.ethz.ch>
//          Stefan Mach <smach@iis.ee.ethz.ch>

// Reduced FMA+COMP+CAST
module floatli_fma #(
  parameter fpnew_pkg::fp_format_e   FpFormat    = fpnew_pkg::fp_format_e'(1),
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::AFTER,

  localparam int unsigned NUM_FORMATS            = fpnew_pkg::num_fp_formats(FpFormat),
  localparam int unsigned WIDTH                  = fpnew_pkg::fp_width(FpFormat) // do not change
) (
  input logic                      clk_i,
  input logic                      rst_ni,
  // Input signals
  input logic [2:0][WIDTH-1:0]     operands_i, // 3 operands
  input logic [2:0]                is_boxed_i, // 3 operands
  input fpnew_pkg::roundmode_e     rnd_mode_i,
  input fpnew_pkg::operation_e     op_i,
  input logic                      op_mod_i,
  input TagType                    tag_i,
  input AuxType                    aux_i,
  // Input Handshake
  input  logic                     in_valid_i,
  output logic                     in_ready_o,
  input  logic                     flush_i,

  input  fpnew_pkg::fp_format_e    src_fmt_i,  // cast
  input  fpnew_pkg::fp_format_e    dst_fmt_i,  // cast
  input  fpnew_pkg::int_format_e   int_fmt_i,  // cast
  // Output signals
  output logic [WIDTH-1:0]         result_o,
  output fpnew_pkg::status_t       status_o,
  output logic                     extension_bit_o,
  output fpnew_pkg::classmask_e    class_mask_o,     // non_comp
  output logic                     is_class_o,       // non_comp
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
  localparam int unsigned BIAS     = fpnew_pkg::bias(FpFormat);
  // Precision bits 'p' include the implicit bit
  localparam int unsigned PRECISION_BITS = MAN_BITS + 1;
  // The lower 2p+3 bits of the internal FMA result will be needed for leading-zero detection
  localparam int unsigned LOWER_SUM_WIDTH  = 2 * PRECISION_BITS + 3;
  localparam int unsigned LZC_RESULT_WIDTH = $clog2(LOWER_SUM_WIDTH);
  // Internal exponent width of FMA must accomodate all meaningful exponent values in order to avoid
  // datapath leakage. This is either given by the exponent bits or the width of the LZC result.
  // In most reasonable FP formats the internal exponent will be wider than the LZC result.
  localparam int unsigned EXP_WIDTH = unsigned'(fpnew_pkg::maximum(EXP_BITS + 2, LZC_RESULT_WIDTH));
  // Shift amount width: maximum internal mantissa size is 3p+3 bits
  localparam int unsigned SHIFT_AMOUNT_WIDTH = $clog2(3 * PRECISION_BITS + 3);

  // ---------------
  // CAST Constants
  // ---------------
  localparam fpnew_pkg::fmt_logic_t   FpFmtConfig  = fpnew_pkg::fp_config(FpFormat);
  localparam fpnew_pkg::ifmt_logic_t  IntFmtConfig = fpnew_pkg::int_config(FpFormat);
  localparam int unsigned NUM_INT_FORMATS = 2;  // INT32, INT64
  localparam int unsigned MAX_INT_WIDTH   = WIDTH;

  localparam int unsigned SUPER_EXP_BITS = EXP_BITS;
  localparam int unsigned SUPER_MAN_BITS = MAN_BITS;
  localparam int unsigned SUPER_BIAS     = 2**(EXP_BITS - 1) - 1;

  // The internal mantissa includes normal bit or an entire integer
  localparam int unsigned INT_MAN_WIDTH = fpnew_pkg::maximum(SUPER_MAN_BITS + 1, MAX_INT_WIDTH);
//  localparam int unsigned INT_MAN_WIDTH = 32;
  // If needed, there will be a LZC for renormalization
  localparam int unsigned CAST_LZC_RESULT_WIDTH = $clog2(INT_MAN_WIDTH);
  // The internal exponent must be able to represent the smallest denormal input value as signed
  // or the number of bits in an integer
  localparam int unsigned INT_EXP_WIDTH = fpnew_pkg::maximum($clog2(MAX_INT_WIDTH),
      fpnew_pkg::maximum(SUPER_EXP_BITS, $clog2(SUPER_BIAS + SUPER_MAN_BITS))) + 1;

  // ----------------
  // Type definition
  // ----------------
  typedef struct packed {
    logic                sign;
    logic [EXP_BITS-1:0] exponent;
    logic [MAN_BITS-1:0] mantissa;
  } fp_t;

  // FSM states
  enum logic [3:0] {FSM_IDLE, FSM_EXP_ADD, FSM_EXP_DIFF, FSM_MANTISSA_PROD_ADDEND_SHIFT, FSM_SUM,
                    FSM_COMPLEMENT_SUM, FSM_NORMALIZATION, FSM_ROUNDING, FSM_CAST_SHIFT,
                    FSM_CAST_DEST_SHIFT, FSM_CAST_DEST_INV, FSM_CAST_ROUNDING} next_state, current_state;

  logic [2:0]            is_boxed_q, is_boxed_d; // 3 operands
  fpnew_pkg::roundmode_e rnd_mode_q, rnd_mode_d;
  fpnew_pkg::operation_e op_q, op_d;
  logic [5:0]            tmp_d, tmp_q;

  logic                  op_mod_q, op_mod_d;
  TagType                tag_q, tag_d;
  AuxType                aux_q, aux_d;
  logic                  flush_q, flush_d;

  fp_t [2:0]             operands_d;
  fp_t [2:0]             operands_q;

  fpnew_pkg::fp_info_t [2:0] info_d, info_tmp;
  logic                      info_a_is_normal_d, info_c_is_normal_d, info_b_is_normal_d;
  logic                      info_a_is_normal_q, info_c_is_normal_q, info_b_is_normal_q;

  fpnew_pkg::fp_format_e    src_fmt_d, src_fmt_q;  // cast
  fpnew_pkg::fp_format_e    dst_fmt_d, dst_fmt_q;  // cast
  fpnew_pkg::int_format_e   int_fmt_d, int_fmt_q;  // cast

  logic enable_reg;

  // registers <-- inputs
  always_ff @(posedge clk_i or negedge rst_ni) begin : input_regs
    if(~rst_ni) begin
      rnd_mode_q          <= fpnew_pkg::RNE;
      op_q                <= fpnew_pkg::FMADD;
      op_mod_q            <= '0;
      tag_q               <= '0;
      aux_q               <= '0;
      flush_q             <= '0;
      operands_q          <= '0;
      is_boxed_q          <= '0;
      info_a_is_normal_q  <= '0;
      info_b_is_normal_q  <= '0;
      info_c_is_normal_q  <= '0;
      src_fmt_q           <= fpnew_pkg::FP32;
      dst_fmt_q           <= fpnew_pkg::FP32;
      int_fmt_q           <= fpnew_pkg::INT32;
    end else if (enable_reg) begin
      rnd_mode_q          <= rnd_mode_d;
      op_q                <= op_d;
      op_mod_q            <= op_mod_d;
      tag_q               <= tag_d;
      aux_q               <= aux_d;
      flush_q             <= flush_d;
      operands_q          <= operands_d;
      is_boxed_q          <= is_boxed_d;
      info_a_is_normal_q  <= info_a_is_normal_d;
      info_b_is_normal_q  <= info_b_is_normal_d;
      info_c_is_normal_q  <= info_c_is_normal_d;
      src_fmt_q           <= src_fmt_d;
      dst_fmt_q           <= dst_fmt_d;
      int_fmt_q           <= int_fmt_d;
    end
  end

  logic signed [EXP_WIDTH-1:0]   exponent_addend, exponent_product_new, exponent_difference_new;
  logic signed [EXP_WIDTH-1:0]   exponent_product_old, exponent_difference_old;
  logic [EXP_WIDTH-1:0]          exponent_product_tmp;

  logic signed [EXP_WIDTH-1:0]   normalized_exponent_old;

  logic signed [EXP_WIDTH-1:0]   tentative_exponent;

  logic [SHIFT_AMOUNT_WIDTH-1:0] addend_shamt;

  logic [PRECISION_BITS-1:0]     mantissa_a, mantissa_b, mantissa_c;
  logic [2*PRECISION_BITS-1:0]   product_new;             // the p*p product is 2p bits wide
  logic [3*PRECISION_BITS+3:0]   product_shifted;      // addends are 3p+4 bit wide (including G/R)

  logic [3*PRECISION_BITS+3:0]   addend_after_shift_d; // upper 3p+4 bits are needed to go on
  logic [3*PRECISION_BITS+3:0]   addend_after_shift_q;


  logic [PRECISION_BITS-1:0]     addend_sticky_bits;  // up to p bit of shifted addend are sticky
  logic                          sticky_before_add;   // they are compressed into a single sticky bit
  logic [3*PRECISION_BITS+3:0]   addend_shifted;      // addends are 3p+4 bit wide (including G/R)
  logic [3*PRECISION_BITS+3:0]   addend_shifted_new, addend_shifted_old;
  logic                          inject_carry_in;     // inject carry for subtractions if needed

  logic [3*PRECISION_BITS+4:0]   sum_raw;   // added one bit for the carry
  logic                          sum_carry; // observe carry bit from sum for sign fixing
  logic [3*PRECISION_BITS+3:0]   sum;       // discard carry as sum won't overflow
  logic                          final_sign;

  logic        [LOWER_SUM_WIDTH-1:0]    sum_lower;            // lower 2p+3 bits of sum are searched
  logic        [LZC_RESULT_WIDTH-1:0]   leading_zero_count;     // the number of leading zeroes
  logic signed [LZC_RESULT_WIDTH:0]     leading_zero_count_sgn; // signed leading-zero count
  logic                                 lzc_zeroes;             // in case only zeroes found

  logic        [SHIFT_AMOUNT_WIDTH-1:0] norm_shamt; // Normalization shift amount
  logic signed [EXP_WIDTH-1:0]          normalized_exponent_new;

  logic [3*PRECISION_BITS+4:0]  sum_shifted, sum_shifted_tmp; // result after first normalization
                                                              // shift
  logic [PRECISION_BITS:0]      final_mantissa;    // final mantissa before rounding with round bit
  logic [2*PRECISION_BITS+2:0]  sum_sticky_bits;   // remaining 2p+3 sticky bits after normalization
  logic                         sticky_after_norm; // sticky bit after normalization

  logic signed [EXP_WIDTH-1:0]  final_exponent;

  // Rounding and classification
  logic                         pre_round_sign;
  logic [EXP_BITS-1:0]          pre_round_exponent;
  logic [MAN_BITS-1:0]          pre_round_mantissa;
  logic [EXP_BITS+MAN_BITS-1:0] pre_round_abs; // absolute value of result before rounding
  logic [1:0]                   round_sticky_bits;

  logic                         of_before_round, of_after_round; // overflow
  logic                         uf_before_round, uf_after_round; // underflow
  logic                         result_zero;

  logic                         rounded_sign;
  logic [EXP_BITS+MAN_BITS-1:0] rounded_abs; // absolute value of result after rounding

  logic                         round_up; // Rounding decision

  // Counter for multicycle multiplication
  logic [$clog2(PRECISION_BITS)-1:0] mul_count_d, mul_count_q;
  logic [2*PRECISION_BITS-1:0]       product_old;
  // Register for mantissa_b
  logic [PRECISION_BITS-1:0]         mantissa_b_old, mantissa_b_new;
  logic                              mantissa_b_msb_q, mantissa_b_msb_d;

  logic [PRECISION_BITS-1:0]         mantissa_c_tmp;

  // Large Adder
  logic [(3*PRECISION_BITS+4)/3:0]   addend_a;
  logic [(3*PRECISION_BITS+4)/3:0]   addend_b;
  logic                              carry_in;
  logic [(3*PRECISION_BITS+4)/3+1:0] adder_result;

  // Exponent adder
  logic [EXP_WIDTH-1:0]              exp_a;
  logic [EXP_WIDTH-1:0]              exp_b;
  logic                              exp_carry_in;
  logic [EXP_WIDTH-1:0]              exp_adder_result;

  // Reduced shifter
  logic [(3*PRECISION_BITS+4)/3:0]   shift_in;
  logic [SHIFT_AMOUNT_WIDTH-1:0]     shift_amount;
  logic [4*PRECISION_BITS+3:0]       shift_out;
  // Temporal signals used for the shift on mantissa_c
  logic [4*PRECISION_BITS+3:0]       shift_out_tmp, reversed_shift_out_tmp;

  // Counter for multicycle shift
  logic                              shift_count_d, shift_count_q;

  // Reduced multiplier
  logic [PRECISION_BITS-1:0]         factor_a;
  logic [1:0]                        factor_b;
  logic [PRECISION_BITS+1:0]         prod;

  logic [PRECISION_BITS+1:0]         partial_product;

  logic                              msb_add_d, msb_add_q;
  logic                              carry_add_d, carry_add_q;

  // Counter for multicycle addition
  logic [1:0]                        add_count_d, add_count_q;

  logic [1:0]                        tmp_shift_d, tmp_shift_q;

  logic [EXP_WIDTH-EXP_BITS-1:0]     exp_prod_msbs_q, exp_prod_msbs_d;
  logic [EXP_WIDTH-EXP_BITS-1:0]     exp_diff_msbs_q, exp_diff_msbs_d;
  logic [EXP_WIDTH-EXP_BITS-1:0]     normalized_exponent_msbs_q, normalized_exponent_msbs_d;

  logic [4*PRECISION_BITS+3:0]       mantissa_c_after_shift_old;
  logic [WIDTH-1:0]                  are_equal;

  assign exponent_product_old    = {exp_prod_msbs_q ,operands_q[0].exponent};
  assign exponent_difference_old = {exp_diff_msbs_q ,operands_q[1].exponent};
  assign normalized_exponent_old = addend_after_shift_q[EXP_WIDTH-1:0];
  assign mantissa_b_old          = {mantissa_b_msb_q ,operands_q[1].mantissa};
  assign {addend_shifted_old, addend_sticky_bits} = {tmp_shift_q,
                                                     addend_after_shift_q[3*PRECISION_BITS+3:2*PRECISION_BITS],
                                                     operands_q[2].mantissa, mantissa_b_old, operands_q[0].mantissa};
  assign product_old             = addend_after_shift_q[2*PRECISION_BITS-1:0];

  assign mantissa_c_after_shift_old = {addend_after_shift_q, mantissa_b_old};

  //non-comp
  logic               operands_equal, operand_a_smaller;

  fp_t                sgnj_result;
  fpnew_pkg::status_t sgnj_status;
  logic               sgnj_extension_bit;

  logic               sign_a, sign_b; // internal signs

  fp_t                minmax_result;
  fpnew_pkg::status_t minmax_status;
  logic               minmax_extension_bit;

  fp_t                cmp_result;
  fpnew_pkg::status_t cmp_status;
  logic               cmp_extension_bit;

  // -----------------
  // Input processing
  // -----------------
  logic               src_is_int, dst_is_int; // if 0, it's a float
  logic               is_int64; // if 0, it's INT32

  assign src_is_int = (op_q == fpnew_pkg::I2F);
  assign dst_is_int = (op_q == fpnew_pkg::F2I);
  assign is_int64   = (int_fmt_q == fpnew_pkg::INT64) ? 1'b1 : 1'b0;

  logic [INT_MAN_WIDTH-1:0]                         encoded_mant; // input mantissa with implicit bit

  logic        [NUM_FORMATS-1:0]                    fmt_sign;
  logic signed [NUM_FORMATS-1:0][INT_EXP_WIDTH-1:0] fmt_exponent;
  logic        [NUM_FORMATS-1:0][INT_MAN_WIDTH-1:0] fmt_mantissa;
  logic signed [NUM_FORMATS-1:0][INT_EXP_WIDTH-1:0] fmt_shift_compensation; // for LZC

  fpnew_pkg::fp_info_t [NUM_FORMATS-1:0]            info_cast;

  logic [NUM_INT_FORMATS-1:0][INT_MAN_WIDTH-1:0]    ifmt_input_val;
  logic                                             int_sign;
  logic [INT_MAN_WIDTH-1:0]                         int_value;

  // FP Input initialization
  for (genvar fmt = 0; fmt < NUM_FORMATS; fmt++) begin : fmt_init_inputs
    // Set up some constants
    localparam int unsigned FP_WIDTH = fpnew_pkg::fp_width(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned EXP_BITS = fpnew_pkg::exp_bits(fpnew_pkg::fp_format_e'(fmt));
    localparam int unsigned MAN_BITS = fpnew_pkg::man_bits(fpnew_pkg::fp_format_e'(fmt));

    if (FpFmtConfig[fmt]) begin : active_format
      // Classify input
      fpnew_classifier #(
        .FpFormat    ( fpnew_pkg::fp_format_e'(fmt) ),
        .NumOperands ( 1                            )
      ) i_fpnew_classifier_cast (
        .operands_i ( operands_q[0][FP_WIDTH-1:0] ),
//        .operands_i ( operands_q[0]                 ),
        .is_boxed_i ( is_boxed_q[fmt]               ),
        .info_o     ( info_cast[fmt]                )
      );

      assign fmt_sign[fmt]     = operands_q[0][FP_WIDTH-1];
      assign fmt_exponent[fmt] = signed'({1'b0, operands_q[0][MAN_BITS+:EXP_BITS]});
      assign fmt_mantissa[fmt] = {info_cast[fmt].is_normal, operands_q[0][MAN_BITS-1:0]}; // zero pad
      // Compensation for the difference in mantissa widths used for leading-zero count
      assign fmt_shift_compensation[fmt] = signed'(INT_MAN_WIDTH - 1 - MAN_BITS);
    end else begin : inactive_format
      assign info_cast[fmt]              = '{default: fpnew_pkg::DONT_CARE}; // format disabled
      assign fmt_sign[fmt]               = fpnew_pkg::DONT_CARE;             // format disabled
      assign fmt_exponent[fmt]           = '{default: fpnew_pkg::DONT_CARE}; // format disabled
      assign fmt_mantissa[fmt]           = '{default: fpnew_pkg::DONT_CARE}; // format disabled
      assign fmt_shift_compensation[fmt] = '{default: fpnew_pkg::DONT_CARE}; // format disabled
    end
  end

  // Sign-extend INT input
  always_comb begin
    // Set up some constants
    localparam int unsigned INT32_WIDTH = 32;
    // sign-extend value only if it's signed
    ifmt_input_val[0]                     = '{default: operands_q[0][INT32_WIDTH-1] & ~op_mod_q};
    ifmt_input_val[0][INT32_WIDTH-1:0]    = operands_q[0][INT32_WIDTH-1:0];
  end

  if (FpFormat == fpnew_pkg::fp_format_e'(1)) begin
    always_comb begin
      localparam int unsigned INT64_WIDTH = 64;
      // sign-extend value only if it's signed
      ifmt_input_val[1]                   = '{default: operands_q[0][INT64_WIDTH-1] & ~op_mod_q};
      ifmt_input_val[1][INT64_WIDTH-1:0]  = operands_q[0][INT64_WIDTH-1:0];
    end
  end else begin : inactive_format
    assign ifmt_input_val[1]                   = '{default: fpnew_pkg::DONT_CARE}; // format disabled
  end

  // Construct input mantissa from integer
  assign int_value    = ifmt_input_val[is_int64];
  assign int_sign     = int_value[INT_MAN_WIDTH-1] & ~op_mod_q; // only signed ints are negative

  assign encoded_mant = src_is_int ? operands_q[2] : fmt_mantissa[src_fmt_q];

  // --------------
  // Normalization
  // --------------
  logic signed [INT_EXP_WIDTH-1:0] src_bias;      // src format bias
  logic signed [INT_EXP_WIDTH-1:0] src_exp;       // src format exponent (biased)
  logic signed [INT_EXP_WIDTH-1:0] src_subnormal; // src is subnormal
  logic signed [INT_EXP_WIDTH-1:0] src_offset;    // src offset within mantissa

//  assign src_bias      = signed'(fpnew_pkg::bias(src_fmt_q));
  assign src_bias      = (src_fmt_q) ? signed'(1023) : signed'(127);
  assign src_exp       = fmt_exponent[src_fmt_q];
  assign src_subnormal = signed'({1'b0, info_cast[src_fmt_q].is_subnormal});
  assign src_offset    = fmt_shift_compensation[src_fmt_q];

  logic                            input_sign;   // input sign
  logic signed [INT_EXP_WIDTH-1:0] input_exp;    // unbiased true exponent
  // logic        [INT_MAN_WIDTH-1:0] input_mant;   // normalized input mantissa
  logic                            mant_is_zero, mant_is_zero_d, mant_is_zero_q; // for integer zeroes

  logic signed [INT_EXP_WIDTH-1:0] fp_input_exp;
  logic signed [INT_EXP_WIDTH-1:0] int_input_exp;

  // Input mantissa needs to be normalized
  logic [CAST_LZC_RESULT_WIDTH-1:0] renorm_shamt;     // renormalization shift amount
  logic [CAST_LZC_RESULT_WIDTH:0]   renorm_shamt_sgn; // signed form for calculations

  // Leading-zero counter is needed for renormalization
  lzc #(
    .WIDTH ( INT_MAN_WIDTH ),
    .MODE  ( 1             ) // MODE = 1 counts leading zeroes
  ) i_lzc_cast (
    .in_i    ( encoded_mant ),
    .cnt_o   ( renorm_shamt ),
    .empty_o ( mant_is_zero )
  );
  assign renorm_shamt_sgn = signed'({1'b0, renorm_shamt});

  // Get the sign from the proper source
  assign input_sign = src_is_int ? int_sign : fmt_sign[src_fmt_q];

  // Unbias exponent and compensate for shift
  assign fp_input_exp  = signed'(src_exp + src_subnormal - src_bias -
                                 renorm_shamt_sgn + src_offset); // compensate for shift
  assign int_input_exp = signed'(INT_MAN_WIDTH - 1 - renorm_shamt_sgn);

  assign input_exp     = src_is_int ? int_input_exp : fp_input_exp;

  logic signed [INT_EXP_WIDTH-1:0] destination_exp;  // re-biased exponent for destination

  // Rebias the exponent
  //  assign destination_exp = input_exp + signed'(fpnew_pkg::bias(dst_fmt_q));
  assign destination_exp = (dst_fmt_q == fpnew_pkg::FP64) ? (input_exp + signed'(1023)) : (input_exp + signed'(127));

  // --------
  // Casting
  // --------
  logic [INT_EXP_WIDTH-1:0] final_exp;        // after eventual adjustments

  logic [2*INT_MAN_WIDTH:0]  preshift_mant, preshift_mant_tmp;    // mantissa before final shift
  logic [2*INT_MAN_WIDTH:0]  destination_mant_tmp; // mantissa from shifter, with rnd bit
  logic [SUPER_MAN_BITS-1:0] final_mant;       // mantissa after adjustments
  logic [MAX_INT_WIDTH-1:0]  final_int;        // integer shifted in position

  logic [$clog2(INT_MAN_WIDTH+1)-1:0] denorm_shamt; // shift amount for denormalization

  logic [1:0] fp_round_sticky_bits_cast, int_round_sticky_bits_cast, round_sticky_bits_cast;
  logic       of_before_round_cast, uf_before_round_cast;

  // Perform adjustments to mantissa and exponent
  always_comb begin : cast_value
    // Default assignment
    final_exp       = unsigned'(destination_exp); // take exponent as is, only look at lower bits
    preshift_mant   = '0;  // initialize mantissa container with zeroes
    denorm_shamt    = SUPER_MAN_BITS - fpnew_pkg::man_bits(dst_fmt_q); // right of mantissa
    of_before_round_cast = 1'b0;
    uf_before_round_cast = 1'b0;

    // Place mantissa to the left of the shifter
//    preshift_mant = input_mant << (INT_MAN_WIDTH + 1);
    preshift_mant = operands_q[1][INT_MAN_WIDTH-1:0] << (INT_MAN_WIDTH + 1);

    // Handle INT casts
    if (dst_is_int) begin
      // By default right shift mantissa to be an integer
      denorm_shamt = unsigned'(MAX_INT_WIDTH - 1 - input_exp);
      // overflow: when converting to unsigned the range is larger by one
      if (input_exp >= signed'(fpnew_pkg::int_width(int_fmt_q) - 1 + op_mod_q)) begin
        denorm_shamt    = '0; // prevent shifting
        of_before_round_cast = 1'b1;
      // underflow
      end else if (input_exp < -1) begin
        denorm_shamt    = MAX_INT_WIDTH + 1; // all bits go to the sticky
        uf_before_round_cast = 1'b1;
      end
    // Handle FP over-/underflows
    end else begin
      // Overflow or infinities (for proper rounding)
      if (dst_fmt_q) begin //FP64
        if ((destination_exp >= 2047) ||
            (~src_is_int && info_cast[1].is_inf)) begin
          final_exp       = unsigned'(2046); // largest normal value
          preshift_mant   = '1;                           // largest normal value and RS bits set
          of_before_round_cast = 1'b1;
        // Denormalize underflowing values
        end else if (destination_exp < 1 &&
                     destination_exp >= -signed'(52)) begin
          final_exp       = '0; // denormal result
          denorm_shamt    = unsigned'(denorm_shamt + 1 - destination_exp); // adjust right shifting
          uf_before_round_cast = 1'b1;
        // Limit the shift to retain sticky bits
        end else if (destination_exp < -signed'(52)) begin
          final_exp       = '0; // denormal result
          denorm_shamt    = unsigned'(denorm_shamt + 2 + 52); // to sticky
          uf_before_round_cast = 1'b1;
        end
      end else begin
        if ((destination_exp >= 255) ||
            (~src_is_int && info_cast[0].is_inf)) begin
          final_exp       = unsigned'(254); // largest normal value
          preshift_mant   = '1;                           // largest normal value and RS bits set
          of_before_round_cast = 1'b1;
        // Denormalize underflowing values
        end else if (destination_exp < 1 &&
                     destination_exp >= -signed'(23)) begin
          final_exp       = '0; // denormal result
          denorm_shamt    = unsigned'(denorm_shamt + 1 - destination_exp); // adjust right shifting
          uf_before_round_cast = 1'b1;
        // Limit the shift to retain sticky bits
        end else if (destination_exp < -signed'(23)) begin
          final_exp       = '0; // denormal result
          denorm_shamt    = unsigned'(denorm_shamt + 2 + 23); // to sticky
          uf_before_round_cast = 1'b1;
        end
      end
    end
  end

  localparam NUM_FP_STICKY  = 2 * INT_MAN_WIDTH - SUPER_MAN_BITS - 1; // removed mantissa, 1. and R
  localparam NUM_INT_STICKY = 2 * INT_MAN_WIDTH - MAX_INT_WIDTH; // removed int and R

  // Extract final mantissa and round bit, discard the normal bit (for FP)
  assign {final_mant, fp_round_sticky_bits_cast[1]} =
      addend_after_shift_q[2*INT_MAN_WIDTH-1-:SUPER_MAN_BITS+1];
  assign {final_int, int_round_sticky_bits_cast[1]} = addend_after_shift_q[2*INT_MAN_WIDTH-:MAX_INT_WIDTH+1];
  // Collapse sticky bits
  assign fp_round_sticky_bits_cast[0]  = (| {addend_after_shift_q[NUM_FP_STICKY-1:0]});
  assign int_round_sticky_bits_cast[0] = (| {addend_after_shift_q[NUM_INT_STICKY-1:0]});

  // select RS bits for destination operation
  assign round_sticky_bits_cast = dst_is_int ? int_round_sticky_bits_cast : fp_round_sticky_bits_cast;

  // Classify input
  fpnew_classifier #(
    .FpFormat    ( FpFormat ),
    .NumOperands ( 3        )
    ) i_class_inputs (
    .operands_i ( operands_q ),
    .is_boxed_i ( is_boxed_q ),
    .info_o     ( info_tmp   )
  );

  fp_t                 operand_a, operand_b, operand_c;
  fpnew_pkg::fp_info_t info_a,    info_b,    info_c;

  logic signed [EXP_WIDTH-1:0]  exponent_a, exponent_b, exponent_c;

  // Zero-extend exponents into signed container - implicit width extension
  assign exponent_a = signed'({1'b0, operand_a.exponent});
  assign exponent_b = signed'({1'b0, operand_b.exponent});
  assign exponent_c = signed'({1'b0, operand_c.exponent});

  // Operation selection and operand adjustment
  // | \c op_q  | \c op_mod_q | Operation Adjustment
  // |:--------:|:-----------:|---------------------
  // | FMADD    | \c 0        | FMADD: none
  // | FMADD    | \c 1        | FMSUB: Invert sign of operand C
  // | FNMSUB   | \c 0        | FNMSUB: Invert sign of operand A
  // | FNMSUB   | \c 1        | FNMADD: Invert sign of operands A and C
  // | ADD      | \c 0        | ADD: Set operand A to +1.0
  // | ADD      | \c 1        | SUB: Set operand A to +1.0, invert sign of operand C
  // | MUL      | \c 0        | MUL: Set operand C to +0.0
  // | *others* | \c -        | *invalid*
  // \note \c op_mod_q always inverts the sign of the addend.
  always_comb begin : op_select
    info_a = info_tmp[0];
    info_b = info_tmp[1];
    info_c = info_tmp[2];
    operand_a = operands_q[0];
    operand_b = operands_q[1];
    operand_c = operands_q[2];

    // op_mod_q inverts sign of operand C
    operand_c.sign = operand_c.sign ^ op_mod_q;

    unique case (op_q)
      fpnew_pkg::FMADD:  ; // do nothing
      fpnew_pkg::FNMSUB: operand_a.sign = ~operand_a.sign; // invert sign of product
      fpnew_pkg::ADD: begin // Set multiplicand to +1
        operand_a = '{sign: 1'b0, exponent: BIAS, mantissa: '0};
        info_a    = '{is_normal: 1'b1, is_boxed: 1'b1, default: 1'b0}; //normal, boxed value.
      end
      fpnew_pkg::MUL: begin // Set addend to -0 (for proper rounding with RDN)
        operand_c = '{sign: 1'b1, exponent: '0, mantissa: '0};
        info_c    = '{is_zero: 1'b1, is_boxed: 1'b1, default: 1'b0}; //zero, boxed value.
      end
      fpnew_pkg::CMP:  ; // do nothing
      fpnew_pkg::SGNJ:  ; // do nothing
      fpnew_pkg::MINMAX:  ; // do nothing
      fpnew_pkg::CLASSIFY:  ; // do nothing
      default: begin // propagate don't cares
        operand_a  = '{default: fpnew_pkg::DONT_CARE};
        operand_b  = '{default: fpnew_pkg::DONT_CARE};
        operand_c  = '{default: fpnew_pkg::DONT_CARE};
        info_a     = '{default: fpnew_pkg::DONT_CARE};
        info_b     = '{default: fpnew_pkg::DONT_CARE};
        info_c     = '{default: fpnew_pkg::DONT_CARE};
      end
    endcase
  end

  assign info_d[0] = info_a;
  assign info_d[1] = info_b;
  assign info_d[2] = info_c;

  // ---------------------
  // Input classification
  // ---------------------
  logic any_operand_inf;
  logic any_operand_nan;
  logic signalling_nan;
  logic effective_subtraction;
  logic tentative_sign;

  // Reduction for special case handling
  assign any_operand_inf = (| {info_a.is_inf,        info_b.is_inf,        info_c.is_inf});
  assign any_operand_nan = (| {info_a.is_nan,        info_b.is_nan,        info_c.is_nan});
  assign signalling_nan  = (| {info_a.is_signalling, info_b.is_signalling, info_c.is_signalling});
  // Effective subtraction in FMA occurs when product and addend signs differ
  assign effective_subtraction = (op_q == fpnew_pkg::MUL) //.TODO (lbertaccini): test
                                    ? 1'b0 : operand_a.sign ^ operand_b.sign ^ operand_c.sign;
  // The tentative sign of the FMA shall be the sign of the product
  assign tentative_sign = operand_a.sign ^ operand_b.sign;

  // ----------------------
  // Special case handling
  // ----------------------
  fp_t                special_result;
  fpnew_pkg::status_t special_status;
  logic               result_is_special;

  always_comb begin : special_cases
    // Default assignments
    special_result    = '{sign: 1'b0, exponent: '1, mantissa: 2**(MAN_BITS-1)}; // canonical qNaN
    special_status    = '0;
    result_is_special = 1'b0;

    // Handle potentially mixed nan & infinity input => important for the case where infinity and
    // zero are multiplied and added to a qnan.
    // RISC-V mandates raising the NV exception in these cases:
    // (inf * 0) + c or (0 * inf) + c INVALID, no matter c (even quiet NaNs)
    if ((info_a.is_inf && info_b.is_zero) || (info_a.is_zero && info_b.is_inf)) begin
      result_is_special = 1'b1; // bypass FMA, output is the canonical qNaN
      special_status.NV = 1'b1; // invalid operation
    // NaN Inputs cause canonical quiet NaN at the output and maybe invalid OP
    end else if (any_operand_nan) begin
      result_is_special = 1'b1;           // bypass FMA, output is the canonical qNaN
      special_status.NV = signalling_nan; // raise the invalid operation flag if signalling
    // Special cases involving infinity
    end else if (any_operand_inf) begin
      result_is_special = 1'b1; // bypass FMA
      // Effective addition of opposite infinities (±inf - ±inf) is invalid!
      if ((info_a.is_inf || info_b.is_inf) && info_c.is_inf && effective_subtraction)
        special_status.NV = 1'b1; // invalid operation
      // Handle cases where output will be inf because of inf product input
      else if (info_a.is_inf || info_b.is_inf) begin
        // Result is infinity with the sign of the product
        special_result    = '{sign: operand_a.sign ^ operand_b.sign, exponent: '1, mantissa: '0};
      // Handle cases where the addend is inf
      end else if (info_c.is_inf) begin
        // Result is inifinity with sign of the addend (= operand_c)
        special_result    = '{sign: operand_c.sign, exponent: '1, mantissa: '0};
      end
    end
  end

  assign cmp_extension_bit    = 1'b0; // Comparisons always produce booleans in integer registers
  assign minmax_extension_bit = 1'b1; // NaN-box as result is always a float value
  assign sgnj_status          = '0;   // sign injections never raise exceptions
  // op_mod_q enables integer sign-extension of result (for storing to integer regfile)
  assign sgnj_extension_bit = op_mod_q ? sgnj_result.sign : 1'b1;

    // ----------------------------
  // Rounding and classification
  // ----------------------------
  logic [WIDTH-1:0] pre_round_abs_cast;  // absolute value of result before rnd
  logic             of_after_round_cast; // overflow
  logic             uf_after_round_cast; // underflow

  logic [NUM_FORMATS-1:0][WIDTH-1:0] fmt_pre_round_abs; // per format
  logic [NUM_FORMATS-1:0]            fmt_of_after_round;
  logic [NUM_FORMATS-1:0]            fmt_uf_after_round;

//  logic [NUM_INT_FORMATS-1:0][WIDTH-1:0] ifmt_pre_round_abs; // per format
  logic [NUM_INT_FORMATS-1:0][WIDTH-1:0] ifmt_pre_round_abs; // per format

  logic             rounded_sign_cast;
  logic [WIDTH-1:0] rounded_abs_cast; // absolute value of result after rounding
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
      assign fmt_pre_round_abs[fmt] = '{default: fpnew_pkg::DONT_CARE};
    end
  end

  // Sign-extend integer result
  always_comb begin
    // Set up some constants
    localparam int unsigned INT32_WIDTH = 32;
      // sign-extend reusult
    ifmt_pre_round_abs[0]                = '{default: final_int[INT32_WIDTH-1]};
    ifmt_pre_round_abs[0][INT32_WIDTH-1:0] = final_int[INT32_WIDTH-1:0];
  end

  if (FpFormat == fpnew_pkg::fp_format_e'(1)) begin
    localparam int unsigned INT64_WIDTH = 64;
    // sign-extend value only if it's signed
    assign ifmt_pre_round_abs[1][INT64_WIDTH-1:0] = final_int[INT64_WIDTH-1:0];
  end else begin : inactive_format_ifmt_pre_round_abs
    assign ifmt_pre_round_abs[1]           = '{default: fpnew_pkg::DONT_CARE}; // format disabled
  end

  // Select output with destination format and operation
  assign pre_round_abs_cast = dst_is_int ? ifmt_pre_round_abs[is_int64] : fmt_pre_round_abs[dst_fmt_q];

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
        fmt_uf_after_round[fmt] = rounded_abs_cast[EXP_BITS+MAN_BITS-1:MAN_BITS] == '0; // denormal
        fmt_of_after_round[fmt] = rounded_abs_cast[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // inf exp.

        // Assemble regular result, nan box short ones. Int zeroes need to be detected`
        fmt_result[fmt]               = '1;
        fmt_result[fmt][FP_WIDTH-1:0] = src_is_int & mant_is_zero_q
                                        ? '0
                                        : {rounded_sign_cast, rounded_abs_cast[EXP_BITS+MAN_BITS-1:0]};
      end
    end else begin : inactive_format
      assign fmt_uf_after_round[fmt] = fpnew_pkg::DONT_CARE;
      assign fmt_of_after_round[fmt] = fpnew_pkg::DONT_CARE;
      assign fmt_result[fmt]         = '{default: fpnew_pkg::DONT_CARE};
    end
  end

  // Classification after rounding select by destination format
  assign uf_after_round_cast = fmt_uf_after_round[dst_fmt_q];
  assign of_after_round_cast = fmt_of_after_round[dst_fmt_q];

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
        special_res = info_cast[src_fmt_q].is_zero
                      ? input_sign << FP_WIDTH-1 // signed zero
                      : {1'b0, QNAN_EXPONENT, QNAN_MANTISSA}; // qNaN

        // Initialize special result with ones (NaN-box)
        fmt_special_result[fmt]               = '1;
        fmt_special_result[fmt][FP_WIDTH-1:0] = special_res;
      end
    end else begin : inactive_format
      assign fmt_special_result[fmt] = '{default: fpnew_pkg::DONT_CARE};
    end
  end

  // Detect special case from source format, I2F casts don't produce a special result
  assign fp_result_is_special = ~src_is_int & (info_cast[src_fmt_q].is_zero |
                                                 info_cast[src_fmt_q].is_nan |
                                                 ~info_cast[src_fmt_q].is_boxed);

  // Signalling input NaNs raise invalid flag, otherwise no flags set
  assign fp_special_status = '{NV: info_cast[src_fmt_q].is_signalling, default: 1'b0};

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
  always_comb begin
    // Set up some constants
    localparam int unsigned INT32_WIDTH = 32;
    automatic logic [INT32_WIDTH-1:0] special_res;

    // Default is overflow to positive max, which is 2**INT_WIDTH-1 or 2**(INT_WIDTH-1)-1
    special_res[INT32_WIDTH-2:0] = '1;       // alone yields 2**(INT_WIDTH-1)-1
    special_res[INT32_WIDTH-1]   = op_mod_q; // for unsigned casts yields 2**INT_WIDTH-1

    // Negative special case (except for nans) tie to -max or 0
    if (input_sign && !info_cast[src_fmt_q].is_nan)
      special_res = ~special_res;

    // Initialize special result with sign-extension
    ifmt_special_result[0]                = '{default: special_res[INT32_WIDTH-1]};
    ifmt_special_result[0][INT32_WIDTH-1:0] = special_res;
  end

  if (FpFormat == fpnew_pkg::fp_format_e'(1)) begin
    always_comb begin
      localparam int unsigned INT64_WIDTH = 64;
      automatic logic [INT64_WIDTH-1:0] special_res64;

      // Default is overflow to positive max, which is 2**INT_WIDTH-1 or 2**(INT_WIDTH-1)-1
      special_res64[INT64_WIDTH-2:0] = '1;       // alone yields 2**(INT_WIDTH-1)-1
      special_res64[INT64_WIDTH-1]   = op_mod_q; // for unsigned casts yields 2**INT_WIDTH-1

      // Negative special case (except for nans) tie to -max or 0
      if (input_sign && !info_cast[src_fmt_q].is_nan)
        special_res64 = ~special_res64;

      // Initialize special result with sign-extension
      ifmt_special_result[1]                = '{default: special_res64[INT64_WIDTH-1]};
      ifmt_special_result[1][INT64_WIDTH-1:0] = special_res64;
    end
  end else begin : inactive_format_ifmt_special_result
    assign ifmt_special_result[1] = '{default: fpnew_pkg::DONT_CARE};
  end

  // Detect special case from source format (inf, nan, overflow, nan-boxing or negative unsigned)
  assign int_result_is_special = info_cast[src_fmt_q].is_nan | info_cast[src_fmt_q].is_inf |
                                 of_before_round_cast | ~info_cast[src_fmt_q].is_boxed |
                                 (input_sign & op_mod_q & ~rounded_int_res_zero);

  // All integer special cases are invalid
  assign int_special_status = '{NV: 1'b1, default: 1'b0};

  // Assemble result according to destination format
  assign int_special_result = ifmt_special_result[is_int64]; // destination format

  // -----------------
  // Result selection
  // -----------------
  fpnew_pkg::status_t int_regular_status, fp_regular_status;

  logic [WIDTH-1:0]   fp_result, int_result;
  fpnew_pkg::status_t fp_status, int_status;

  assign fp_regular_status.NV = src_is_int & (of_before_round_cast | of_after_round_cast); // overflow is invalid for I2F casts
  assign fp_regular_status.DZ = 1'b0; // no divisions
  assign fp_regular_status.OF = ~src_is_int & (~info_cast[src_fmt_q].is_inf & (of_before_round_cast | of_after_round_cast)); // inf casts no OF
  assign fp_regular_status.UF = uf_after_round_cast & fp_regular_status.NX;
  assign fp_regular_status.NX = src_is_int ? (| fp_round_sticky_bits_cast) // overflow is invalid in i2f
            : (| fp_round_sticky_bits_cast) | (~info_cast[src_fmt_q].is_inf & (of_before_round_cast | of_after_round_cast));
  assign int_regular_status = '{NX: (| int_round_sticky_bits_cast), default: 1'b0};

  assign fp_result  = fp_result_is_special  ? fp_special_result  : fmt_result[dst_fmt_q];
  assign fp_status  = fp_result_is_special  ? fp_special_status  : fp_regular_status;
  assign int_result = int_result_is_special ? int_special_result : rounded_int_res;
  assign int_status = int_result_is_special ? int_special_status : int_regular_status;

  // registers <-- partial results
  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      exp_prod_msbs_q            <= '0;
      exp_diff_msbs_q            <= '0;
      addend_after_shift_q       <= '0;
      add_count_q                <= '0;
      carry_add_q                <= '0;
      msb_add_q                  <= '0;
      shift_count_q              <= '0;
      mul_count_q                <= '0;
      mantissa_b_msb_q           <= '0;
      normalized_exponent_msbs_q <= '0;
      tmp_q                      <= '0;
      tmp_shift_q                <= '0;
      mant_is_zero_q             <= '0;
      current_state              <= FSM_IDLE;
    end else if (enable_reg) begin
      exp_prod_msbs_q            <= exp_prod_msbs_d;
      exp_diff_msbs_q            <= exp_diff_msbs_d;
      addend_after_shift_q       <= addend_after_shift_d;
      add_count_q                <= add_count_d;
      carry_add_q                <= carry_add_d;
      msb_add_q                  <= msb_add_d;
      shift_count_q              <= shift_count_d;
      mul_count_q                <= mul_count_d;
      mantissa_b_msb_q           <= mantissa_b_msb_d;
      normalized_exponent_msbs_q <= normalized_exponent_msbs_d;
      tmp_q                      <= tmp_d;
      tmp_shift_q                <= tmp_shift_d;
      mant_is_zero_q             <= mant_is_zero_d;
      current_state              <= next_state;
    end
  end

  always_comb
  begin : main_fsm_comb
    // real finite-state machine
    next_state                 = current_state;

    // signals
    exponent_product_new       = exponent_product_old;
    exponent_difference_new    = exponent_difference_old;

    addend_after_shift_d       = addend_after_shift_q;

    norm_shamt                 = addend_shamt;
    normalized_exponent_new    = normalized_exponent_old;
    sum_shifted                = '0;
    sum_shifted_tmp            = shift_out;

    {final_mantissa, sum_sticky_bits} = '0;
    final_exponent             = normalized_exponent_old;
    product_new                = product_old;
    partial_product            = '0;
    mantissa_b_new             = mantissa_b_old;

    addend_a                   = '0;
    addend_b                   = '0;
    carry_in                   = 1'b0;

    exp_a                      = '0;
    exp_b                      = '0;
    exp_carry_in               = '0;
    exponent_product_tmp       = '0;

    shift_in                   = sum[(3*PRECISION_BITS+4)/3-1:0];
    shift_amount               = addend_shamt;
    shift_out_tmp              = shift_out;
    for (int i=0; i<PRECISION_BITS; i++) begin
      mantissa_c_tmp [PRECISION_BITS-1-i] = mantissa_c[i];
    end
    for (int i=0; i<4*PRECISION_BITS+4; i++) begin
      reversed_shift_out_tmp[4*PRECISION_BITS+4-1-i] = shift_out_tmp[i];
    end

    for (int i=0; i<2*INT_MAN_WIDTH+1; i++) begin
      preshift_mant_tmp [2*INT_MAN_WIDTH-i] = preshift_mant[i];
    end
    destination_mant_tmp       = '0;

    addend_shifted             = '0;

    tmp_d                      = tmp_q;

    factor_a                   = mantissa_a;
    factor_b                   = mantissa_b_old[1:0];;

    rounded_abs                = '0;
    result_zero                = 1'b0;

    rounded_sign               = '0;

    carry_add_d                = carry_add_q;
    msb_add_d                  = msb_add_q;

    sticky_before_add          = '0;
    product_shifted            = '0;
    inject_carry_in            = '0;
    round_up                   = '0;
    tmp_shift_d                = tmp_shift_q;

    out_valid_o                = 1'b0;
    in_ready_o                 = 1'b1;
    busy_o                     = 1'b0;

    operands_d                 = operands_q;
    exp_prod_msbs_d            = exp_prod_msbs_q;
    exp_diff_msbs_d            = exp_diff_msbs_q;
    normalized_exponent_msbs_d = normalized_exponent_msbs_q;
    mantissa_b_msb_d           = mantissa_b_msb_q;
    addend_shifted_new         = addend_shifted_old;
    mul_count_d                = mul_count_q;
    shift_count_d              = shift_count_q;
    add_count_d                = add_count_q;

    //non-comp
    operands_equal             = '0;
    operand_a_smaller          = '0;
    sign_a                     = '0;
    sign_b                     = '0;
    minmax_result              = '0;
    minmax_status              = '0;
    cmp_result                 = '0; // false
    cmp_status                 = '0; // no flags
    sgnj_result                = '0;

    are_equal                  = '0;

    src_fmt_d                  = src_fmt_q;
    dst_fmt_d                  = dst_fmt_q;
    int_fmt_d                  = int_fmt_q;

    rnd_mode_d                 = rnd_mode_q;
    op_d                       = op_q;
    op_mod_d                   = op_mod_q;
    tag_d                      = tag_q;
    aux_d                      = aux_q;
    flush_d                    = flush_q;

    rounded_abs_cast           = operands_q[2];
    rounded_int_res            = rounded_abs_cast;
    result_true_zero           = '0;

    rounded_sign_cast = (result_true_zero && 1'b0)
                ? (rnd_mode_q == fpnew_pkg::RDN)
                : input_sign;

    info_a_is_normal_d         = info_a_is_normal_q;
    info_b_is_normal_d         = info_b_is_normal_q;
    info_c_is_normal_d         = info_c_is_normal_q;

    mant_is_zero_d             = mant_is_zero_q;
    is_boxed_d                 = is_boxed_q;

    enable_reg                 = 1'b1;

    case(current_state)
      FSM_IDLE   : begin
        operands_d                   = operands_i;
        is_boxed_d                   = is_boxed_i;
        src_fmt_d                    = src_fmt_i;
        dst_fmt_d                    = dst_fmt_i;
        int_fmt_d                    = int_fmt_i;
        rnd_mode_d                   = rnd_mode_i;
        op_d                         = op_i;
        op_mod_d                     = op_mod_i;
        tag_d                        = tag_i;
        aux_d                        = aux_i;
        flush_d                      = flush_i;
        enable_reg                   = 1'b0;

        if ((in_valid_i && in_ready_o)) begin
          enable_reg                 = 1'b1;
          next_state                 = FSM_EXP_ADD;
         end else begin
          next_state                 = FSM_IDLE;
        end
      end

      FSM_EXP_ADD: begin
        addend_after_shift_d         = '0;
        in_ready_o                   = 1'b0;
        info_a_is_normal_d           = info_d[0].is_normal;
        info_b_is_normal_d           = info_d[1].is_normal;
        info_c_is_normal_d           = info_d[2].is_normal;
        if ((op_q == fpnew_pkg::F2F) || (op_q == fpnew_pkg::F2I)
                     || (op_q == fpnew_pkg::I2F) || (op_q == fpnew_pkg::CPKAB) || (op_q == fpnew_pkg::CPKCD)) begin
          if (int_sign) begin
            //int_mantissa = unsigned'(-int_value);
            addend_a            = ~int_value[(3*PRECISION_BITS+4)/3:0];
            addend_b            = '0;
            carry_in            = 1'b1;
            operands_d[2][(3*PRECISION_BITS+4)/3+1:0]                   = adder_result;
            exp_a               = ~int_value[INT_MAN_WIDTH-1:(3*PRECISION_BITS+4)/3+1];
            exp_b               = '0;
            exp_carry_in        = operands_d[2][(3*PRECISION_BITS+4)/3+1];
            operands_d[2][INT_MAN_WIDTH-1:(3*PRECISION_BITS+4)/3+1] = exp_adder_result;
          end
          else begin
            operands_d[2] = int_value; // get magnitude of negative
          end

          next_state      = FSM_CAST_SHIFT;
          busy_o          = 1'b1;

        end else if ((op_q == fpnew_pkg::SGNJ) || (op_q == fpnew_pkg::MINMAX)
                     || (op_q == fpnew_pkg::CMP) || (op_q == fpnew_pkg::CLASSIFY)) begin
          next_state      = FSM_IDLE;
          out_valid_o     = 1'b1;
          busy_o          = 1'b1;

          addend_a                      = operands_q[1];
          addend_b                      = ~operands_q[0];
          carry_in                      = 1'b1;
          are_equal                     = adder_result;
          exp_a                         = operands_q[1][WIDTH-1:(3*PRECISION_BITS+4)/3+1];
          exp_b                         = ~operands_q[0][WIDTH-1:(3*PRECISION_BITS+4)/3+1];
          exp_carry_in                  = are_equal[(3*PRECISION_BITS+4)/3+1];
          are_equal[WIDTH-1:(3*PRECISION_BITS+4)/3+1] = exp_adder_result;

          operands_equal    = (!are_equal) || (info_a.is_zero && info_b.is_zero);
          // Invert result if non-zero signs involved (unsigned comparison)
          if (operand_a.sign && ~operand_b.sign)
            operand_a_smaller = 1'b1;
          else if (~operand_a.sign && operand_b.sign)
            operand_a_smaller = 1'b0;
          else
            operand_a_smaller = (~are_equal[WIDTH-1]) ^ (operand_a.sign || operand_b.sign);

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
            default: sgnj_result = '{default: fpnew_pkg::DONT_CARE}; // don't care
          endcase

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
              default: minmax_result = '{default: fpnew_pkg::DONT_CARE}; // don't care
            endcase
          end

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
                else cmp_result = (operand_a_smaller & ~operands_equal) ^ op_mod_q;
              end
              fpnew_pkg::RDN: begin // Equal
                if (any_operand_nan) cmp_result = op_mod_q; // NaN always not equal
                else cmp_result = operands_equal ^ op_mod_q;
              end
              default: cmp_result = '{default: fpnew_pkg::DONT_CARE}; // don't care
            endcase
          end
        end
        else begin
          if (~(result_is_special)) begin
            exp_a                      = exponent_a;
            exp_b                      = exponent_b;
            exp_carry_in               = info_a.is_subnormal;
            exponent_product_tmp       = exp_adder_result;

            exponent_product_new       = (info_a.is_zero || info_b.is_zero)
                                            ? 2 - signed'(BIAS) // in case the product is zero, set minimum exp.
                                            : signed'(exponent_product_tmp + info_b.is_subnormal
                                                - signed'(BIAS));
            {exp_prod_msbs_d ,operands_d[0].exponent} = exponent_product_new;

            busy_o                     = 1'b1;
            next_state = FSM_EXP_DIFF;
          end
          else begin
            out_valid_o = 1'b1;
            busy_o      = 1'b1;
            if (out_ready_i && out_valid_o) begin
              next_state  = FSM_IDLE;
            end
            else begin
              next_state = FSM_EXP_ADD;
              // next_state  = FSM_WAIT;
            end
          end
        end
      end

      FSM_CAST_SHIFT: begin
        //  assign input_mant = encoded_mant << renorm_shamt;
        busy_o                  = 1'b1;
        in_ready_o              = 1'b0;
        mant_is_zero_d          = mant_is_zero;
        shift_count_d           = shift_count_q + 1;
        if (~shift_count_q) begin
          shift_in              = encoded_mant[(3*PRECISION_BITS+4)/3-1:0];
          shift_amount          = renorm_shamt;
          operands_d[1][INT_MAN_WIDTH-1:0]  = shift_out[INT_MAN_WIDTH-1:0];
          next_state            = FSM_CAST_SHIFT;
        end else begin
          shift_in              = encoded_mant[INT_MAN_WIDTH-1:(3*PRECISION_BITS+4)/3];
          shift_amount          = renorm_shamt;
          operands_d[1][INT_MAN_WIDTH-1:0] = operands_q[1] | (shift_out[INT_MAN_WIDTH-1:0] << ((3*PRECISION_BITS+4)/3));
          next_state            = FSM_CAST_DEST_SHIFT;
        end
      end

      FSM_CAST_DEST_SHIFT: begin
//      destination_mant = preshift_mant >> denorm_shamt;
        busy_o                  = 1'b1;
        in_ready_o                   = 1'b0;
        if (add_count_q == 0) begin
          shift_in              = preshift_mant_tmp[2*INT_MAN_WIDTH:(3*PRECISION_BITS+4)*2/3];
          shift_amount          = denorm_shamt;
          addend_after_shift_d[2*INT_MAN_WIDTH-(3*PRECISION_BITS+4)*2/3:0]  = shift_out[2*INT_MAN_WIDTH-(3*PRECISION_BITS+4)*2/3:0];
          add_count_d           = add_count_q + 1;
          next_state            = FSM_CAST_DEST_SHIFT;
        end else if (add_count_q == 1) begin
          shift_in              = preshift_mant_tmp[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3];
          shift_amount          = denorm_shamt;
          addend_after_shift_d[2*INT_MAN_WIDTH:2*INT_MAN_WIDTH-(3*PRECISION_BITS+4)*2/3]  = shift_out[(3*PRECISION_BITS+4)*2/3:0];
          add_count_d           = add_count_q + 1;
          next_state            = FSM_CAST_DEST_SHIFT;
        end else begin
          shift_in              = preshift_mant_tmp[(3*PRECISION_BITS+4)/3-1:0];
          shift_amount          = denorm_shamt;
          destination_mant_tmp  = (addend_after_shift_q[2*INT_MAN_WIDTH-(3*PRECISION_BITS+4)*2/3:0] << (3*PRECISION_BITS+4)*2/3)
              | (addend_after_shift_q[2*INT_MAN_WIDTH:2*INT_MAN_WIDTH-(3*PRECISION_BITS+4)*2/3] << (3*PRECISION_BITS+4)/3)
              | (shift_out[2*INT_MAN_WIDTH-1:0]);
          add_count_d           = '0;
          for (int i=0; i<2*INT_MAN_WIDTH+1; i++) begin
            addend_after_shift_d[2*INT_MAN_WIDTH-i] = destination_mant_tmp[i];
          end
          next_state            = FSM_CAST_ROUNDING;
        end
      end

      FSM_CAST_DEST_INV: begin
    //  rounded_int_res      = rounded_sign_cast ? unsigned'(-rounded_abs_cast) : rounded_abs_cast;
        if (rounded_sign_cast) begin
          addend_a            = ~rounded_abs_cast[(3*PRECISION_BITS+4)/3:0];
          addend_b            = '0;
          carry_in            = 1'b1;
          rounded_int_res[(3*PRECISION_BITS+4)/3+1:0]                   = adder_result;
          exp_a               = ~rounded_abs_cast[INT_MAN_WIDTH-1:(3*PRECISION_BITS+4)/3+1];
          exp_b               = '0;
          exp_carry_in        = rounded_int_res[(3*PRECISION_BITS+4)/3+1];
          rounded_int_res[INT_MAN_WIDTH-1:(3*PRECISION_BITS+4)/3+1]     = exp_adder_result;
        end
        else begin
          rounded_int_res = rounded_abs_cast; // get magnitude of negative
        end
        busy_o                  = 1'b1;
        next_state              = FSM_IDLE;
        out_valid_o             = 1'b1;
        in_ready_o              = 1'b0;
      end

      FSM_CAST_ROUNDING: begin
        // Take the rounding decision according to RISC-V spec
        // RoundMode | Mnemonic | Meaning
        // :--------:|:--------:|:-------
        //    000    |   RNE    | Round to Nearest, ties to Even
        //    001    |   RTZ    | Round towards Zero
        //    010    |   RDN    | Round Down (towards -\infty)
        //    011    |   RUP    | Round Up (towards \infty)
        //    100    |   RMM    | Round to Nearest, ties to Max Magnitude
        //  others   |          | *invalid*
        unique case (rnd_mode_q)
          fpnew_pkg::RNE: // Decide accoring to round/sticky bits
            unique case (round_sticky_bits_cast)
              2'b00,
              2'b01: round_up = 1'b0;           // < ulp/2 away, round down
              2'b10: round_up = pre_round_abs_cast[0]; // = ulp/2 away, round towards even result
              2'b11: round_up = 1'b1;           // > ulp/2 away, round up
              default: round_up = fpnew_pkg::DONT_CARE;
            endcase
          fpnew_pkg::RTZ: round_up = 1'b0; // always round down
          fpnew_pkg::RDN: round_up = (| round_sticky_bits_cast) ? input_sign  : 1'b0; // to 0 if +, away if -
          fpnew_pkg::RUP: round_up = (| round_sticky_bits_cast) ? ~input_sign : 1'b0; // to 0 if -, away if +
          fpnew_pkg::RMM: round_up = round_sticky_bits_cast[1]; // round down if < ulp/2 away, else up
          default: round_up = fpnew_pkg::DONT_CARE; // propagate x
        endcase

        // Perform the rounding, exponent change and overflow to inf happens automagically
        // rounded_abs = pre_round_abs + round_up;
        addend_a       = pre_round_abs_cast[(3*PRECISION_BITS+4)/3-1:0];
        addend_b       = '0;
        carry_in       = round_up;
        operands_d[2][(3*PRECISION_BITS+4)/3:0]    = adder_result;
        exp_a               = pre_round_abs_cast[INT_MAN_WIDTH-1:(3*PRECISION_BITS+4)/3];
        exp_b               = '0;
        exp_carry_in        = operands_d[2][(3*PRECISION_BITS+4)/3];
        operands_d[2][INT_MAN_WIDTH-1:(3*PRECISION_BITS+4)/3] = exp_adder_result;

        // True zero result is a zero result without dirty round/sticky bits
        result_true_zero = (pre_round_abs_cast == '0) && (round_sticky_bits_cast == '0);

        busy_o                  = 1'b1;
        next_state              = FSM_CAST_DEST_INV;
        in_ready_o              = 1'b0;
      end

      FSM_EXP_DIFF: begin
        busy_o                = 1'b1;
        in_ready_o            = 1'b0;

        // exponent_difference        = exponent_addend - exponent_product;
        exp_a                 = exponent_addend;
        exp_b                 = ~{exp_prod_msbs_q, operands_q[0].exponent};
        exp_carry_in          = 1'b1;
        exponent_difference_new = exp_adder_result;
        {exp_diff_msbs_d, operands_d[1].exponent} = exponent_difference_new;

        next_state = FSM_MANTISSA_PROD_ADDEND_SHIFT;
        if (mul_count_q == 0) begin
          mantissa_b_new = mantissa_b;
          {mantissa_b_msb_d, operands_d[1].mantissa} = mantissa_b_new;
        end
      end
      FSM_MANTISSA_PROD_ADDEND_SHIFT: begin
        busy_o              = 1'b1;
        in_ready_o          = 1'b0;
        mul_count_d         = mul_count_q + 1;

        // The right shift on mantissa_c is realized with a left shifter
        // The input and the output of the left shifter have to be bit-reversed to obtaing the right
        // shift

        // Left shift
        shift_in     = mantissa_c_tmp;
        shift_amount = addend_shamt;
        shift_out_tmp = shift_out;

        // when computing an addition (A = 1) the multiplication is realized shifting mantissa_b
        if (op_q == fpnew_pkg::ADD) begin
          addend_after_shift_d[2*PRECISION_BITS-1:0]  = mantissa_b << (PRECISION_BITS-1);
          next_state = FSM_SUM;
          {tmp_shift_d, addend_after_shift_d[3*PRECISION_BITS+3:2*PRECISION_BITS],
              operands_d[2].mantissa, mantissa_b_new, operands_d[0].mantissa} = reversed_shift_out_tmp;
        end
        else begin
          // Product is placed into a 3p+4 bit wide vector, padded with 2 bits for round and sticky:
          // | 000...000 | product | RS |
          //  <-  p+2  -> <-  2p -> < 2>
          factor_a            = mantissa_a;
          factor_b            = mantissa_b_old[1:0];;
          partial_product     = prod;

          if ((mul_count_q == PRECISION_BITS/2) && (FpFormat == fpnew_pkg::fp_format_e'(1)))
            addend_a          = addend_after_shift_q[(mul_count_q<<1)+:PRECISION_BITS+1];
          else
            addend_a          = addend_after_shift_q[(mul_count_q<<1)+:PRECISION_BITS+2];

          addend_b            = partial_product;
          carry_in            = '0;
          addend_after_shift_d[(mul_count_q<<1)+:PRECISION_BITS+2]    = adder_result[PRECISION_BITS+2:0];

          if (((mul_count_q == PRECISION_BITS/2) && (FpFormat == fpnew_pkg::fp_format_e'(1)))
              || (((mul_count_q == PRECISION_BITS/2-1) && (FpFormat == fpnew_pkg::fp_format_e'(0))))) begin

            next_state = FSM_SUM;
            {tmp_shift_d ,addend_after_shift_d[3*PRECISION_BITS+3:2*PRECISION_BITS],
              operands_d[2].mantissa, mantissa_b_new, operands_d[0].mantissa} = reversed_shift_out_tmp;
              if (op_q == fpnew_pkg::MUL) begin
                {tmp_shift_d ,addend_after_shift_d[3*PRECISION_BITS+3:2*PRECISION_BITS],
                  operands_d[2].mantissa, mantissa_b_new, operands_d[0].mantissa} = '0;
                product_shifted    = addend_after_shift_d[2*PRECISION_BITS-1:0] << 2;
                {msb_add_d, addend_after_shift_d} = {1'b0, product_shifted};
                next_state = FSM_NORMALIZATION;
              end
          end
          else begin
            next_state = FSM_MANTISSA_PROD_ADDEND_SHIFT;
            mantissa_b_new = mantissa_b_old >> 2;
          end
        end
        {mantissa_b_msb_d, operands_d[1].mantissa} = mantissa_b_new;
      end
      FSM_SUM: begin
        busy_o             = 1'b1;
        in_ready_o         = 1'b0;
        add_count_d        = add_count_q + 1;
        product_shifted    = addend_after_shift_q[2*PRECISION_BITS-1:0] << 2;

        // In case of a subtraction, the addend is inverted
        addend_shifted       = (effective_subtraction) ? ~addend_shifted_old : addend_shifted_old;
        addend_shifted_new   = addend_shifted_old;

        sticky_before_add    = (| {operands_q[1].mantissa[0], operands_q[0].mantissa});
        inject_carry_in      = effective_subtraction & ~sticky_before_add;

        if (add_count_q == 0) begin
          addend_shifted_new[(3*PRECISION_BITS+4)/3-1:0] = addend_shifted[(3*PRECISION_BITS+4)/3-1:0];

          addend_a       = product_shifted[(3*PRECISION_BITS+4)/3-1:0];
          addend_b       = addend_shifted_new[(3*PRECISION_BITS+4)/3-1:0];
          carry_in       = inject_carry_in;
          {carry_add_d, addend_shifted_new[(3*PRECISION_BITS+4)/3-1:0]}
              = adder_result[(3*PRECISION_BITS+4)/3:0];
          {tmp_shift_d ,addend_after_shift_d[3*PRECISION_BITS+3:2*PRECISION_BITS],
              operands_d[2].mantissa, mantissa_b_new, operands_d[0].mantissa} = {addend_shifted_new, addend_sticky_bits};
          {mantissa_b_msb_d, operands_d[1].mantissa} = mantissa_b_new;

          next_state     = FSM_SUM;
        end
        else if (add_count_q == 1) begin
          addend_shifted_new[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3]
                               = addend_shifted[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3];

          addend_a       = product_shifted[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3];
          addend_b       = addend_shifted_new[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3];
          carry_in       = carry_add_q;
          {carry_add_d, addend_shifted_new[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3]}
              = adder_result[(3*PRECISION_BITS+4)/3:0];
          {tmp_shift_d ,addend_after_shift_d[3*PRECISION_BITS+3:2*PRECISION_BITS],
              operands_d[2].mantissa, mantissa_b_new, operands_d[0].mantissa} = {addend_shifted_new, addend_sticky_bits};
          {mantissa_b_msb_d, operands_d[1].mantissa} = mantissa_b_new;
          next_state     = FSM_SUM;
        end
        else begin
          add_count_d     = '0;
          addend_shifted_new[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3]
              = addend_shifted[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3];

          addend_a       = product_shifted[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3];
          addend_b       = addend_shifted_new[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3];
          carry_in       = carry_add_q;
          {msb_add_d, addend_shifted_new[(3*PRECISION_BITS+4)-1:(3*PRECISION_BITS+4)*2/3]}
              = adder_result[(3*PRECISION_BITS+4)/3+1:0];
          addend_after_shift_d = addend_shifted_new;
          if (effective_subtraction && ~msb_add_d) begin
            next_state     = FSM_COMPLEMENT_SUM;
          end
          else begin
            next_state     = FSM_NORMALIZATION;
          end
        end
      end
      // Complement negative sum (can only happen in subtraction -> overflows for positive results)
      //  assign sum        = (effective_subtraction && ~sum_carry) ? -sum_raw : sum_raw;
      FSM_COMPLEMENT_SUM: begin
        busy_o                = 1'b1;
        in_ready_o            = 1'b0;
        add_count_d           = add_count_q + 1;

        if (add_count_q == 0) begin
          addend_a            = {~addend_after_shift_q[(3*PRECISION_BITS+4)/3-1:0]};
          addend_b            = '0;
          carry_in            = 1'b1;
          {carry_add_d, addend_after_shift_d[(3*PRECISION_BITS+4)/3-1:0]}
              = adder_result[(3*PRECISION_BITS+4)/3:0];

          next_state          = FSM_COMPLEMENT_SUM;
        end
        else if (add_count_q == 1) begin
          addend_a            = {~addend_after_shift_q[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3]};
          addend_b            = 1'b0;
          carry_in            = carry_add_q;
          {carry_add_d, addend_after_shift_d[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3]}
                              = adder_result[(3*PRECISION_BITS+4)/3:0];

          next_state          = FSM_COMPLEMENT_SUM;
        end
        else begin
          addend_a            = {~msb_add_q,
                              ~addend_after_shift_q[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3]};
          addend_b            = 1'b0;
          carry_in            = carry_add_q;
          {msb_add_d, addend_after_shift_d[(3*PRECISION_BITS+4)-1:(3*PRECISION_BITS+4)*2/3]}
                              = adder_result[(3*PRECISION_BITS+4)/3+1:0];

          next_state          = FSM_NORMALIZATION;
        end
      end

      FSM_NORMALIZATION: begin
        busy_o               = 1'b1;
        in_ready_o           = 1'b0;
        shift_count_d        = shift_count_q + 1;
        // Normalization shift amount based on exponents and LZC (unsigned as only left shifts)

        if ((exponent_difference_old <= 0) || (effective_subtraction
              && (exponent_difference_old <= 2))) begin
          // Normal result (biased exponent > 0 and not a zero)
          if ((exponent_product_old - leading_zero_count_sgn + 1 >= 0) && !lzc_zeroes) begin
            // Undo initial product shift, remove the counted zeroes
            norm_shamt              = PRECISION_BITS + 2 + leading_zero_count;

            //   normalized_exponent = {exp_prod_msbs_q ,operands_q[0].exponent} - leading_zero_count_sgn + 1;
            // account for shift
            exp_a                   = exponent_product_old;
            exp_b                   = -leading_zero_count_sgn;
            exp_carry_in            = 1'b1;
            normalized_exponent_new = exp_adder_result;
          // Subnormal result
          end else begin
            // Cap the shift distance to align mantissa with minimum exponent
            norm_shamt              = unsigned'(signed'(PRECISION_BITS) + 2 + exponent_product_old);
            normalized_exponent_new = 0; // subnormals encoded as 0
          end
        // Addend-anchored case
        end else begin
          norm_shamt              = addend_shamt; // Undo the initial shift
          normalized_exponent_new = tentative_exponent;
        end

        if (shift_count_q == 0) begin
          shift_in                         = sum[(3*PRECISION_BITS+4)/3-1:0];
          shift_amount                     = norm_shamt;
          sum_shifted                      = shift_out[(3*PRECISION_BITS+4):0];
          {carry_add_d, operands_d[2].mantissa, operands_d[0].mantissa, mantissa_b_new, tmp_d} = shift_out[(3*PRECISION_BITS+4):0];
          {mantissa_b_msb_d, operands_d[1].mantissa} = mantissa_b_new;
          next_state                       = FSM_NORMALIZATION;
        end
        else begin
          shift_in                         = sum[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3];
          shift_amount                     = norm_shamt;
//          {tmp_shift_d, addend_after_shift_d[(3*PRECISION_BITS+4)*2/3-1:0]} = shift_out;
          next_state                       = FSM_ROUNDING;
          addend_after_shift_d[EXP_WIDTH-1:0] = normalized_exponent_new;
          {carry_add_d, operands_d[2].mantissa, operands_d[0].mantissa, mantissa_b_new, tmp_d} =
              (shift_out << (3*PRECISION_BITS+4)/3)
              | {carry_add_q, operands_q[2].mantissa, operands_q[0].mantissa, mantissa_b_old, tmp_q};
        end
      end

      FSM_ROUNDING: begin
        in_ready_o      = 1'b0;
        busy_o          = 1'b1;
        out_valid_o     = 1'b1;

        // Take the rounding decision according to RISC-V spec
        // RoundMode | Mnemonic | Meaning
        // :--------:|:--------:|:-------
        //    000    |   RNE    | Round to Nearest, ties to Even
        //    001    |   RTZ    | Round towards Zero
        //    010    |   RDN    | Round Down (towards -\infty)
        //    011    |   RUP    | Round Up (towards \infty)
        //    100    |   RMM    | Round to Nearest, ties to Max Magnitude
        //  others   |          | *invalid*
        unique case (rnd_mode_q)
          fpnew_pkg::RNE: // Decide accoring to round/sticky bits
            unique case (round_sticky_bits)
              2'b00,
              2'b01: round_up = 1'b0;           // < ulp/2 away, round down
              2'b10: round_up = pre_round_abs[0]; // = ulp/2 away, round towards even result
              2'b11: round_up = 1'b1;           // > ulp/2 away, round up
              default: round_up = fpnew_pkg::DONT_CARE;
            endcase
          fpnew_pkg::RTZ: round_up = 1'b0; // always round down
          fpnew_pkg::RDN: round_up = (| round_sticky_bits) ? pre_round_sign  : 1'b0; // to 0 if +, away if -
          fpnew_pkg::RUP: round_up = (| round_sticky_bits) ? ~pre_round_sign : 1'b0; // to 0 if -, away if +
          fpnew_pkg::RMM: round_up = round_sticky_bits[1]; // round down if < ulp/2 away, else up
          default: round_up = fpnew_pkg::DONT_CARE; // propagate x
        endcase

        // Perform the rounding, exponent change and overflow to inf happens automagically
        // rounded_abs = pre_round_abs + round_up;
        addend_a       = pre_round_abs[(3*PRECISION_BITS+4)/3-1:0];
        addend_b       = '0;
        carry_in       = round_up;
        rounded_abs[(3*PRECISION_BITS+4)/3:0]    = adder_result;
        exp_a               = pre_round_abs[EXP_BITS+MAN_BITS-1:(3*PRECISION_BITS+4)/3];
        exp_b               = '0;
        exp_carry_in        = rounded_abs[(3*PRECISION_BITS+4)/3];
        rounded_abs[EXP_BITS+MAN_BITS-1:(3*PRECISION_BITS+4)/3] = exp_adder_result;

        // True zero result is a zero result without dirty round/sticky bits
        result_zero = (pre_round_abs == '0) && (round_sticky_bits == '0);

        // In case of effective subtraction (thus signs of addition operands must have differed) and a
        // true zero result, the result sign is '-' in case of RDN and '+' for other modes.
        rounded_sign = (result_zero && effective_subtraction)
                        ? (rnd_mode_q == fpnew_pkg::RDN)
                        : pre_round_sign;


        if ((exponent_difference_old <= 0) || (effective_subtraction && (exponent_difference_old <= 2))) begin
          // Normal result (biased exponent > 0 and not a zero)
          if ((exponent_product_old - leading_zero_count_sgn + 1 >= 0) && !lzc_zeroes) begin
            // Undo initial product shift, remove the counted zeroes
            norm_shamt          = PRECISION_BITS + 2 + leading_zero_count;
          // Subnormal result
          end else begin
            // Cap the shift distance to align mantissa with minimum exponent
            norm_shamt          = unsigned'(signed'(PRECISION_BITS) + 2 + exponent_product_old);
          end
        // Addend-anchored case
        end else begin
          norm_shamt          = addend_shamt; // Undo the initial shift
        end
        shift_in              = sum[3*PRECISION_BITS+3:(3*PRECISION_BITS+4)*2/3];
        shift_amount          = norm_shamt;
        sum_shifted_tmp       = shift_out;
        sum_shifted = (sum_shifted_tmp << (3*PRECISION_BITS+4)*2/3) |
//            ({tmp_shift_q, addend_after_shift_q[(3*PRECISION_BITS+4)*2/3-1:0]} << (3*PRECISION_BITS+4)/3)
             {carry_add_q, operands_q[2].mantissa, operands_q[0].mantissa, mantissa_b_old, tmp_q};

        {final_mantissa, sum_sticky_bits} = sum_shifted;
        final_exponent                    = normalized_exponent_old;

        // The normalized sum has overflown, align right and fix exponent
        if (sum_shifted[3*PRECISION_BITS+4]) begin // check the carry bit
          {final_mantissa, sum_sticky_bits} = sum_shifted >> 1;
          final_exponent                    = normalized_exponent_old + 1;
        // The normalized sum is normal, nothing to do
        end else if (sum_shifted[3*PRECISION_BITS+3]) begin // check the sum MSB
          // do nothing
        // The normalized sum is still denormal, align left - unless the result is not already
        // subnormal
        end else if (normalized_exponent_old > 1) begin
          {final_mantissa, sum_sticky_bits} = sum_shifted << 1;
          final_exponent                    = normalized_exponent_old - 1;
        // Otherwise we're denormal
        end else begin
          final_exponent = '0;
        end

        if (out_ready_i && out_valid_o) begin
          next_state    = FSM_IDLE;
          add_count_d   = '0;
          mul_count_d   = '0;
          shift_count_d = '0;
        end
        else begin
          next_state    = FSM_ROUNDING;
        end
      end

    endcase // current_state
  end

  assign sum_raw = {msb_add_q, addend_after_shift_q};

  // floatli adder
  assign adder_result = addend_a + addend_b + carry_in;

  // floatli_exp_adder
  assign exp_adder_result = signed'(exp_a + exp_b + exp_carry_in);

  // floatli_shifter
  assign shift_out       = shift_in << shift_amount;

  // floatli mantissa multiplier
  assign prod = factor_a * factor_b;

  // Calculate internal exponents from encoded values. Real exponents are (ex = Ex - bias + 1 - nx)
  // with Ex the encoded exponent and nx the implicit bit. Internal exponents stay biased.
  assign exponent_addend = signed'(exponent_c + $signed({1'b0, ~info_c_is_normal_q})); // 0 as subnorm
  // Biased product exponent is the sum of encoded exponents minus the bias.

  // The tentative exponent will be the larger of the product or addend exponent
  assign tentative_exponent = ({exp_diff_msbs_q ,operands_q[1].exponent} > 0) ? exponent_addend : {exp_prod_msbs_q ,operands_q[0].exponent};

  always_comb begin : addend_shift_amount
    // Product-anchored case, saturated shift (addend is only in the sticky bit)
    if (exponent_difference_old <= signed'(-2 * PRECISION_BITS - 1))
      addend_shamt = 3 * PRECISION_BITS + 4;
    // Addend and product will have mutual bits to add
    else if (exponent_difference_old <= signed'(PRECISION_BITS + 2))
      addend_shamt = unsigned'(signed'(PRECISION_BITS) + 3 - exponent_difference_old);
    // Addend-anchored case, saturated shift (product is only in the sticky bit)
    else
      addend_shamt = 0;
  end

  // Add implicit bits to mantissae
  assign mantissa_a = {info_a_is_normal_q, operand_a.mantissa};
  assign mantissa_b = {info_b_is_normal_q, operand_b.mantissa};
  assign mantissa_c = {info_c_is_normal_q, operand_c.mantissa};

  assign sum_carry = sum_raw[3*PRECISION_BITS+4];
  assign sum = sum_raw;
  // In case of a mispredicted subtraction result, do a sign flip
  assign final_sign = (effective_subtraction && (sum_carry == tentative_sign))
                      ? 1'b1
                      : (effective_subtraction ? 1'b0 : tentative_sign);

  assign sum_lower = sum[LOWER_SUM_WIDTH-1:0];

  // Leading zero counter for cancellations
  lzc #(
    .WIDTH ( LOWER_SUM_WIDTH ),
    .MODE  ( 1               ) // MODE = 1 counts leading zeroes
  ) i_lzc (
    .in_i    ( sum_lower          ),
    .cnt_o   ( leading_zero_count ),
    .empty_o ( lzc_zeroes         )
  );

  assign leading_zero_count_sgn = signed'({1'b0, leading_zero_count});

  // Update the sticky bit with the shifted-out bits
  assign sticky_after_norm = (| {sum_sticky_bits}) | sticky_before_add;


  // Classification before round. RISC-V mandates checking underflow AFTER rounding!
  assign of_before_round = final_exponent >= 2**(EXP_BITS)-1; // infinity exponent is all ones
  assign uf_before_round = final_exponent == 0;               // exponent for subnormals capped to 0

  // Assemble result before rounding. In case of overflow, the largest normal value is set.
  assign pre_round_sign     = final_sign;
  assign pre_round_exponent = (of_before_round) ? 2**EXP_BITS-2 : unsigned'(final_exponent[EXP_BITS-1:0]);
  assign pre_round_mantissa = (of_before_round) ? '1 : final_mantissa[MAN_BITS:1]; // bit 0 is R bit
  assign pre_round_abs      = {pre_round_exponent, pre_round_mantissa};

  //non_comb
  // ---------------
  // Classification
  // ---------------
  fpnew_pkg::status_t    class_status;
  logic                  class_extension_bit;
//  fpnew_pkg::classmask_e class_mask_d; // the result is actually here

  // Classification - always return the classification mask on the dedicated port
  always_comb begin : classify
    if (info_a.is_normal) begin
      class_mask_o = operand_a.sign       ? fpnew_pkg::NEGNORM    : fpnew_pkg::POSNORM;
    end else if (info_a.is_subnormal) begin
      class_mask_o = operand_a.sign       ? fpnew_pkg::NEGSUBNORM : fpnew_pkg::POSSUBNORM;
    end else if (info_a.is_zero) begin
      class_mask_o = operand_a.sign       ? fpnew_pkg::NEGZERO    : fpnew_pkg::POSZERO;
    end else if (info_a.is_inf) begin
      class_mask_o = operand_a.sign       ? fpnew_pkg::NEGINF     : fpnew_pkg::POSINF;
    end else if (info_a.is_nan) begin
      class_mask_o = info_a.is_signalling ? fpnew_pkg::SNAN       : fpnew_pkg::QNAN;
    end else begin
      class_mask_o = fpnew_pkg::QNAN; // default value
    end
  end

  assign class_status        = '0;   // classification does not set flags
  assign class_extension_bit = 1'b0; // classification always produces results in integer registers

  // In case of overflow, the round and sticky bits are set for proper rounding
  assign round_sticky_bits  = (of_before_round) ? 2'b11 : {final_mantissa[0], sticky_after_norm};

  // Classification after rounding
  assign uf_after_round = rounded_abs[EXP_BITS+MAN_BITS-1:MAN_BITS] == '0; // exponent = 0
  assign of_after_round = rounded_abs[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // exponent all ones

  // -----------------
  // Result selection
  // -----------------
  logic [WIDTH-1:0]     regular_result;
  fpnew_pkg::status_t   regular_status;

  // Assemble regular result
  assign regular_result    = {rounded_sign, rounded_abs};
  assign regular_status.NV = 1'b0; // only valid cases are handled in regular path
  assign regular_status.DZ = 1'b0; // no divisions
  assign regular_status.OF = of_before_round | of_after_round;   // rounding can introduce overflow
  assign regular_status.UF = uf_after_round & regular_status.NX; // only inexact results raise UF
  assign regular_status.NX = (| round_sticky_bits) | of_before_round | of_after_round;

  always_comb begin : select_result
    unique case (op_q)
      fpnew_pkg::F2F: begin
        result_o        = dst_is_int ? int_result : fp_result;
        status_o        = dst_is_int ? int_status : fp_status;
        extension_bit_o = dst_is_int ? int_result[WIDTH-1] : 1'b1;
      end
      fpnew_pkg::F2I: begin
        result_o        = dst_is_int ? int_result : fp_result;
        status_o        = dst_is_int ? int_status : fp_status;
        extension_bit_o = dst_is_int ? int_result[WIDTH-1] : 1'b1;
      end
      fpnew_pkg::I2F: begin
        result_o        = dst_is_int ? int_result : fp_result;
        status_o        = dst_is_int ? int_status : fp_status;
        extension_bit_o = dst_is_int ? int_result[WIDTH-1] : 1'b1;
      end
      fpnew_pkg::CPKAB: begin
        result_o        = dst_is_int ? int_result : fp_result;
        status_o        = dst_is_int ? int_status : fp_status;
        extension_bit_o = dst_is_int ? int_result[WIDTH-1] : 1'b1;
      end
      fpnew_pkg::CPKCD: begin
        result_o        = dst_is_int ? int_result : fp_result;
        status_o        = dst_is_int ? int_status : fp_status;
        extension_bit_o = dst_is_int ? int_result[WIDTH-1] : 1'b1;
      end

      fpnew_pkg::SGNJ: begin
        result_o        = sgnj_result;
        status_o        = sgnj_status;
        extension_bit_o = sgnj_extension_bit;
      end
      fpnew_pkg::MINMAX: begin
        result_o        = minmax_result;
        status_o        = minmax_status;
        extension_bit_o = minmax_extension_bit;
      end
      fpnew_pkg::CMP: begin
        result_o        = cmp_result;
        status_o        = cmp_status;
        extension_bit_o = cmp_extension_bit;
      end
      fpnew_pkg::CLASSIFY: begin
        result_o        = '{default: fpnew_pkg::DONT_CARE}; // unused
        status_o        = class_status;
        extension_bit_o = class_extension_bit;
      end
      fpnew_pkg::FMADD: begin
        result_o        = (result_is_special && (current_state == FSM_EXP_ADD)) ? special_result : regular_result;
        status_o        = (result_is_special && (current_state == FSM_EXP_ADD)) ? special_status : regular_status;
        extension_bit_o = 1'b1; // always NaN-Box result
      end
      fpnew_pkg::FNMSUB: begin
        result_o        = (result_is_special && (current_state == FSM_EXP_ADD)) ? special_result : regular_result;
        status_o        = (result_is_special && (current_state == FSM_EXP_ADD)) ? special_status : regular_status;
        extension_bit_o = 1'b1; // always NaN-Box result
      end
      fpnew_pkg::ADD: begin
        result_o        = (result_is_special && (current_state == FSM_EXP_ADD)) ? special_result : regular_result;
        status_o        = (result_is_special && (current_state == FSM_EXP_ADD)) ? special_status : regular_status;
        extension_bit_o = 1'b1; // always NaN-Box result
      end
      fpnew_pkg::MUL: begin
        result_o        = (result_is_special && (current_state == FSM_EXP_ADD)) ? special_result : regular_result;
        status_o        = (result_is_special && (current_state == FSM_EXP_ADD)) ? special_status : regular_status;
        extension_bit_o = 1'b1; // always NaN-Box result
      end
      default: begin
        result_o        = '{default: fpnew_pkg::DONT_CARE}; // dont care
        status_o        = '{default: fpnew_pkg::DONT_CARE}; // dont care
        extension_bit_o = fpnew_pkg::DONT_CARE;             // dont care
      end
    endcase
  end

  //non_comb
  assign is_class_o = (op_q == fpnew_pkg::CLASSIFY);

  assign tag_o           = tag_q;
  assign aux_o           = aux_q;

endmodule
