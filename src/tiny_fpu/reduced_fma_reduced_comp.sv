// Copyright 2019,2020 ETH Zurich and University of Bologna.
//
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

// Authors: Luca Bertaccini <lbertaccini@iis.ee.ethz.ch>, Stefan Mach <smach@iis.ee.ethz.ch>

// Reduced FMA
module fpnew_fma #(
  parameter fpnew_pkg::fp_format_e   FpFormat    = fpnew_pkg::fp_format_e'(0),
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,

  localparam int unsigned WIDTH = fpnew_pkg::fp_width(FpFormat) // do not change
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
  localparam int unsigned  EXP_WIDTH = unsigned'(fpnew_pkg::maximum(EXP_BITS + 2, LZC_RESULT_WIDTH));
  // Shift amount width: maximum internal mantissa size is 3p+3 bits
  localparam int unsigned SHIFT_AMOUNT_WIDTH = $clog2(3 * PRECISION_BITS + 3);

  // ----------------
  // Type definition
  // ----------------
  typedef struct packed {
    logic                sign;
    logic [EXP_BITS-1:0] exponent;
    logic [MAN_BITS-1:0] mantissa;
  } fp_t;

  // FSM states
  enum logic [2:0] {FSM_EXP_ADD, FSM_EXP_DIFF, FSM_MANTISSA_PROD_ADDEND_SHIFT, FSM_SUM,
                    FSM_COMPLEMENT_SUM, FSM_NORMALIZATION, FSM_ROUNDING} next_state, current_state;

//  logic [2:0]            is_boxed_q; // 3 operands
  fpnew_pkg::roundmode_e rnd_mode_q;
  fpnew_pkg::operation_e op_q, op;
  logic [5:0]            tmp_d, tmp_q;

  logic                  op_mod_q, op_mod;
  TagType                tag_q;
  AuxType                aux_q;
  logic                  flush_q;

  fp_t [2:0]             operands_d;
  fp_t [2:0]             operands_q;

  fpnew_pkg::fp_info_t [2:0] info_d, info_q;

  // registers <-- inputs
  always_ff @(posedge clk_i or negedge rst_ni) begin : input_regs
    if(~rst_ni) begin
//      is_boxed_q      <= '0;
      rnd_mode_q          <= fpnew_pkg::RNE;
      op_q                <= fpnew_pkg::FMADD;
      op_mod_q            <= '0;
      tag_q               <= '0;
      aux_q               <= '0;
      flush_q             <= '0;
      operands_q          <= '0;
      info_q[0].is_normal <= '0;
      info_q[1].is_normal <= '0;
      info_q[2].is_normal <= '0;
      info_q[2].is_zero   <= '0;
    end else begin
//      is_boxed_q      <= is_boxed_i;
      rnd_mode_q          <= rnd_mode_i;
      op_q                <= op_i;
      op_mod_q            <= op_mod_i;
      tag_q               <= tag_i;
      aux_q               <= aux_i;
      flush_q             <= flush_i;
      operands_q          <= operands_d;
      info_q[0].is_normal <= info_d[0].is_normal;
      info_q[1].is_normal <= info_d[1].is_normal;
      info_q[2].is_normal <= info_d[2].is_normal;
      info_q[2].is_zero   <= info_d[2].is_zero;
    end
  end

  logic [2:0][WIDTH-1:0] operands;
//  logic [2:0]            is_boxed; // 3 operands

  generate
    for (genvar i=0; i<3; i++) begin
      assign operands[i] = (current_state == FSM_EXP_ADD) ? operands_i[i] : operands_q[i];
//      assign is_boxed[i] = (current_state == FSM_EXP_ADD) ? is_boxed_i[i] : is_boxed_q[i];
    end
  endgenerate

  // Classify input
  fpnew_classifier #(
    .FpFormat    ( FpFormat ),
    .NumOperands ( 3        )
    ) i_class_inputs (
    .operands_i ( operands   ),
    .is_boxed_i ( is_boxed_i ),
    .info_o     ( info_d     )
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
    info_a = '0;
    info_b = '0;
    info_c = '0;
    // Default assignments - packing-order-agnostic
    if (current_state == FSM_EXP_ADD) begin
      operand_a = operands_i[0];
      operand_b = operands_i[1];
      operand_c = operands_i[2];
      info_a    = info_d[0];
      info_b    = info_d[1];
      info_c    = info_d[2];
      op        = op_i;
      op_mod    = op_mod_i;
    end
    else begin
      operand_a = operands_q[0];
      operand_b = operands_q[1];
      operand_c = operands_q[2];
      info_a.is_normal = info_q[0].is_normal;
      info_b.is_normal = info_q[1].is_normal;
      info_c.is_normal = info_q[2].is_normal;
      info_c.is_zero   = info_q[2].is_zero;
      op               = op_q;
      op_mod           = op_mod_q;
    end

    // op_mod_q inverts sign of operand C
    operand_c.sign = operand_c.sign ^ op_mod;

    unique case (op)
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
  assign effective_subtraction = operand_a.sign ^ operand_b.sign ^ operand_c.sign;
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

  // ---------------------------
  // Floatli fma FSM
  // ---------------------------
  logic signed [EXP_WIDTH-1:0]  exponent_addend, exponent_product_new, exponent_difference_new;
  logic signed [EXP_WIDTH-1:0]  exponent_product_old, exponent_difference_old;
  logic [EXP_WIDTH-1:0]         exponent_product_tmp;

  logic signed [EXP_WIDTH-1:0]  normalized_exponent_old;

  logic signed [EXP_WIDTH-1:0]  tentative_exponent;

  logic [SHIFT_AMOUNT_WIDTH-1:0] addend_shamt;

  logic [PRECISION_BITS-1:0]    mantissa_a, mantissa_b, mantissa_c;
  logic [2*PRECISION_BITS-1:0]  product_new;             // the p*p product is 2p bits wide
  logic [3*PRECISION_BITS+3:0]  product_shifted;      // addends are 3p+4 bit wide (including G/R)

  logic [3*PRECISION_BITS+3:0]  addend_after_shift_d; // upper 3p+4 bits are needed to go on
  logic [3*PRECISION_BITS+3:0]  addend_after_shift_q;


  logic [PRECISION_BITS-1:0]    addend_sticky_bits;  // up to p bit of shifted addend are sticky
  logic                         sticky_before_add;   // they are compressed into a single sticky bit
  logic [3*PRECISION_BITS+3:0]  addend_shifted;      // addends are 3p+4 bit wide (including G/R)
  logic [3*PRECISION_BITS+3:0]  addend_shifted_new, addend_shifted_old;
  logic                         inject_carry_in;     // inject carry for subtractions if needed

  logic [3*PRECISION_BITS+4:0]  sum_raw;   // added one bit for the carry
  logic                         sum_carry; // observe carry bit from sum for sign fixing
  logic [3*PRECISION_BITS+3:0]  sum;       // discard carry as sum won't overflow
  logic                         final_sign;

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
  assign normalized_exponent_old = {normalized_exponent_msbs_q ,operands_q[2].exponent};
  assign mantissa_b_old          = {mantissa_b_msb_q ,operands_q[1].mantissa};
  assign {addend_shifted_old, addend_sticky_bits} = {tmp_shift_q,
                                                     addend_after_shift_q[3*PRECISION_BITS+3:2*PRECISION_BITS],
                                                     operands_q[2].mantissa, mantissa_b_old, operands_q[0].mantissa};
  assign product_old             = addend_after_shift_q[2*PRECISION_BITS-1:0];

  assign mantissa_c_after_shift_old = {addend_after_shift_q, mantissa_b_old};

//non-comp
  logic operands_equal, operand_a_smaller;

  fp_t                sgnj_result;
  fpnew_pkg::status_t sgnj_status;
  logic               sgnj_extension_bit;

  logic sign_a, sign_b; // internal signs

  fp_t                minmax_result;
  fpnew_pkg::status_t minmax_status;
  logic               minmax_extension_bit;

  fp_t                cmp_result;
  fpnew_pkg::status_t cmp_status;
  logic               cmp_extension_bit;

  assign cmp_extension_bit = 1'b0; // Comparisons always produce booleans in integer registers

  assign minmax_extension_bit = 1'b1; // NaN-box as result is always a float value

  assign sgnj_status = '0;        // sign injections never raise exceptions

  // op_mod_q enables integer sign-extension of result (for storing to integer regfile)
  assign sgnj_extension_bit = op_mod ? sgnj_result.sign : 1'b1;

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
      current_state              <= FSM_EXP_ADD;
    end else begin
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
      current_state              <= next_state;
    end
  end

  always_comb
  begin : main_fsm_comb
    // real finite-state machine
    next_state                 = current_state;

    // signals
    exponent_product_new         = exponent_product_old;
    exponent_difference_new      = exponent_difference_old;

    addend_after_shift_d       = addend_after_shift_q;

    norm_shamt                 = addend_shamt;
    normalized_exponent_new      = normalized_exponent_old;
    sum_shifted                = '0;
    sum_shifted_tmp            = shift_out;

    {final_mantissa, sum_sticky_bits} = '0;
    final_exponent             = normalized_exponent_old;
    product_new                  = product_old;
    partial_product            = '0;
    mantissa_b_new               = mantissa_b_old;

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
    cmp_status                 = '1; // no flags
    sgnj_result                = '0;

    are_equal                  = '0;

    case(current_state)
      FSM_EXP_ADD: begin
        addend_after_shift_d         = '0;
        operands_d                   = operands;

        if ((op_i == fpnew_pkg::SGNJ) || (op_i == fpnew_pkg::MINMAX) || (op_i == fpnew_pkg::CMP) || (op_i == fpnew_pkg::CLASSIFY)) begin
          next_state = FSM_EXP_ADD;
          if (in_valid_i && in_ready_o) begin
            out_valid_o     = in_valid_i;
            busy_o          = in_valid_i;
            in_ready_o      = out_ready_i;
          end
          addend_a                      = operands[1];
          addend_b                      = ~operands[0];
          carry_in                      = 1'b1;
          are_equal                     = adder_result;
          exp_a                         = operands[1][WIDTH-2:(3*PRECISION_BITS+4)/3+1];
          exp_b                         = ~operands[0][WIDTH-2:(3*PRECISION_BITS+4)/3+1];
          exp_carry_in                  = are_equal[(3*PRECISION_BITS+4)/3+1];
          are_equal[WIDTH-1:(3*PRECISION_BITS+4)/3+1] = exp_adder_result;

          operands_equal    = (!are_equal) || (info_a.is_zero && info_b.is_zero);
          // Invert result if non-zero signs involved (unsigned comparison)
          //operand_a_smaller = (operand_a < operand_b) ^ (operand_a.sign || operand_b.sign);

          operand_a_smaller = (~are_equal[WIDTH-1]) ^ (operand_a.sign || operand_b.sign);

          // Default assignment
          sgnj_result = operand_a; // result based on operand a

          // NaN-boxing check will treat invalid inputs as canonical NaNs
          if (!info_a.is_boxed) sgnj_result = '{sign: 1'b0, exponent: '1, mantissa: 2**(MAN_BITS-1)};

          // Internal signs are treated as positive in case of non-NaN-boxed values
          sign_a = operand_a.sign & info_a.is_boxed;
          sign_b = operand_b.sign & info_b.is_boxed;

          // Do the sign injection based on rm field
          unique case (rnd_mode_i)
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
            unique case (rnd_mode_i)
              fpnew_pkg::RNE: minmax_result = operand_a_smaller ? operand_a : operand_b; // MIN
              fpnew_pkg::RTZ: minmax_result = operand_a_smaller ? operand_b : operand_a; // MAX
              default: minmax_result = '{default: fpnew_pkg::DONT_CARE}; // don't care
            endcase
          end

          // Signalling NaNs always compare as false and are illegal
          if (signalling_nan) cmp_status.NV = 1'b1; // invalid operation
          // Otherwise do comparisons
          else begin
            unique case (rnd_mode_i)
              fpnew_pkg::RNE: begin // Less than or equal
                if (any_operand_nan) cmp_status.NV = 1'b1; // Signalling comparison: NaNs are invalid
                else cmp_result = (operand_a_smaller | operands_equal) ^ op_mod_i;
              end
              fpnew_pkg::RTZ: begin // Less than
                if (any_operand_nan) cmp_status.NV = 1'b1; // Signalling comparison: NaNs are invalid
                else cmp_result = (operand_a_smaller & ~operands_equal) ^ op_mod_i;
              end
              fpnew_pkg::RDN: begin // Equal
                if (any_operand_nan) cmp_result = op_mod_i; // NaN always not equal
                else cmp_result = operands_equal ^ op_mod_i;
              end
              default: cmp_result = '{default: fpnew_pkg::DONT_CARE}; // don't care
            endcase
          end
        end
        else begin
          if ((in_valid_i && in_ready_o) && ~(result_is_special)) begin
            exp_a                      = exponent_a;
            exp_b                      = exponent_b;
            exp_carry_in               = info_a.is_subnormal;
            exponent_product_tmp       = exp_adder_result;

            exponent_product_new         = (info_a.is_zero || info_b.is_zero)
                                            ? 2 - signed'(BIAS) // in case the product is zero, set minimum exp.
                                            : signed'(exponent_product_tmp + info_b.is_subnormal
                                                - signed'(BIAS));
            {exp_prod_msbs_d ,operands_d[0].exponent} = exponent_product_new;

            busy_o                     = 1'b1;
            next_state = FSM_EXP_DIFF;
          end
          else begin
            if ((in_valid_i && in_ready_o) && (result_is_special)) begin
              busy_o      = 1'b1;
              out_valid_o = 1'b1;
            end
            else begin
              busy_o    = 1'b0;
            end
            next_state = FSM_EXP_ADD;
          end
        end
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
        if (op == fpnew_pkg::ADD) begin
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
        addend_shifted_new     = addend_shifted_old;

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
            norm_shamt            = PRECISION_BITS + 2 + leading_zero_count;

            //   normalized_exponent = {exp_prod_msbs_q ,operands_q[0].exponent} - leading_zero_count_sgn + 1;
            // account for shift
            exp_a                 = exponent_product_old;
            exp_b                 = -leading_zero_count_sgn;
            exp_carry_in          = 1'b1;
            normalized_exponent_new = exp_adder_result;
          // Subnormal result
          end else begin
            // Cap the shift distance to align mantissa with minimum exponent
            norm_shamt            = unsigned'(signed'(PRECISION_BITS) + 2 + exponent_product_old);
            normalized_exponent_new = 0; // subnormals encoded as 0
          end
        // Addend-anchored case
        end else begin
          norm_shamt            = addend_shamt; // Undo the initial shift
          normalized_exponent_new = tentative_exponent;
        end
        {normalized_exponent_msbs_d ,operands_d[2].exponent} = normalized_exponent_new;

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
          {tmp_shift_d, addend_after_shift_d[(3*PRECISION_BITS+4)*2/3-1:0]} = shift_out;
          next_state                       = FSM_ROUNDING;
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
            ({tmp_shift_q, addend_after_shift_q[(3*PRECISION_BITS+4)*2/3-1:0]} << (3*PRECISION_BITS+4)/3)
            | {carry_add_q, operands_q[2].mantissa, operands_q[0].mantissa, mantissa_b_old, tmp_q};

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
          next_state    = FSM_EXP_ADD;
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

  sum_raw_third_adder #(
    .PRECISION_BITS  ( PRECISION_BITS  )
  ) i_sum_raw_third_adder (
    .product_shifted ( addend_a        ),
    .addend_shifted  ( addend_b        ),
    .inject_carry_in ( carry_in        ),
    .sum_raw         ( adder_result    )
  );

  exp_adder #(
    .EXP_WIDTH ( EXP_WIDTH )
  ) i_exp_adder (
    .exp_a            ( exp_a            ),
    .exp_b            ( exp_b            ),
    .exp_carry_in     ( exp_carry_in     ),
    .exp_adder_result ( exp_adder_result )
  );

  shift_fma_word2shift_third #(
    .PRECISION_BITS      ( PRECISION_BITS     ),
    .SHIFT_AMOUNT_WIDTH  ( SHIFT_AMOUNT_WIDTH )
  ) i_shift_fma_word2shift_third (
    .sum                 ( shift_in           ),
    .norm_shamt          ( shift_amount       ),
    .sum_shifted         ( shift_out          )
  );

  reduced_mantissa_multiplier #(
    .PRECISION_BITS (PRECISION_BITS)
  ) i_mul_2bit (
    .mantissa_a       ( factor_a  ),
    .mantissa_b       ( factor_b  ),
    .product          ( prod      )
  );

  // Calculate internal exponents from encoded values. Real exponents are (ex = Ex - bias + 1 - nx)
  // with Ex the encoded exponent and nx the implicit bit. Internal exponents stay biased.
  assign exponent_addend = signed'(exponent_c + $signed({1'b0, ~info_c.is_normal})); // 0 as subnorm
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
  assign mantissa_a = {info_a.is_normal, operand_a.mantissa};
  assign mantissa_b = {info_b.is_normal, operand_b.mantissa};
  assign mantissa_c = {info_c.is_normal, operand_c.mantissa};

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
  //

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
    unique case (op_i)
      fpnew_pkg::SGNJ: begin
        result_o        = sgnj_result;
        status_o        = sgnj_status;
        extension_bit_o= sgnj_extension_bit;
      end
      fpnew_pkg::MINMAX: begin
        result_o        = minmax_result;
        status_o        = minmax_status;
        extension_bit_o= minmax_extension_bit;
      end
      fpnew_pkg::CMP: begin
        result_o        = cmp_result;
        status_o        = cmp_status;
        extension_bit_o= cmp_extension_bit;
      end
      fpnew_pkg::CLASSIFY: begin
        result_o        = '{default: fpnew_pkg::DONT_CARE}; // unused
        status_o        = class_status;
        extension_bit_o= class_extension_bit;
      end
      fpnew_pkg::FMADD: begin
        result_o        = result_is_special ? special_result : regular_result;
        status_o        = result_is_special ? special_status : regular_status;
        extension_bit_o = 1'b1; // always NaN-Box result
      end
      fpnew_pkg::FNMSUB: begin
        result_o        = result_is_special ? special_result : regular_result;
        status_o        = result_is_special ? special_status : regular_status;
        extension_bit_o = 1'b1; // always NaN-Box result
      end
      fpnew_pkg::ADD: begin
        result_o        = result_is_special ? special_result : regular_result;
        status_o        = result_is_special ? special_status : regular_status;
        extension_bit_o = 1'b1; // always NaN-Box result
      end
      fpnew_pkg::MUL: begin
        result_o        = result_is_special ? special_result : regular_result;
        status_o        = result_is_special ? special_status : regular_status;
        extension_bit_o = 1'b1; // always NaN-Box result
      end
      default: begin
        result_o        = '{default: fpnew_pkg::DONT_CARE}; // dont care
        status_o        = '{default: fpnew_pkg::DONT_CARE}; // dont care
        extension_bit_o= fpnew_pkg::DONT_CARE;             // dont care
      end
    endcase
  end

  //non_comb
  assign is_class_o = (op_i == fpnew_pkg::CLASSIFY);

  //to be defined
  assign tag_o           = tag_q;
  assign aux_o           = aux_q;
endmodule
