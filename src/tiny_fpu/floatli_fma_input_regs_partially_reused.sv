// Copyright 2019, 2020 ETH Zurich and University of Bologna.
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
module floatli_fma #(
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
  parameter int unsigned  PRECISION_BITS = MAN_BITS + 1;
  // The lower 2p+3 bits of the internal FMA result will be needed for leading-zero detection
  localparam int unsigned LOWER_SUM_WIDTH  = 2 * PRECISION_BITS + 3;
  localparam int unsigned LZC_RESULT_WIDTH = $clog2(LOWER_SUM_WIDTH);
  // Internal exponent width of FMA must accomodate all meaningful exponent values in order to avoid
  // datapath leakage. This is either given by the exponent bits or the width of the LZC result.
  // In most reasonable FP formats the internal exponent will be wider than the LZC result.
  parameter int unsigned  EXP_WIDTH = unsigned'(fpnew_pkg::maximum(EXP_BITS + 2, LZC_RESULT_WIDTH));
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

  // Input registers
  logic                  enable_input_reg;
  logic                  enable_operands_reg;
  logic                  clear;

  logic [2:0]            is_boxed_q; // 3 operands
  fpnew_pkg::roundmode_e rnd_mode_q;
  fpnew_pkg::operation_e op_q;
  logic                  op_mod_q;
  TagType                tag_q;
  AuxType                aux_q;
  logic                  flush_q;

  fp_t [2:0]             operands_d;
  fp_t [2:0]             operands_q;


  always_ff @(posedge clk_i or negedge rst_ni) begin : input_regs
    if(~rst_ni) begin
      is_boxed_q      <= '0;
      rnd_mode_q      <= fpnew_pkg::RNE;
      op_q            <= fpnew_pkg::FMADD;
      op_mod_q        <= '0;
      tag_q           <= '0;
      aux_q           <= '0;
      flush_q         <= '0;
    end
    else if (clear) begin
      is_boxed_q      <= '0;
      rnd_mode_q      <= fpnew_pkg::RNE;
      op_q            <= fpnew_pkg::FMADD;
      op_mod_q        <= '0;
      tag_q           <= '0;
      aux_q           <= '0;
      flush_q         <= '0;
    end
    else if (enable_input_reg) begin
      is_boxed_q      <= is_boxed_i;
      rnd_mode_q      <= rnd_mode_i;
      op_q            <= op_i;
      op_mod_q        <= op_mod_i;
      tag_q           <= tag_i;
      aux_q           <= aux_i;
      flush_q         <= flush_i;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni) begin : proc_regs
    if(~rst_ni) begin
      operands_q      <= '0;
    end
    else if (clear) begin
      operands_q      <= '0;
    end
    else if (enable_operands_reg) begin
      operands_q      <= operands_d;
    end
  end

  // -----------------
  // Input processing
  // -----------------
  fpnew_pkg::fp_info_t [2:0] info_d, info_q;
  logic                      enable_info_reg;

  logic [2:0][WIDTH-1:0] operands;
  logic [2:0]            is_boxed; // 3 operands


  generate
    for (genvar i=0; i<3; i++) begin
      assign operands[i] = (current_state == FSM_EXP_ADD) ? operands_i[i] : operands_q[i];
      assign is_boxed[i] = (current_state == FSM_EXP_ADD) ? is_boxed_i[i] : is_boxed_q[i];
    end
  endgenerate

  // Classify input
  fpnew_classifier #(
    .FpFormat    ( FpFormat ),
    .NumOperands ( 3        )
    ) i_class_inputs (
    .operands_i ( operands   ),
    .is_boxed_i ( is_boxed   ),
    .info_o     ( info_d     )
  );

  always_ff @(posedge clk_i or negedge rst_ni) begin : info_regs
    if(~rst_ni) begin
      info_q      <= '0;
    end
    else if (clear) begin
      info_q      <= '0;
    end
    else if (enable_info_reg) begin
      info_q      <= info_d;
    end
  end

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

    // Default assignments - packing-order-agnostic
    if (current_state == FSM_EXP_ADD) begin
      operand_a = operands_i[0];
      operand_b = operands_i[1];
      operand_c = operands_i[2];
      info_a    = info_d[0];
      info_b    = info_d[1];
      info_c    = info_d[2];
    end
    else begin
      operand_a = operands_q[0];
      operand_b = operands_q[1];
      operand_c = operands_q[2];
      info_a    = info_q[0];
      info_b    = info_q[1];
      info_c    = info_q[2];
    end

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
  logic signed [EXP_WIDTH-1:0]  exponent_addend, exponent_product_d, exponent_difference_d;
  logic signed [EXP_WIDTH-1:0]  exponent_product_q, exponent_difference_q;
  logic [EXP_WIDTH-1:0]         exponent_product_tmp;
  logic                         enable_exponent_product, enable_exponent_difference;

  logic signed [EXP_WIDTH-1:0]  normalized_exponent_q;
  logic                         enable_normalized_exponent;

  logic signed [EXP_WIDTH-1:0]  tentative_exponent;

  logic [SHIFT_AMOUNT_WIDTH-1:0] addend_shamt;

  logic [PRECISION_BITS-1:0]    mantissa_a, mantissa_b, mantissa_c;
  logic [2*PRECISION_BITS-1:0]  product_d;             // the p*p product is 2p bits wide
  logic [3*PRECISION_BITS+3:0]  product_shifted;      // addends are 3p+4 bit wide (including G/R)

  logic [3*PRECISION_BITS+3:0]  addend_after_shift_d; // upper 3p+4 bits are needed to go on
  logic [3*PRECISION_BITS+3:0]  addend_after_shift_q;
  logic                         enable_addend_shift;


  logic [PRECISION_BITS-1:0]    addend_sticky_bits;  // up to p bit of shifted addend are sticky
  logic                         sticky_before_add;   // they are compressed into a single sticky bit
  logic [3*PRECISION_BITS+3:0]  addend_shifted;      // addends are 3p+4 bit wide (including G/R)
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
  logic signed [EXP_WIDTH-1:0]          normalized_exponent_d;

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

  logic [(3*PRECISION_BITS+4)/2:0]   sum_raw_tmp;

  // Counter for multicycle multiplication
  logic                              enable_product;
  logic [$clog2(PRECISION_BITS)-1:0] mul_count_q;
  logic [2*PRECISION_BITS-1:0]       product_q;
  // Register for mantissa_b
  logic [PRECISION_BITS-1:0]         mantissa_b_q, mantissa_b_d;
  logic                              enable_m_b_reg;
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
  logic [(3*PRECISION_BITS+4)/3-1:0] shift_in;
  logic [SHIFT_AMOUNT_WIDTH-1:0]     shift_amount;
  logic [4*PRECISION_BITS+3:0]       shift_out;
  // Temporal signals used for the shift on mantissa_c
  logic [4*PRECISION_BITS+3:0]       shift_out_tmp, reversed_shift_out_tmp;

  // Counter for multicycle shift
  logic                              shift_count_q;
  logic                              enable_shift_counter;

  logic [4:0]                        tmp_d, tmp_q;

  // Reduced multiplier
  logic [PRECISION_BITS-1:0]         factor_a;
  logic [1:0]                        factor_b;
  logic [PRECISION_BITS+1:0]         prod;

  logic [PRECISION_BITS+1:0]         partial_product;

  logic                              msb_add_d, msb_add_q;
  logic                              carry_add_d, carry_add_q;
  logic                              enable_carry_add;

  // Counter for multicycle addition
  logic                              enable_add_counter;
  logic                              clear_add_counter;
  logic [1:0]                        add_count_q;

  logic                              enable_tmp_shift;
  logic [1:0]                        tmp_shift_d, tmp_shift_q;

  logic [EXP_WIDTH-EXP_BITS-1:0]     exp_prod_msbs_q, exp_prod_msbs_d;
  logic [EXP_WIDTH-EXP_BITS-1:0]     exp_diff_msbs_q, exp_diff_msbs_d;
  logic [EXP_WIDTH-EXP_BITS-1:0]     normalized_exponent_msbs_q, normalized_exponent_msbs_d;

  logic [4*PRECISION_BITS+3:0]       mantissa_c_after_shift_q;

  assign exponent_product_q       = {exp_prod_msbs_q ,operands_q[0].exponent};
  assign exponent_difference_q    = {exp_diff_msbs_q ,operands_q[1].exponent};
  assign normalized_exponent_q    = {normalized_exponent_msbs_q ,operands_q[2].exponent};
  assign mantissa_b_q             = {mantissa_b_msb_q ,operands_q[1].mantissa};

  assign mantissa_c_after_shift_q = {addend_after_shift_q, mantissa_b_q};

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      exp_prod_msbs_q    <= '0;
    end
    else if(clear) begin
      exp_prod_msbs_q    <= '0;
    end
    else if (enable_exponent_product) begin
      exp_prod_msbs_q    <= exp_prod_msbs_d;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      exp_diff_msbs_q       <= '0;
    end
    else if(clear) begin
      exp_diff_msbs_q       <= '0;
    end
    else if (enable_exponent_difference) begin
      exp_diff_msbs_q       <= exp_diff_msbs_d;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      addend_after_shift_q <= '0;
    end
    else if(clear) begin
      addend_after_shift_q <= '0;
    end
    else if (enable_addend_shift) begin
      addend_after_shift_q <= addend_after_shift_d;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      add_count_q <= '0;
    end
    else if(clear || clear_add_counter) begin
      add_count_q <= '0;
    end
    else if (enable_add_counter) begin
      add_count_q <= add_count_q + 1;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      carry_add_q <= '0;
      msb_add_q   <= '0;
    end
    else if(clear) begin
      carry_add_q <= '0;
      msb_add_q   <= '0;
    end
    else if (enable_carry_add) begin
      carry_add_q <= carry_add_d;
      msb_add_q   <= msb_add_d;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      shift_count_q <= '0;
    end
    else if(clear) begin
      shift_count_q <= '0;
    end
    else if (enable_shift_counter) begin
      shift_count_q <= shift_count_q + 1;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      product_q     <= '0;
      mul_count_q   <= '0;
    end
    else if(clear) begin
      product_q     <= '0;
      mul_count_q   <= '0;
    end
    else if (enable_product) begin
      product_q     <= product_d;
      mul_count_q   <= mul_count_q + 1;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      mantissa_b_msb_q  <= '0;
    end
    else if(clear) begin
      mantissa_b_msb_q  <= '0;
    end
    else if (enable_m_b_reg) begin
      mantissa_b_msb_q  <= mantissa_b_msb_d;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      normalized_exponent_msbs_q <= '0;
    end
    else if(clear) begin
      normalized_exponent_msbs_q <= '0;
    end
    else if (enable_normalized_exponent) begin
      normalized_exponent_msbs_q <= normalized_exponent_msbs_d;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      tmp_q <= '0;
    end
    else if(clear) begin
      tmp_q <= '0;
    end
    else if (enable_shift_counter) begin
      tmp_q <= tmp_d;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin
    if(~rst_ni) begin
      tmp_shift_q <= '0;
    end
    else if(clear) begin
      tmp_shift_q <= '0;
    end
    else if (enable_tmp_shift) begin
      tmp_shift_q <= tmp_shift_d;
    end
  end

  always_ff @(posedge clk_i or negedge rst_ni)
  begin : main_fsm_seq
    if(~rst_ni) begin
      current_state <= FSM_EXP_ADD;
    end
    else if(clear) begin
      current_state <= FSM_EXP_ADD;
    end
    else begin
      current_state <= next_state;
    end
  end

  always_comb
  begin : main_fsm_comb

    clear                      = 1'b0;
    enable_input_reg           = 1'b0;

    // real finite-state machine
    next_state                 = current_state;

    // enable signals
    enable_exponent_product    = 1'b0;
    enable_exponent_difference = 1'b0;
    enable_addend_shift        = 1'b0;
    enable_normalized_exponent = 1'b0;
    enable_product             = 1'b0;
    enable_shift_counter       = 1'b0;
    enable_m_b_reg             = 1'b0;
    enable_add_counter         = 1'b0;
    enable_carry_add           = 1'b0;
    enable_tmp_shift           = 1'b0;
    enable_info_reg            = 1'b0;
    enable_operands_reg        = 1'b0;

    // signals
    exponent_product_d         = '0;
    exponent_difference_d      = '0;

    {addend_after_shift_d, addend_sticky_bits} = '0;
    sum_raw_tmp                = '0;

    norm_shamt                 = '0;
    normalized_exponent_d      = '0;
    sum_shifted                = '0;
    sum_shifted_tmp            = '0;

    {final_mantissa, sum_sticky_bits} = '0;
    final_exponent             = '0;
    product_d                  = '0;
    partial_product            = '0;
    mantissa_b_d               = '0;

    addend_a                   = '0;
    addend_b                   = '0;
    carry_in                   = 1'b0;

    exp_a                      = '0;
    exp_b                      = '0;
    exp_carry_in               = 1'b0;
    exponent_product_tmp       = '0;

    shift_in                   = '0;
    shift_amount               = '0;
    shift_out_tmp              = '0;
    reversed_shift_out_tmp     = '0;
    mantissa_c_tmp             = '0;
    addend_shifted             = '0;

    tmp_d                      = '0;

    factor_a                   = '0;
    factor_b                   = '0;

    rounded_abs                = '0;
    result_zero                = 1'b0;

    rounded_sign               = '0;

    carry_add_d                = 1'b0;
    msb_add_d                  = 1'b0;

    clear_add_counter          = '0;
    sticky_before_add          = '0;
    product_shifted            = '0;
    inject_carry_in            = '0;
    round_up                   = '0;
    tmp_shift_d                = '0;

    out_valid_o                = 1'b0;
    in_ready_o                 = 1'b1;
    busy_o                     = 1'b0;

    operands_d                 = operands_q;
    exp_prod_msbs_d            = '0;
    exp_diff_msbs_d            = '0;
    normalized_exponent_msbs_d = '0;
    mantissa_b_msb_d           = '0;


    case(current_state)
      FSM_EXP_ADD: begin
      enable_input_reg             = 1'b1;
      enable_operands_reg          = 1'b1;
      enable_info_reg              = 1'b1;

      operands_d                   = operands;

        if ((in_valid_i && in_ready_o) && ~(result_is_special) && ~(info_a.is_zero || info_b.is_zero)) begin
          enable_exponent_product    = 1'b1;
          exp_a                      = exponent_a;
          exp_b                      = exponent_b;
          exp_carry_in               = info_a.is_subnormal;
          exponent_product_tmp       = exp_adder_result;

          exponent_product_d         = signed'(exponent_product_tmp + info_b.is_subnormal
                                              - signed'(BIAS));
          {exp_prod_msbs_d ,operands_d[0].exponent} = exponent_product_d;

          busy_o                     = 1'b1;
          next_state = FSM_EXP_DIFF;
        end
        else if ((in_valid_i && in_ready_o) && ~(result_is_special) && (info_a.is_zero || info_b.is_zero)) begin
          enable_exponent_product    = 1'b1;
          exponent_product_d         = 2 - signed'(BIAS);
          enable_exponent_difference = 1'b1;
          {exp_prod_msbs_d ,operands_d[0].exponent} = exponent_product_d;


          // exponent_difference        = exponent_addend - exponent_product;
          exp_a                      = exponent_addend;
          exp_b                      = ~exponent_product_d;
          exp_carry_in               = 1'b1;
          exponent_difference_d      = exp_adder_result;
          {exp_diff_msbs_d ,operands_d[1].exponent} = exponent_difference_d;
          enable_exponent_difference = 1'b1;
          next_state                 = FSM_MANTISSA_PROD_ADDEND_SHIFT;
          busy_o                     = 1'b1;
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
      FSM_EXP_DIFF: begin
        busy_o                = 1'b1;
        in_ready_o            = 1'b0;
        enable_exponent_difference   = 1'b1;
        enable_operands_reg          = 1'b1;

        // exponent_difference        = exponent_addend - exponent_product;
        exp_a                 = exponent_addend;
        exp_b                 = ~{exp_prod_msbs_q, operands_q[0].exponent};
        exp_carry_in          = 1'b1;
        exponent_difference_d = exp_adder_result;
        {exp_diff_msbs_d, operands_d[1].exponent} = exponent_difference_d;

        enable_m_b_reg = 1'b1;

        next_state = FSM_MANTISSA_PROD_ADDEND_SHIFT;
        if (mul_count_q == 0) begin
          mantissa_b_d = mantissa_b;
          {mantissa_b_msb_d, operands_d[1].mantissa} = mantissa_b_d;
        end
      end
      FSM_MANTISSA_PROD_ADDEND_SHIFT: begin
        busy_o              = 1'b1;
        in_ready_o          = 1'b0;
        enable_addend_shift = 1'b1;
        enable_product      = 1'b1;
        enable_m_b_reg      = 1'b1;
        product_d           = product_q;
        mantissa_b_d        = mantissa_b_q;

        // The right shift on mantissa_c is realized with a left shifter
        // The input and the output of the left shifter have to be bit-reversed to obtaing the right
        // shift

        // Bit-reverting mantissa_c
        for (int i=0; i<PRECISION_BITS; i++) begin
          mantissa_c_tmp [PRECISION_BITS-1-i] = mantissa_c[i];
        end
        // Left shift
        shift_in     = mantissa_c_tmp;
        shift_amount = addend_shamt;
        shift_out_tmp = shift_out;
        // Bit-reverting shifter output
        for (int i=0; i<4*PRECISION_BITS+4; i++) begin
          reversed_shift_out_tmp[4*PRECISION_BITS+4-1-i] = shift_out_tmp[i];
        end

        // when computing an addition (A = 1) the multiplication is realized shifting mantissa_b
        if (op_q == fpnew_pkg::ADD) begin
          product_d  = mantissa_b << (PRECISION_BITS-1);
          next_state = FSM_SUM;
          {addend_after_shift_d, mantissa_b_d} = reversed_shift_out_tmp;
        end
        else begin

          // Product is placed into a 3p+4 bit wide vector, padded with 2 bits for round and sticky:
          // | 000...000 | product | RS |
          //  <-  p+2  -> <-  2p -> < 2>
          factor_a            = mantissa_a;
          factor_b            = mantissa_b_q[1:0];;
          partial_product     = prod;

          if ((mul_count_q == PRECISION_BITS/2) && (FpFormat == fpnew_pkg::fp_format_e'(1)))
            addend_a          = product_q[(mul_count_q<<1)+:PRECISION_BITS+1];
          else
            addend_a          = product_q[(mul_count_q<<1)+:PRECISION_BITS+2];

          addend_b            = partial_product;
          carry_in            = '0;
          product_d[(mul_count_q<<1)+:PRECISION_BITS+2]    = adder_result[PRECISION_BITS+2:0];

          if (((mul_count_q == PRECISION_BITS/2) && (FpFormat == fpnew_pkg::fp_format_e'(1)))
              || (((mul_count_q == PRECISION_BITS/2-1) && (FpFormat == fpnew_pkg::fp_format_e'(0))))) begin

            next_state = FSM_SUM;
            {addend_after_shift_d, mantissa_b_d} = reversed_shift_out_tmp;
          end
          else begin
            next_state = FSM_MANTISSA_PROD_ADDEND_SHIFT;
            mantissa_b_d = mantissa_b_q >> 2;
          end
        end
        {mantissa_b_msb_d, operands_d[1].mantissa} = mantissa_b_d;
        enable_operands_reg                        = 1'b1;
      end
      FSM_SUM: begin
        busy_o             = 1'b1;
        in_ready_o         = 1'b0;
        enable_add_counter = 1'b1;

        product_shifted    = product_q << 2;

        // In case of a subtraction, the addend is inverted
        addend_shifted       = (effective_subtraction) ? ~addend_after_shift_q : addend_after_shift_q;
        addend_after_shift_d = addend_after_shift_q;

        sticky_before_add    = (| mantissa_b_q);
        inject_carry_in      = effective_subtraction & ~sticky_before_add;

        enable_carry_add = 1'b1;
        enable_addend_shift = 1'b1;

        if (add_count_q == 0) begin
          addend_after_shift_d[(3*PRECISION_BITS+4)/3-1:0] = addend_shifted[(3*PRECISION_BITS+4)/3-1:0];

          addend_a       = product_shifted[(3*PRECISION_BITS+4)/3-1:0];
          addend_b       = addend_after_shift_d[(3*PRECISION_BITS+4)/3-1:0];
          carry_in       = inject_carry_in;
          {carry_add_d, addend_after_shift_d[(3*PRECISION_BITS+4)/3-1:0]}
              = adder_result[(3*PRECISION_BITS+4)/3:0];

          next_state     = FSM_SUM;
        end
        else if (add_count_q == 1) begin
          addend_after_shift_d[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3]
                               = addend_shifted[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3];

          addend_a       = product_shifted[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3];
          addend_b       = addend_after_shift_d[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3];
          carry_in       = carry_add_q;
          {carry_add_d, addend_after_shift_d[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3]}
              = adder_result[(3*PRECISION_BITS+4)/3:0];

          next_state     = FSM_SUM;
        end
        else begin
          clear_add_counter = 1'b1;
          addend_after_shift_d[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3]
              = addend_shifted[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3];

          addend_a       = product_shifted[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3];
          addend_b       = addend_after_shift_d[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3];
          carry_in       = carry_add_q;
          {msb_add_d, addend_after_shift_d[(3*PRECISION_BITS+4)-1:(3*PRECISION_BITS+4)*2/3]}
              = adder_result[(3*PRECISION_BITS+4)/3+1:0];

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
        enable_add_counter    = 1'b1;
        in_ready_o            = 1'b0;
        addend_after_shift_d  = addend_after_shift_q;

        if (add_count_q == 0) begin
          addend_a            = {~addend_after_shift_q[(3*PRECISION_BITS+4)/3-1:0]};
          addend_b            = '0;
          carry_in            = 1'b1;
          {carry_add_d, addend_after_shift_d[(3*PRECISION_BITS+4)/3-1:0]}
              = adder_result[(3*PRECISION_BITS+4)/3:0];

          enable_carry_add    = 1'b1;
          enable_addend_shift = 1'b1;
          next_state          = FSM_COMPLEMENT_SUM;
        end
        else if (add_count_q == 1) begin
          addend_a            = {~addend_after_shift_q[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3]};
          addend_b            = 1'b0;
          carry_in            = carry_add_q;
          {carry_add_d, addend_after_shift_d[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3]}
                              = adder_result[(3*PRECISION_BITS+4)/3:0];

          enable_carry_add    = 1'b1;
          enable_addend_shift = 1'b1;
          next_state          = FSM_COMPLEMENT_SUM;
        end
        else begin
          addend_a            = {~msb_add_q,
                              ~addend_after_shift_q[(3*PRECISION_BITS+3):(3*PRECISION_BITS+4)*2/3]};
          addend_b            = 1'b0;
          carry_in            = carry_add_q;
          {msb_add_d, addend_after_shift_d[(3*PRECISION_BITS+4)-1:(3*PRECISION_BITS+4)*2/3]}
                              = adder_result[(3*PRECISION_BITS+4)/3+1:0];

          enable_carry_add    = 1'b1;
          enable_addend_shift = 1'b1;
          next_state          = FSM_NORMALIZATION;
        end
      end

      FSM_NORMALIZATION: begin
        busy_o               = 1'b1;
        in_ready_o           = 1'b0;
        enable_shift_counter = 1'b1;
        tmp_d                = tmp_q;
        addend_after_shift_d = addend_after_shift_q;

        enable_normalized_exponent = 1'b1;
        // Normalization shift amount based on exponents and LZC (unsigned as only left shifts)

        if ((exponent_difference_q <= 0) || (effective_subtraction
              && (exponent_difference_q <= 2))) begin
          // Normal result (biased exponent > 0 and not a zero)
          if ((exponent_product_q - leading_zero_count_sgn + 1 >= 0) && !lzc_zeroes) begin
            // Undo initial product shift, remove the counted zeroes
            norm_shamt            = PRECISION_BITS + 2 + leading_zero_count;

            //   normalized_exponent = {exp_prod_msbs_q ,operands_q[0].exponent} - leading_zero_count_sgn + 1;
            // account for shift
            exp_a                 = exponent_product_q;
            exp_b                 = -leading_zero_count_sgn;
            exp_carry_in          = 1'b1;
            normalized_exponent_d = exp_adder_result;
          // Subnormal result
          end else begin
            // Cap the shift distance to align mantissa with minimum exponent
            norm_shamt            = unsigned'(signed'(PRECISION_BITS) + 2 + exponent_product_q);
            normalized_exponent_d = 0; // subnormals encoded as 0
          end
        // Addend-anchored case
        end else begin
          norm_shamt            = addend_shamt; // Undo the initial shift
          normalized_exponent_d = tentative_exponent;
        end
        {normalized_exponent_msbs_d ,operands_d[2].exponent} = normalized_exponent_d;

        if (shift_count_q == 0) begin
          shift_in                         = sum[(3*PRECISION_BITS+4)/3-1:0];
          shift_amount                     = norm_shamt;
          sum_shifted                      = shift_out[(3*PRECISION_BITS+4):0];
          {product_d, mantissa_b_d, tmp_d} = shift_out[(3*PRECISION_BITS+4):0];
          {mantissa_b_msb_d, operands_d[1].mantissa} = mantissa_b_d;
          enable_operands_reg              = 1'b1;
          enable_product                   = 1'b1;
          enable_m_b_reg                   = 1'b1;
          next_state                       = FSM_NORMALIZATION;
        end
        else begin
          shift_in                         = sum[(3*PRECISION_BITS+4)*2/3-1:(3*PRECISION_BITS+4)/3];
          shift_amount                     = norm_shamt;
          {tmp_shift_d, addend_after_shift_d[(3*PRECISION_BITS+4)*2/3-1:0]} = shift_out;
          enable_addend_shift              = 1'b1;
          enable_tmp_shift                 = 1'b1;
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


        if ((exponent_difference_q <= 0) || (effective_subtraction && (exponent_difference_q <= 2))) begin
          // Normal result (biased exponent > 0 and not a zero)
          if ((exponent_product_q - leading_zero_count_sgn + 1 >= 0) && !lzc_zeroes) begin
            // Undo initial product shift, remove the counted zeroes
            norm_shamt          = PRECISION_BITS + 2 + leading_zero_count;
          // Subnormal result
          end else begin
            // Cap the shift distance to align mantissa with minimum exponent
            norm_shamt          = unsigned'(signed'(PRECISION_BITS) + 2 + exponent_product_q);
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
            | {product_q, mantissa_b_q, tmp_q};

        {final_mantissa, sum_sticky_bits} = sum_shifted;
        final_exponent                    = normalized_exponent_q;

        // The normalized sum has overflown, align right and fix exponent
        if (sum_shifted[3*PRECISION_BITS+4]) begin // check the carry bit
          {final_mantissa, sum_sticky_bits} = sum_shifted >> 1;
          final_exponent                    = normalized_exponent_q + 1;
        // The normalized sum is normal, nothing to do
        end else if (sum_shifted[3*PRECISION_BITS+3]) begin // check the sum MSB
          // do nothing
        // The normalized sum is still denormal, align left - unless the result is not already
        // subnormal
        end else if (normalized_exponent_q > 1) begin
          {final_mantissa, sum_sticky_bits} = sum_shifted << 1;
          final_exponent                    = normalized_exponent_q - 1;
        // Otherwise we're denormal
        end else begin
          final_exponent = '0;
        end

        if (out_ready_i && out_valid_o) begin
          clear         = 1'b1;
          next_state    = FSM_EXP_ADD;
        end
        else begin
          clear         = 1'b0;
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
    if (exponent_difference_q <= signed'(-2 * PRECISION_BITS - 1))
      addend_shamt = 3 * PRECISION_BITS + 4;
    // Addend and product will have mutual bits to add
    else if (exponent_difference_q <= signed'(PRECISION_BITS + 2))
      addend_shamt = unsigned'(signed'(PRECISION_BITS) + 3 - exponent_difference_q);
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

  // Select output depending on special case detection
  assign result_o = result_is_special ? special_result : regular_result;
  assign status_o = result_is_special ? special_status : regular_status;

  assign extension_bit_o = 1'b1; // always NaN-Box result

  //to be defined
  assign tag_o           = tag_q;
  assign aux_o           = aux_q;
endmodule
