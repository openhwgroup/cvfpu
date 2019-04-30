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

module fpnew_fma #(
  parameter fpnew_pkg::fp_format_e   FpFormat    = fpnew_pkg::FP32,
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
  localparam int unsigned PRECISION_BITS = MAN_BITS + 1;
  // The lower 2p+3 bits of the internal FMA result will be needed for leading-zero detection
  localparam int unsigned LOWER_SUM_WIDTH  = 2 * PRECISION_BITS + 3;
  localparam int unsigned LZC_RESULT_WIDTH = $clog2(LOWER_SUM_WIDTH);
  // Internal exponent width of FMA must accomodate all meaningful exponent values in order to avoid
  // datapath leakage. This is either given by the exponent bits or the width of the LZC result.
  // In most reasonable FP formats the internal exponent will be wider than the LZC result.
  localparam int unsigned EXP_WIDTH = fpnew_pkg::maximum(EXP_BITS + 2, LZC_RESULT_WIDTH);
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

  // ---------------
  // Input pipeline
  // ---------------
  // Pipelined input signals
  logic [2:0][WIDTH-1:0] operands_q;
  logic [2:0]            is_boxed_q;
  fpnew_pkg::roundmode_e rnd_mode_q;
  fpnew_pkg::operation_e op_q;
  logic                  op_mod_q;
  TagType                tag_q;
  AuxType                aux_q;
  logic                  out_valid_input;
  logic                  in_ready_inside; // written by inside pipeline
  logic                  busy_input;

  // Generate pipeline at input if needed
  if (PipeConfig==fpnew_pkg::BEFORE || PipeConfig==fpnew_pkg::DISTRIBUTED) begin : input_pipeline
    localparam NUM_REGS = PipeConfig==fpnew_pkg::DISTRIBUTED
                          ? (NumPipeRegs / 3) // Last to get regs
                          : NumPipeRegs;
    fpnew_pipe_in #(
      .Width       ( WIDTH       ),
      .NumPipeRegs ( NUM_REGS    ),
      .NumOperands ( 3           ),
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
      .src_fmt_i      ( fpnew_pkg::FP32 ), // unused
      .dst_fmt_i      ( fpnew_pkg::FP32 ), // unused
      .int_fmt_i      ( fpnew_pkg::INT8 ), // unused
      .tag_i,
      .aux_i,
      .in_valid_i,
      .in_ready_o,
      .flush_i,
      .operands_o     ( operands_q       ),
      .is_boxed_o     ( is_boxed_q       ),
      .rnd_mode_o     ( rnd_mode_q       ),
      .op_o           ( op_q             ),
      .op_mod_o       ( op_mod_q         ),
      .src_fmt_o      ( /* unused */     ),
      .dst_fmt_o      ( /* unused */     ),
      .int_fmt_o      ( /* unused */     ),
      .tag_o          ( tag_q            ),
      .aux_o          ( aux_q            ),
      .out_valid_o    ( out_valid_input  ),
      .out_ready_i    ( in_ready_inside  ),
      .busy_o         ( busy_input       )
    );
  // Otherwise pass through inputs
  end else begin : no_input_pipeline
    assign in_ready_o      = in_ready_inside;
    assign operands_q      = operands_i;
    assign is_boxed_q      = is_boxed_i;
    assign rnd_mode_q      = rnd_mode_i;
    assign op_q            = op_i;
    assign op_mod_q        = op_mod_i;
    assign tag_q           = tag_i;
    assign aux_q           = aux_i;
    assign out_valid_input = in_valid_i;
    assign busy_input      = 1'b0;
  end

  // -----------------
  // Input processing
  // -----------------
  fpnew_pkg::fp_info_t [2:0] info_q;

  // Classify input
  fpnew_classifier #(
    .FpFormat    ( FpFormat ),
    .NumOperands ( 3        )
    ) i_class_inputs (
    .operands_i ( operands_q ),
    .is_boxed_i ( is_boxed_q ),
    .info_o     ( info_q     )
  );

  fp_t                 operand_a, operand_b, operand_c;
  fpnew_pkg::fp_info_t info_a,    info_b,    info_c;

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
    operand_a = operands_q[0];
    operand_b = operands_q[1];
    operand_c = operands_q[2];
    info_a    = info_q[0];
    info_b    = info_q[1];
    info_c    = info_q[2];

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
        operand_a  = 'X;
        operand_b  = 'X;
        operand_c  = 'X;
        info_a     = 'X;
        info_b     = 'X;
        info_c     = 'X;
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
        special_status.OF = 1'b1; // overflow
        special_status.NX = 1'b1; // inexact operation
      // Handle cases where the addend is inf
      end else if (info_c.is_inf) begin
        // Result is inifinity with sign of the addend (= operand_c)
        special_result    = '{sign: operand_c.sign, exponent: '1, mantissa: '0};
        special_status.OF = 1'b1; // overflow
        special_status.NX = 1'b1; // inexact operation
      end
    end
  end

  // ---------------------------
  // Initial exponent data path
  // ---------------------------
  logic signed [EXP_WIDTH-1:0] exponent_a, exponent_b, exponent_c;
  logic signed [EXP_WIDTH-1:0] exponent_addend, exponent_product, exponent_difference;
  logic signed [EXP_WIDTH-1:0] tentative_exponent;

  // Zero-extend exponents into signed container - implicit width extension
  assign exponent_a = signed'({1'b0, operand_a.exponent});
  assign exponent_b = signed'({1'b0, operand_b.exponent});
  assign exponent_c = signed'({1'b0, operand_c.exponent});

  // Calculate internal exponents from encoded values. Real exponents are (ex = Ex - bias + 1 - nx)
  // with Ex the encoded exponent and nx the implicit bit. Internal exponents stay biased.
  assign exponent_addend = signed'(exponent_c + $signed({1'b0, ~info_c.is_normal})); // 0 as subnorm
  // Biased product exponent is the sum of encoded exponents minus the bias.
  assign exponent_product = (info_a.is_zero || info_b.is_zero)
                            ? 2 - signed'(BIAS) // in case the product is zero, set minimum exp.
                            : signed'(exponent_a + info_a.is_subnormal
                                      + exponent_b + info_b.is_subnormal
                                      - signed'(BIAS));
  // Exponent difference is the addend exponent minus the product exponent
  assign exponent_difference = exponent_addend - exponent_product;
  // The tentative exponent will be the larger of the product or addend exponent
  assign tentative_exponent = (exponent_difference > 0) ? exponent_addend : exponent_product;

  // Shift amount for addend based on exponents (unsigned as only right shifts)
  logic [SHIFT_AMOUNT_WIDTH-1:0] addend_shamt;

  always_comb begin : addend_shift_amount
    // Product-anchored case, saturated shift (addend is only in the sticky bit)
    if (exponent_difference <= signed'(-2 * PRECISION_BITS - 1))
      addend_shamt = 3 * PRECISION_BITS + 4;
    // Addend and product will have mutual bits to add
    else if (exponent_difference <= signed'(PRECISION_BITS + 2))
      addend_shamt = unsigned'(signed'(PRECISION_BITS) + 3 - exponent_difference);
    // Addend-anchored case, saturated shift (product is only in the sticky bit)
    else
      addend_shamt = 0;
  end

  // ------------------
  // Product data path
  // ------------------
  logic [PRECISION_BITS-1:0]   mantissa_a, mantissa_b, mantissa_c;
  logic [2*PRECISION_BITS-1:0] product;             // the p*p product is 2p bits wide
  logic [3*PRECISION_BITS+3:0] product_shifted;     // addends are 3p+4 bit wide (including G/R)

  // Add implicit bits to mantissae
  assign mantissa_a = {info_a.is_normal, operand_a.mantissa};
  assign mantissa_b = {info_b.is_normal, operand_b.mantissa};
  assign mantissa_c = {info_c.is_normal, operand_c.mantissa};

  // Mantissa multiplier (a*b)
  assign product = mantissa_a * mantissa_b;

  // Product is placed into a 3p+4 bit wide vector, padded with 2 bits for round and sticky:
  // | 000...000 | product | RS |
  //  <-  p+2  -> <-  2p -> < 2>
  assign product_shifted = product << 2; // constant shift

  // -----------------
  // Addend data path
  // -----------------
  logic [3*PRECISION_BITS+3:0] addend_after_shift;  // upper 3p+4 bits are needed to go on
  logic [PRECISION_BITS-1:0]   addend_sticky_bits;  // up to p bit of shifted addend are sticky
  logic                        sticky_before_add;   // they are compressed into a single sticky bit
  logic [3*PRECISION_BITS+3:0] addend_shifted;      // addends are 3p+4 bit wide (including G/R)
  logic                        inject_carry_in;     // inject carry for subtractions if needed

  // In parallel, the addend is right-shifted according to the exponent difference. Up to p bits
  // are shifted out and compressed into a sticky bit.
  // BEFORE THE SHIFT:
  // | mantissa_c | 000..000 |
  //  <-    p   -> <- 3p+4 ->
  // AFTER THE SHIFT:
  // | 000..........000 | mantissa_c | 000...............0GR |  sticky bits  |
  //  <- addend_shamt -> <-    p   -> <- 2p+4-addend_shamt -> <-  up to p  ->
  assign {addend_after_shift, addend_sticky_bits} =
      (mantissa_c << (3 * PRECISION_BITS + 4)) >> addend_shamt;

  assign sticky_before_add     = (| addend_sticky_bits);
  // assign addend_after_shift[0] = sticky_before_add;

  // In case of a subtraction, the addend is inverted
  assign addend_shifted  = (effective_subtraction) ? ~addend_after_shift : addend_after_shift;
  assign inject_carry_in = effective_subtraction & ~sticky_before_add;

  // ---------------
  // Internal pipeline
  // ---------------
  // Pipelined internal signals
  logic                          effective_subtraction_q;
  logic                          tentative_sign_q;
  logic signed [EXP_WIDTH-1:0]   exponent_product_q;
  logic signed [EXP_WIDTH-1:0]   exponent_difference_q;
  logic signed [EXP_WIDTH-1:0]   tentative_exponent_q;
  logic [SHIFT_AMOUNT_WIDTH-1:0] addend_shamt_q;
  logic                          sticky_before_add_q;
  logic [3*PRECISION_BITS+3:0]   product_shifted_q;
  logic [3*PRECISION_BITS+3:0]   addend_shifted_q;
  logic                          inject_carry_in_q;
  fpnew_pkg::roundmode_e         rnd_mode_q2;
  fp_t                           special_result_q;
  fpnew_pkg::status_t            special_status_q;
  logic                          result_is_special_q;
  TagType                        tag_q2;
  AuxType                        aux_q2;
  logic                          out_valid_inside;
  logic                          in_ready_output; // written by output pipeline
  logic                          busy_inside;

  // Generate pipeline between mul and add if needed
  if (PipeConfig==fpnew_pkg::INSIDE || PipeConfig==fpnew_pkg::DISTRIBUTED) begin : inside_pipeline
    localparam NUM_REGS = PipeConfig==fpnew_pkg::DISTRIBUTED
                          ? ((NumPipeRegs + 2) / 3) // First to get regs
                          : NumPipeRegs;
    fpnew_pipe_fma_inside #(
      .ExpWidth    ( EXP_WIDTH      ),
      .PrecBits    ( PRECISION_BITS ),
      .NumPipeRegs ( NUM_REGS       ),
      .FpType      ( fp_t           ),
      .TagType     ( TagType        ),
      .AuxType     ( AuxType        )
    ) i_inside_pipe (
      .clk_i,
      .rst_ni,
      .effective_subtraction_i ( effective_subtraction ),
      .tentative_sign_i        ( tentative_sign        ),
      .exponent_product_i      ( exponent_product      ),
      .exponent_difference_i   ( exponent_difference   ),
      .tentative_exponent_i    ( tentative_exponent    ),
      .addend_shamt_i          ( addend_shamt          ),
      .sticky_before_add_i     ( sticky_before_add     ),
      .product_shifted_i       ( product_shifted       ),
      .addend_shifted_i        ( addend_shifted        ),
      .inject_carry_in_i       ( inject_carry_in       ),
      .rnd_mode_i              ( rnd_mode_q            ),
      .dst_fmt_i               ( fpnew_pkg::FP32       ), // unused
      .result_is_special_i     ( result_is_special     ),
      .special_result_i        ( special_result        ),
      .special_status_i        ( special_status        ),
      .tag_i                   ( tag_q                 ),
      .aux_i                   ( aux_q                 ),
      .in_valid_i              ( out_valid_input       ),
      .in_ready_o              ( in_ready_inside       ),
      .flush_i,
      .effective_subtraction_o ( effective_subtraction_q ),
      .tentative_sign_o        ( tentative_sign_q        ),
      .exponent_product_o      ( exponent_product_q      ),
      .exponent_difference_o   ( exponent_difference_q   ),
      .tentative_exponent_o    ( tentative_exponent_q    ),
      .addend_shamt_o          ( addend_shamt_q          ),
      .sticky_before_add_o     ( sticky_before_add_q     ),
      .product_shifted_o       ( product_shifted_q       ),
      .addend_shifted_o        ( addend_shifted_q        ),
      .inject_carry_in_o       ( inject_carry_in_q       ),
      .rnd_mode_o              ( rnd_mode_q2             ),
      .dst_fmt_o               ( /* unused */            ),
      .result_is_special_o     ( result_is_special_q     ),
      .special_result_o        ( special_result_q        ),
      .special_status_o        ( special_status_q        ),
      .tag_o                   ( tag_q2                  ),
      .aux_o                   ( aux_q2                  ),
      .out_valid_o             ( out_valid_inside        ),
      .out_ready_i             ( in_ready_output         ),
      .busy_o                  ( busy_inside             )
    );
  // Otherwise pass through inputs
  end else begin : no_inside_pipeline
    assign in_ready_inside         = in_ready_output;
    assign effective_subtraction_q = effective_subtraction;
    assign tentative_sign_q        = tentative_sign;
    assign exponent_product_q      = exponent_product;
    assign exponent_difference_q   = exponent_difference;
    assign tentative_exponent_q    = tentative_exponent;
    assign addend_shamt_q          = addend_shamt;
    assign sticky_before_add_q     = sticky_before_add;
    assign product_shifted_q       = product_shifted;
    assign addend_shifted_q        = addend_shifted;
    assign inject_carry_in_q       = inject_carry_in;
    assign rnd_mode_q2             = rnd_mode_q;
    assign result_is_special_q     = result_is_special;
    assign special_result_q        = special_result;
    assign special_status_q        = special_status;
    assign tag_q2                  = tag_q;
    assign aux_q2                  = aux_q;
    assign out_valid_inside        = out_valid_input;
    assign busy_inside             = 1'b0;
  end

  // ------
  // Adder
  // ------
  logic [3*PRECISION_BITS+4:0] sum_raw;   // added one bit for the carry
  logic                        sum_carry; // observe carry bit from sum for sign fixing
  logic [3*PRECISION_BITS+3:0] sum;       // discard carry as sum won't overflow
  logic                        final_sign;

  // Mantissa adder (ab+c). In normal addition, it cannot overflow.
  assign sum_raw   = product_shifted_q + addend_shifted_q + inject_carry_in_q;
  assign sum_carry = sum_raw[3*PRECISION_BITS+4];

  // Complement negative sum (can only happen in subtraction -> overflows for positive results)
  assign sum        = (effective_subtraction_q && ~sum_carry) ? -sum_raw : sum_raw;
  // In case of a mispredicted subtraction result, do a sign flip
  assign final_sign = (effective_subtraction_q && (sum_carry == tentative_sign_q))
                      ? 1'b1
                      : (effective_subtraction_q ? 1'b0 : tentative_sign_q);

  // --------------
  // Normalization
  // --------------
  logic        [LOWER_SUM_WIDTH-1:0]  sum_lower;              // lower 2p+3 bits of sum are searched
  logic        [LZC_RESULT_WIDTH-1:0] leading_zero_count;     // the number of leading zeroes
  logic signed [LZC_RESULT_WIDTH:0]   leading_zero_count_sgn; // signed leading-zero count
  logic                               lzc_zeroes;             // in case only zeroes found

  logic        [SHIFT_AMOUNT_WIDTH-1:0] norm_shamt; // Normalization shift amount
  logic signed [EXP_WIDTH-1:0]          normalized_exponent;

  logic [3*PRECISION_BITS+4:0] sum_shifted;       // result after first normalization shift
  logic [PRECISION_BITS:0]     final_mantissa;    // final mantissa before rounding with round bit
  logic [2*PRECISION_BITS+2:0] sum_sticky_bits;   // remaining 2p+3 sticky bits after normalization
  logic                        sticky_after_norm; // sticky bit after normalization

  logic signed [EXP_WIDTH-1:0] final_exponent;

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

  // Normalization shift amount based on exponents and LZC (unsigned as only left shifts)
  always_comb begin : norm_shift_amount
    // Product-anchored case or cancellations require LZC
    if ((exponent_difference_q <= 0) || (effective_subtraction_q && (exponent_difference_q <= 2))) begin
      // Normal result (biased exponent > 0 and not a zero)
      if ((exponent_product_q - leading_zero_count_sgn + 1 >= 0) && !lzc_zeroes) begin
        // Undo initial product shift, remove the counted zeroes
        norm_shamt          = PRECISION_BITS + 2 + leading_zero_count;
        normalized_exponent = exponent_product_q - leading_zero_count_sgn + 1; // account for shift
      // Subnormal result
      end else begin
        // Cap the shift distance to align mantissa with minimum exponent
        norm_shamt          = unsigned'(signed'(PRECISION_BITS + 2 + exponent_product_q));
        normalized_exponent = 0; // subnormals encoded as 0
      end
    // Addend-anchored case
    end else begin
      norm_shamt          = addend_shamt_q; // Undo the initial shift
      normalized_exponent = tentative_exponent_q;
    end
  end

  // Do the large normalization shift
  assign sum_shifted       = sum << norm_shamt;

  // The addend-anchored case needs a 1-bit normalization since the leading-one can be to the left
  // or right of the (non-carry) MSB of the sum.
  always_comb begin : small_norm
    // Default assignment, discarding carry bit
    {final_mantissa, sum_sticky_bits} = sum_shifted;
    final_exponent                    = normalized_exponent;

    // The normalized sum has overflown, align right and fix exponent
    if (sum_shifted[3*PRECISION_BITS+4]) begin // check the carry bit
      {final_mantissa, sum_sticky_bits} = sum_shifted >> 1;
      final_exponent                    = normalized_exponent + 1;
    // The normalized sum is normal, nothing to do
    end else if (sum_shifted[3*PRECISION_BITS+3]) begin // check the sum MSB
      // do nothing
    // The normalized sum is still denormal, align left - unless the result is not already subnormal
    end else if (normalized_exponent > 1) begin
      {final_mantissa, sum_sticky_bits} = sum_shifted << 1;
      final_exponent                    = normalized_exponent - 1;
    // Otherwise we're denormal
    end else begin
      final_exponent = '0;
    end
  end

  // Update the sticky bit with the shifted-out bits
  assign sticky_after_norm = (| {sum_sticky_bits}) | sticky_before_add_q;

  // ----------------------------
  // Rounding and classification
  // ----------------------------
  logic                         pre_round_sign;
  logic [EXP_BITS-1:0]          pre_round_exponent;
  logic [MAN_BITS-1:0]          pre_round_mantissa;
  logic [EXP_BITS+MAN_BITS-1:0] pre_round_abs; // absolute value of result before rounding
  logic [1:0]                   round_sticky_bits;

  logic of_before_round, of_after_round; // overflow
  logic uf_before_round, uf_after_round; // underflow
  logic result_zero;

  logic                         rounded_sign;
  logic [EXP_BITS+MAN_BITS-1:0] rounded_abs; // absolute value of result after rounding

  // Classification before round. RISC-V mandates checking underflow AFTER rounding!
  assign of_before_round = final_exponent >= 2**(EXP_BITS)-1; // infinity exponent is all ones
  assign uf_before_round = final_exponent == 0;               // exponent for subnormals capped to 0

  // Assemble result before rounding. In case of overflow, the largest normal value is set.
  assign pre_round_sign     = final_sign;
  assign pre_round_exponent = (of_before_round) ? 2**EXP_BITS-2 : final_exponent[EXP_BITS-1:0];
  assign pre_round_mantissa = (of_before_round) ? '1 : final_mantissa[MAN_BITS:1]; // bit 0 is R bit
  assign pre_round_abs      = {pre_round_exponent, pre_round_mantissa};

  // In case of overflow, the round and sticky bits are set for proper rounding
  assign round_sticky_bits  = (of_before_round) ? 2'b11 : {final_mantissa[0], sticky_after_norm};

  // Perform the rounding
  fpnew_rounding #(
    .AbsWidth ( EXP_BITS + MAN_BITS )
  ) i_fpnew_rounding (
    .abs_value_i             ( pre_round_abs           ),
    .sign_i                  ( pre_round_sign          ),
    .round_sticky_bits_i     ( round_sticky_bits       ),
    .rnd_mode_i              ( rnd_mode_q2             ),
    .effective_subtraction_i ( effective_subtraction_q ),
    .abs_rounded_o           ( rounded_abs             ),
    .sign_o                  ( rounded_sign            ),
    .exact_zero_o            ( result_zero             )
  );

  // Classification after rounding
  assign uf_after_round = rounded_abs[EXP_BITS+MAN_BITS-1:MAN_BITS] == '0; // exponent = 0
  assign of_after_round = rounded_abs[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // exponent all ones

  // -----------------
  // Result selection
  // -----------------
  logic [WIDTH-1:0]     regular_result;
  fpnew_pkg::status_t   regular_status;

  // Assemble regular result
  assign regular_result = {rounded_sign, rounded_abs};
  assign regular_status = '{
    NV: 1'b0, // only valid cases are handled in regular path
    DZ: 1'b0, // no divisions
    OF: of_before_round | of_after_round,         // rounding can introduce new overflow
    UF: uf_after_round & ~result_zero,            // true zero results don't count as underflow
    NX: (| round_sticky_bits) | of_before_round | of_after_round // RS bits mean loss in precision
  };

  // Final results for output pipeline
  fp_t                result_d;
  fpnew_pkg::status_t status_d;
  logic               busy_output;

  // Select output depending on special case detection
  assign result_d = result_is_special_q ? special_result_q : regular_result;
  assign status_d = result_is_special_q ? special_status_q : regular_status;

  // ----------------
  // Output Pipeline
  // ----------------
  // Generate pipeline at output if needed
  if (PipeConfig==fpnew_pkg::AFTER || PipeConfig==fpnew_pkg::DISTRIBUTED) begin : output_pipline
    localparam NUM_REGS = PipeConfig==fpnew_pkg::DISTRIBUTED
                          ? ((NumPipeRegs + 1) / 3) // Second to get regs
                          : NumPipeRegs;
    fpnew_pipe_out #(
      .Width       ( WIDTH       ),
      .NumPipeRegs ( NUM_REGS    ),
      .TagType     ( TagType     ),
      .AuxType     ( AuxType     )
    ) i_output_pipe (
      .clk_i,
      .rst_ni,
      .result_i        ( result_d         ),
      .status_i        ( status_d         ),
      .extension_bit_i ( 1'b1             ), // always NaN-Box result
      .class_mask_i    ( fpnew_pkg::QNAN  ), // unused
      .is_class_i      ( 1'b0             ), // unused
      .tag_i           ( tag_q2           ),
      .aux_i           ( aux_q2           ),
      .in_valid_i      ( out_valid_inside ),
      .in_ready_o      ( in_ready_output  ),
      .flush_i,
      .result_o,
      .status_o,
      .extension_bit_o,
      .class_mask_o    ( /* unused */     ),
      .is_class_o      ( /* unused */     ),
      .tag_o,
      .aux_o,
      .out_valid_o,
      .out_ready_i,
      .busy_o          ( busy_output      )
    );
  // Otherwise pass through outputs
  end else begin : no_output_pipeline
    assign in_ready_output = out_ready_i;
    assign result_o        = result_d;
    assign status_o        = status_d;
    assign extension_bit_o = 1'b1; // always NaN-Box result
    assign tag_o           = tag_q2;
    assign aux_o           = aux_q2;
    assign out_valid_o     = out_valid_inside;
  end

  assign busy_o = busy_input | busy_inside | busy_output;

endmodule
