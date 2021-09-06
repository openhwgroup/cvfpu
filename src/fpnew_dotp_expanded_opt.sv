// Copyright 2019-21 ETH Zurich and University of Bologna.
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
// Author: Luca Bertaccini <lbertaccini@iis.ee.ethz.ch>

`include "common_cells/registers.svh"

module fpnew_dotp_expanded_opt #(
  parameter fpnew_pkg::fp_format_e   FpFormat    = fpnew_pkg::fp_format_e'(0),
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,

  localparam int unsigned WIDTH = fpnew_pkg::fp_width(FpFormat), // do not change
  localparam int unsigned DST_WIDTH = 2*WIDTH                    // do not change
) (
  input logic                      clk_i,
  input logic                      rst_ni,
  // Input signals
  input logic [3:0][WIDTH-1:0]     operands_i, // 4 operands
  input logic                      alt_format_i, // TODO (lbertaccini): Use to support ALT destination formats
  input logic [3:0]                is_boxed_i, // 4 operands
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
  output logic [DST_WIDTH-1:0]     result_o,
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
  // Destination parameters
  localparam fpnew_pkg::fp_format_e DstFpFormat    = fpnew_pkg::expanded_format(FpFormat);
  localparam int unsigned DST_EXP_BITS             = fpnew_pkg::exp_bits(DstFpFormat);
  localparam int unsigned DST_MAN_BITS             = fpnew_pkg::man_bits(DstFpFormat);
  localparam int unsigned DST_BIAS                 = fpnew_pkg::bias(DstFpFormat);
  localparam fpnew_pkg::fp_format_e DstFpFormatAlt = fpnew_pkg::expanded_alt_format(FpFormat); // TODO (lbertaccini): Use to support ALT destination formats
  localparam int unsigned DST_EXP_BITS_ALT         = fpnew_pkg::exp_bits(DstFpFormatAlt); // TODO (lbertaccini): Use to support ALT destination formats
  localparam int unsigned DST_MAN_BITS_ALT         = fpnew_pkg::man_bits(DstFpFormatAlt); // TODO (lbertaccini): Use to support ALT destination formats
  localparam int unsigned DST_BIAS_ALT             = fpnew_pkg::bias(DstFpFormatAlt); // TODO (lbertaccini): Use to support ALT destination formats
  // Precision bits 'p' include the implicit bit
  localparam int unsigned PRECISION_BITS = MAN_BITS + 1;
  localparam int unsigned DST_PRECISION_BITS = DST_MAN_BITS + 1;
  localparam int unsigned ADDITIONAL_PRECISION_BITS = DST_PRECISION_BITS - 2 * PRECISION_BITS;
  // The lower 2p+3 bits of the internal DOTP result will be needed for leading-zero detection
  localparam int unsigned LOWER_SUM_WIDTH  = DST_PRECISION_BITS + 3; // TODO (lbertaccini): Check this for denormal support (3p+3)?
  localparam int unsigned LZC_RESULT_WIDTH = $clog2(LOWER_SUM_WIDTH);
  // Internal exponent width of DOTP must accomodate all meaningful exponent values in order to avoid
  // datapath leakage. This is either given by the exponent bits or the width of the LZC result.
  // In most reasonable FP formats the internal exponent will be wider than the LZC result.
  localparam int unsigned EXP_WIDTH = unsigned'(fpnew_pkg::maximum(EXP_BITS + 2, LZC_RESULT_WIDTH));
  localparam int unsigned DST_EXP_WIDTH = unsigned'(fpnew_pkg::maximum(DST_EXP_BITS + 2, LZC_RESULT_WIDTH));
  // Shift amount width: maximum internal mantissa size is 3p+3 bits
  localparam int unsigned SHIFT_AMOUNT_WIDTH = $clog2(3 * PRECISION_BITS + 3 + ADDITIONAL_PRECISION_BITS);
  // Pipelines
  localparam NUM_INP_REGS = PipeConfig == fpnew_pkg::BEFORE
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? ((NumPipeRegs + 1) / 3) // Second to get distributed regs
                               : 0); // no regs here otherwise
  localparam NUM_MID_REGS = PipeConfig == fpnew_pkg::INSIDE
                          ? NumPipeRegs
                          : (PipeConfig == fpnew_pkg::DISTRIBUTED
                             ? ((NumPipeRegs + 2) / 3) // First to get distributed regs
                             : 0); // no regs here otherwise
  localparam NUM_OUT_REGS = PipeConfig == fpnew_pkg::AFTER
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? (NumPipeRegs / 3) // Last to get distributed regs
                               : 0); // no regs here otherwise

  // ----------------
  // Type definition
  // ----------------
  typedef struct packed {
    logic                sign;
    logic [EXP_BITS-1:0] exponent;
    logic [MAN_BITS-1:0] mantissa;
  } fp_src_t;
  typedef struct packed {
    logic                    sign;
    logic [DST_EXP_BITS-1:0] exponent;
    logic [DST_MAN_BITS-1:0] mantissa;
  } fp_dst_t;
  typedef struct packed {// TODO (lbertaccini): Use to support ALT destination formats
    logic                    sign;// TODO (lbertaccini): Use to support ALT destination formats
    logic [DST_EXP_BITS_ALT-1:0] exponent;// TODO (lbertaccini): Use to support ALT destination formats
    logic [DST_MAN_BITS_ALT-1:0] mantissa;// TODO (lbertaccini): Use to support ALT destination formats
  } fp_dst_alt_t; // TODO (lbertaccini): Use to support ALT destination formats

  // ---------------
  // Input pipeline
  // ---------------
  // Input pipeline signals, index i holds signal after i register stages
  logic                  [0:NUM_INP_REGS][3:0][WIDTH-1:0] inp_pipe_operands_q;
  logic                  [0:NUM_INP_REGS][3:0]            inp_pipe_is_boxed_q;
  fpnew_pkg::roundmode_e [0:NUM_INP_REGS]                 inp_pipe_rnd_mode_q;
  fpnew_pkg::operation_e [0:NUM_INP_REGS]                 inp_pipe_op_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_op_mod_q;
  TagType                [0:NUM_INP_REGS]                 inp_pipe_tag_q;
  AuxType                [0:NUM_INP_REGS]                 inp_pipe_aux_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_valid_q;
  // Ready signal is combinatorial for all stages
  logic [0:NUM_INP_REGS] inp_pipe_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign inp_pipe_operands_q[0] = operands_i;
  assign inp_pipe_is_boxed_q[0] = is_boxed_i;
  assign inp_pipe_rnd_mode_q[0] = rnd_mode_i;
  assign inp_pipe_op_q[0]       = op_i;
  assign inp_pipe_op_mod_q[0]   = op_mod_i;
  assign inp_pipe_tag_q[0]      = tag_i;
  assign inp_pipe_aux_q[0]      = aux_i;
  assign inp_pipe_valid_q[0]    = in_valid_i;
  // Input stage: Propagate pipeline ready signal to updtream circuitry
  assign in_ready_o = inp_pipe_ready[0];
  // Generate the register stages
  for (genvar i = 0; i < NUM_INP_REGS; i++) begin : gen_input_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign inp_pipe_ready[i] = inp_pipe_ready[i+1] | ~inp_pipe_valid_q[i+1];
    // Valid: enabled by ready signal, synchronous clear with the flush signal
    `FFLARNC(inp_pipe_valid_q[i+1], inp_pipe_valid_q[i], inp_pipe_ready[i], flush_i, 1'b0, clk_i, rst_ni)
    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = inp_pipe_ready[i] & inp_pipe_valid_q[i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(inp_pipe_operands_q[i+1], inp_pipe_operands_q[i], reg_ena, '0)
    `FFL(inp_pipe_is_boxed_q[i+1], inp_pipe_is_boxed_q[i], reg_ena, '0)
    `FFL(inp_pipe_rnd_mode_q[i+1], inp_pipe_rnd_mode_q[i], reg_ena, fpnew_pkg::RNE)
    `FFL(inp_pipe_op_q[i+1],       inp_pipe_op_q[i],       reg_ena, fpnew_pkg::FMADD)
    `FFL(inp_pipe_op_mod_q[i+1],   inp_pipe_op_mod_q[i],   reg_ena, '0)
    `FFL(inp_pipe_tag_q[i+1],      inp_pipe_tag_q[i],      reg_ena, TagType'('0))
    `FFL(inp_pipe_aux_q[i+1],      inp_pipe_aux_q[i],      reg_ena, AuxType'('0))
  end

  // -----------------
  // Input processing
  // -----------------
  fpnew_pkg::fp_info_t [3:0] info_q;

  // Classify input
  fpnew_classifier #(
    .FpFormat    ( FpFormat ),
    .NumOperands ( 4        )
    ) i_class_inputs (
    .operands_i ( inp_pipe_operands_q[NUM_INP_REGS] ),
    .is_boxed_i ( inp_pipe_is_boxed_q[NUM_INP_REGS] ),
    .info_o     ( info_q                            )
  );

  fp_src_t             operand_a, operand_b, operand_c, operand_d;
  fpnew_pkg::fp_info_t info_a,    info_b,    info_c,    info_d;

  // Operation selection and operand adjustment
  // | \c op_q  | \c op_mod_q | Operation Adjustment
  // |:--------:|:-----------:|---------------------
  // | DOTP     | \c 0        | DOTP: none
  // | DOTP     | \c 1        | DOTPN: Invert sign of the second product
  // | *others* | \c -        | *invalid*
  // \note \c op_mod_q always inverts the sign of the addend.
  always_comb begin : op_select

    // Default assignments - packing-order-agnostic
    operand_a = inp_pipe_operands_q[NUM_INP_REGS][0];
    operand_b = inp_pipe_operands_q[NUM_INP_REGS][1];
    operand_c = inp_pipe_operands_q[NUM_INP_REGS][2];
    operand_d = inp_pipe_operands_q[NUM_INP_REGS][3];
    info_a    = info_q[0];
    info_b    = info_q[1];
    info_c    = info_q[2];
    info_d    = info_q[3];

    // op_mod_q inverts sign of operand C and thus inverts the sign of the second product
    operand_c.sign = operand_c.sign ^ inp_pipe_op_mod_q[NUM_INP_REGS];

    // TODO(lbertaccini): Add pre-processing for different operations (DOTP, INNER_SUM)
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
  assign any_operand_inf = (| {info_a.is_inf,        info_b.is_inf,        info_c.is_inf,         info_d.is_inf});
  assign any_operand_nan = (| {info_a.is_nan,        info_b.is_nan,        info_c.is_nan,         info_d.is_nan});
  assign signalling_nan  = (| {info_a.is_signalling, info_b.is_signalling, info_c.is_signalling, info_d.is_signalling});
  // Effective subtraction in DOTP occurs when the signs of the two products differ
  assign effective_subtraction = (operand_a.sign ^ operand_b.sign) ^ (operand_c.sign ^ operand_d.sign);

  // ----------------------
  // Special case handling
  // ----------------------
  fp_dst_t            special_result;
  fpnew_pkg::status_t special_status;
  logic               result_is_special;

  always_comb begin : special_cases
    // Default assignments
    special_result    = '{sign: 1'b0, exponent: '1, mantissa: 2**(MAN_BITS-1)}; // canonical qNaN
    special_status    = '0;
    result_is_special = 1'b0;

    // Handle potentially mixed nan & infinity input => important for the case where infinity and
    // zero are multiplied and added to a qNaN.
    // RISC-V mandates raising the NV exception in these cases:
    // (inf * 0) + c or (0 * inf) + c INVALID, no matter c (even quiet NaNs)
    if (  ((info_a.is_inf && info_b.is_zero) || (info_a.is_zero && info_b.is_inf))
       || ((info_c.is_inf && info_d.is_zero) || (info_c.is_zero && info_d.is_inf)) ) begin
      result_is_special = 1'b1; // bypass DOTP, output is the canonical qNaN
      special_status.NV = 1'b1; // invalid operation
    // NaN Inputs cause canonical quiet NaN at the output and maybe invalid OP
    end else if (any_operand_nan) begin
      result_is_special = 1'b1;           // bypass DOTP, output is the canonical qNaN
      special_status.NV = signalling_nan; // raise the invalid operation flag if signalling
    // Special cases involving infinity
    end else if (any_operand_inf) begin
      result_is_special = 1'b1; // bypass DOTP
      // Effective addition of opposite infinities (±inf - ±inf) is invalid!
      if ((info_a.is_inf || info_b.is_inf) && (info_c.is_inf || info_d.is_inf) && effective_subtraction)
        special_status.NV = 1'b1; // invalid operation
      // Handle cases where output will be inf because of inf product input
      else if (info_a.is_inf || info_b.is_inf) begin
        // Result is infinity with the sign of the first product
        special_result    = '{sign: operand_a.sign ^ operand_b.sign, exponent: '1, mantissa: '0};
      // Handle cases where the second product is inf
      end else if (info_c.is_inf || info_d.is_inf) begin
        // Result is inifinity with sign of the second product
        special_result    = '{sign: operand_c.sign ^ operand_d.sign, exponent: '1, mantissa: '0};
      end
    end
  end

  // ---------------------------
  // Initial exponent data path
  // ---------------------------
  logic signed [EXP_WIDTH-1:0] exponent_a, exponent_b, exponent_c, exponent_d;
  logic signed [DST_EXP_WIDTH-1:0] exponent_product_x, exponent_product_y, exponent_difference;
  logic signed [DST_EXP_WIDTH-1:0] exponent_product_min, exponent_product_max;
  logic signed [DST_EXP_WIDTH-1:0] tentative_exponent;
  logic                            max_exponent;

  // Zero-extend exponents into signed container - implicit width extension
  assign exponent_a = signed'({1'b0, operand_a.exponent});
  assign exponent_b = signed'({1'b0, operand_b.exponent});
  assign exponent_c = signed'({1'b0, operand_c.exponent});
  assign exponent_d = signed'({1'b0, operand_d.exponent});

  // Calculate internal exponents from encoded values. Real exponents are (ex = Ex - bias + 1 - nx)
  // with Ex the encoded exponent and nx the implicit bit. Internal exponents stay biased.
  // Biased product exponent is the sum of encoded exponents minus the bias.
  assign exponent_product_y = (info_c.is_zero || info_d.is_zero)
                              ? 2 - signed'(fpnew_pkg::bias(DstFpFormat)) // in case the product is zero, set minimum exp.
                              : signed'(exponent_c + info_c.is_subnormal
                                        + exponent_d + info_d.is_subnormal
                                        - 2*signed'(fpnew_pkg::bias(FpFormat))
                                        + signed'(fpnew_pkg::bias(DstFpFormat))); // rebias for dst fmt
  assign exponent_product_x = (info_a.is_zero || info_b.is_zero)
                              ? 2 - signed'(fpnew_pkg::bias(DstFpFormat)) // in case the product is zero, set minimum exp.
                              : signed'(exponent_a + info_a.is_subnormal
                                        + exponent_b + info_b.is_subnormal
                                        - 2*signed'(fpnew_pkg::bias(FpFormat))
                                        + signed'(fpnew_pkg::bias(DstFpFormat))); // rebias for dst fmt
  // Find maximum exponent, the minimum will be shifted for the addition
  assign max_exponent = (exponent_product_y >= exponent_product_x) ? 1'b1 : 1'b0;
  // The tentative sign of the DOTP shall be the sign of the first product
  assign tentative_sign = (max_exponent) ? (operand_c.sign ^ operand_d.sign) : operand_a.sign ^ operand_b.sign;
  // Exponent difference is the product_y exponent minus the product_x exponent
  assign exponent_difference = (max_exponent) ? exponent_product_y - exponent_product_x
                                              : exponent_product_x - exponent_product_y;
  assign exponent_product_max = (max_exponent) ? exponent_product_y : exponent_product_x;
  assign exponent_product_min = (max_exponent) ? exponent_product_x : exponent_product_y;
  // The tentative exponent will be the larger of the product_x or the product_y exponent
  assign tentative_exponent = (max_exponent) ? exponent_product_y : exponent_product_x;

  // Shift amount for product_y based on exponents (unsigned as only right shifts)
  logic [SHIFT_AMOUNT_WIDTH-1:0] addend_shamt;
  always_comb begin : addend_shift_amount
    // product_y and product_x will have mutual bits to add
    if (exponent_difference <= signed'(DST_PRECISION_BITS + 3)) begin
      addend_shamt = unsigned'(signed'(exponent_difference)); // TODO(lbertaccini): is it still +3 or +2?
    // Addend-anchored case, saturated shift (product is only in the sticky bit)
    end else begin
      addend_shamt = DST_PRECISION_BITS + 3;
    end
  end

  // ------------------
  // Product data path
  // ------------------
  logic [PRECISION_BITS-1:0]   mantissa_a, mantissa_b, mantissa_c, mantissa_d;
  logic [2*PRECISION_BITS-1:0] product_x, product_y;  // the p*p product is 2p bits wide
  logic [2*PRECISION_BITS-1:0] product_max, product_min;  // the p*p product is 2p bits wide

  // Add implicit bits to mantissae
  assign mantissa_a = {info_a.is_normal, operand_a.mantissa};
  assign mantissa_b = {info_b.is_normal, operand_b.mantissa};
  assign mantissa_c = {info_c.is_normal, operand_c.mantissa};
  assign mantissa_d = {info_d.is_normal, operand_d.mantissa};

  // Mantissa multiplier (a*b)
  assign product_x = mantissa_a * mantissa_b;
  // Mantissa multiplier (c*d)
  assign product_y = mantissa_c * mantissa_d;

  // ------------------
  // Shift data path
  // ------------------
  logic [DST_PRECISION_BITS+2:0] product_max_shifted;
  logic [DST_PRECISION_BITS+2:0] product_min_after_shift;
  logic [2*PRECISION_BITS-1:0]   addend_sticky_bits;  // up to p bit of shifted addend are sticky
  logic                          sticky_before_add;   // they are compressed into a single sticky bit
  logic [DST_PRECISION_BITS+2:0] product_min_shifted;
  logic                          inject_carry_in;     // inject carry for subtractions if needed
  // Place larger value in product_max and the smaller in product_min
  assign product_max = (max_exponent) ? product_y : product_x;
  assign product_min = (max_exponent) ? product_x : product_y;

  // Product max is placed into a 2p+3 bit wide vector, padded with 3 bits for rounding purposes:
  // | product_max  |  rnd  |
  //  <-  2p_dst  -> <  3   >
  assign product_max_shifted = product_max << (3 + ADDITIONAL_PRECISION_BITS); // constant shift

  // In parallel, the min product is right-shifted according to the exponent difference. Up to p bits
  // are shifted out and compressed into a sticky bit.
  // | product_min   |  rnd   | sticky_bits |
  //  <-   2p_dst  -> <  3   > <     p      >
  assign {product_min_after_shift, addend_sticky_bits} =
      (product_min << (2*PRECISION_BITS + 3 + ADDITIONAL_PRECISION_BITS)) >> addend_shamt;

  assign sticky_before_add     = (| addend_sticky_bits);

  // In case of a subtraction, the addend is inverted
  assign product_min_shifted  = (effective_subtraction) ? ~product_min_after_shift : product_min_after_shift;
  assign inject_carry_in = effective_subtraction & ~sticky_before_add;

  // ------
  // Adder
  // ------
  logic [DST_PRECISION_BITS+3:0] sum_raw;   // added one bit for the carry
  logic                          sum_carry; // observe carry bit from sum for sign fixing
  logic [DST_PRECISION_BITS+2:0] sum;       // discard carry as sum won't overflow
  logic                          final_sign;

  //Mantissa adder (ab+c). In normal addition, it cannot overflow.
  assign sum_raw = product_max_shifted + product_min_shifted + inject_carry_in;
  assign sum_carry = sum_raw[DST_PRECISION_BITS+3];

  // Complement negative sum (can only happen in subtraction -> overflows for positive results)
  assign sum        = (effective_subtraction && ~sum_carry) ? -sum_raw : sum_raw;

  // In case of a mispredicted subtraction result, do a sign flip
  assign final_sign = (effective_subtraction && (sum_carry == tentative_sign))
                      ? 1'b1
                      : (effective_subtraction ? 1'b0 : tentative_sign);

  // ---------------
  // Internal pipeline
  // ---------------
  // Pipeline output signals as non-arrays
  logic                            effective_subtraction_q;
  logic signed [DST_EXP_WIDTH-1:0] exponent_product_min_q;
  logic signed [DST_EXP_WIDTH-1:0] exponent_difference_q;
  logic signed [DST_EXP_WIDTH-1:0] tentative_exponent_q;
  logic [SHIFT_AMOUNT_WIDTH-1:0]   addend_shamt_q;
  logic                            sticky_before_add_q;
  logic [DST_PRECISION_BITS+2:0]   sum_q;
  logic                            final_sign_q;
  fpnew_pkg::roundmode_e           rnd_mode_q;
  logic                            result_is_special_q;
  fp_dst_t                         special_result_q;
  fpnew_pkg::status_t              special_status_q;
  logic                            sum_carry_q;
  // Internal pipeline signals, index i holds signal after i register stages
  logic                  [0:NUM_MID_REGS]                         mid_pipe_eff_sub_q;
  logic signed           [0:NUM_MID_REGS][DST_EXP_WIDTH-1:0]      mid_pipe_exp_min_q;
  logic signed           [0:NUM_MID_REGS][DST_EXP_WIDTH-1:0]      mid_pipe_exp_diff_q;
  logic signed           [0:NUM_MID_REGS][DST_EXP_WIDTH-1:0]      mid_pipe_tent_exp_q;
  logic                  [0:NUM_MID_REGS][SHIFT_AMOUNT_WIDTH-1:0] mid_pipe_add_shamt_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_sticky_q;
  logic                  [0:NUM_MID_REGS][DST_PRECISION_BITS+2:0] mid_pipe_sum_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_final_sign_q;
  fpnew_pkg::roundmode_e [0:NUM_MID_REGS]                         mid_pipe_rnd_mode_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_res_is_spec_q;
  fp_dst_t               [0:NUM_MID_REGS]                         mid_pipe_spec_res_q;
  fpnew_pkg::status_t    [0:NUM_MID_REGS]                         mid_pipe_spec_stat_q;
  TagType                [0:NUM_MID_REGS]                         mid_pipe_tag_q;
  AuxType                [0:NUM_MID_REGS]                         mid_pipe_aux_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_valid_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_sum_carry_q;
  // Ready signal is combinatorial for all stages
  logic [0:NUM_MID_REGS] mid_pipe_ready;

  // Input stage: First element of pipeline is taken from upstream logic
  assign mid_pipe_eff_sub_q[0]     = effective_subtraction;
  assign mid_pipe_exp_min_q[0]     = exponent_product_min;
  assign mid_pipe_exp_diff_q[0]    = exponent_difference;
  assign mid_pipe_tent_exp_q[0]    = tentative_exponent;
  assign mid_pipe_add_shamt_q[0]   = addend_shamt;
  assign mid_pipe_sticky_q[0]      = sticky_before_add;
  assign mid_pipe_sum_q[0]         = sum;
  assign mid_pipe_final_sign_q[0]  = final_sign;
  assign mid_pipe_rnd_mode_q[0]    = inp_pipe_rnd_mode_q[NUM_INP_REGS];
  assign mid_pipe_res_is_spec_q[0] = result_is_special;
  assign mid_pipe_spec_res_q[0]    = special_result;
  assign mid_pipe_spec_stat_q[0]   = special_status;
  assign mid_pipe_tag_q[0]         = inp_pipe_tag_q[NUM_INP_REGS];
  assign mid_pipe_aux_q[0]         = inp_pipe_aux_q[NUM_INP_REGS];
  assign mid_pipe_valid_q[0]       = inp_pipe_valid_q[NUM_INP_REGS];
  assign mid_pipe_sum_carry_q[0]   = sum_carry;
  // Input stage: Propagate pipeline ready signal to input pipe
  assign inp_pipe_ready[NUM_INP_REGS] = mid_pipe_ready[0];

  // Generate the register stages
  for (genvar i = 0; i < NUM_MID_REGS; i++) begin : gen_inside_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign mid_pipe_ready[i] = mid_pipe_ready[i+1] | ~mid_pipe_valid_q[i+1];
    // Valid: enabled by ready signal, synchronous clear with the flush signal
    `FFLARNC(mid_pipe_valid_q[i+1], mid_pipe_valid_q[i], mid_pipe_ready[i], flush_i, 1'b0, clk_i, rst_ni)
    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = mid_pipe_ready[i] & mid_pipe_valid_q[i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(mid_pipe_eff_sub_q[i+1],     mid_pipe_eff_sub_q[i],     reg_ena, '0)
    `FFL(mid_pipe_exp_min_q[i+1],     mid_pipe_exp_min_q[i],     reg_ena, '0)
    `FFL(mid_pipe_exp_diff_q[i+1],    mid_pipe_exp_diff_q[i],    reg_ena, '0)
    `FFL(mid_pipe_tent_exp_q[i+1],    mid_pipe_tent_exp_q[i],    reg_ena, '0)
    `FFL(mid_pipe_add_shamt_q[i+1],   mid_pipe_add_shamt_q[i],   reg_ena, '0)
    `FFL(mid_pipe_sticky_q[i+1],      mid_pipe_sticky_q[i],      reg_ena, '0)
    `FFL(mid_pipe_sum_q[i+1],         mid_pipe_sum_q[i],         reg_ena, '0)
    `FFL(mid_pipe_final_sign_q[i+1],  mid_pipe_final_sign_q[i],  reg_ena, '0)
    `FFL(mid_pipe_rnd_mode_q[i+1],    mid_pipe_rnd_mode_q[i],    reg_ena, fpnew_pkg::RNE)
    `FFL(mid_pipe_res_is_spec_q[i+1], mid_pipe_res_is_spec_q[i], reg_ena, '0)
    `FFL(mid_pipe_spec_res_q[i+1],    mid_pipe_spec_res_q[i],    reg_ena, '0)
    `FFL(mid_pipe_spec_stat_q[i+1],   mid_pipe_spec_stat_q[i],   reg_ena, '0)
    `FFL(mid_pipe_tag_q[i+1],         mid_pipe_tag_q[i],         reg_ena, TagType'('0))
    `FFL(mid_pipe_aux_q[i+1],         mid_pipe_aux_q[i],         reg_ena, AuxType'('0))
    `FFL(mid_pipe_sum_carry_q[i+1],   mid_pipe_sum_carry_q[i],   reg_ena, '0)
  end
  // Output stage: assign selected pipe outputs to signals for later use
  assign effective_subtraction_q = mid_pipe_eff_sub_q[NUM_MID_REGS];
  assign exponent_product_min_q  = mid_pipe_exp_min_q[NUM_MID_REGS];
  assign exponent_difference_q   = mid_pipe_exp_diff_q[NUM_MID_REGS];
  assign tentative_exponent_q    = mid_pipe_tent_exp_q[NUM_MID_REGS];
  assign addend_shamt_q          = mid_pipe_add_shamt_q[NUM_MID_REGS];
  assign sticky_before_add_q     = mid_pipe_sticky_q[NUM_MID_REGS];
  assign sum_q                   = mid_pipe_sum_q[NUM_MID_REGS];
  assign final_sign_q            = mid_pipe_final_sign_q[NUM_MID_REGS];
  assign rnd_mode_q              = mid_pipe_rnd_mode_q[NUM_MID_REGS];
  assign result_is_special_q     = mid_pipe_res_is_spec_q[NUM_MID_REGS];
  assign special_result_q        = mid_pipe_spec_res_q[NUM_MID_REGS];
  assign special_status_q        = mid_pipe_spec_stat_q[NUM_MID_REGS];
  assign sum_carry_q             = mid_pipe_sum_carry_q[NUM_MID_REGS];

  // --------------
  // Normalization
  // --------------
  logic        [LOWER_SUM_WIDTH-1:0]  sum_lower;              // lower 2p+3 bits of sum are searched
  logic        [LZC_RESULT_WIDTH-1:0] leading_zero_count;     // the number of leading zeroes
  logic signed [LZC_RESULT_WIDTH:0]   leading_zero_count_sgn; // signed leading-zero count
  logic                               lzc_zeroes;             // in case only zeroes found

  logic        [SHIFT_AMOUNT_WIDTH-1:0] norm_shamt; // Normalization shift amount
  logic signed [DST_EXP_WIDTH-1:0]      normalized_exponent;

  logic [DST_PRECISION_BITS+3:0] sum_shifted;       // result after first normalization shift
  logic [DST_PRECISION_BITS:0]   final_mantissa;    // final mantissa before rounding with round bit
  logic [1:0]                    sum_sticky_bits;   // remaining 3p+3 sticky bits after normalization
  logic                          sticky_after_norm; // sticky bit after normalization
  logic                          effective_carry_sum;

  assign effective_carry_sum = ~effective_subtraction_q && sum_carry_q;

  logic signed [DST_EXP_WIDTH-1:0] final_exponent;

  assign sum_lower = sum_q[LOWER_SUM_WIDTH-1:0];

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
    // // Product-anchored case or cancellations require LZC
    // if (effective_subtraction_q && (exponent_difference_q <= 2)) begin
    //   // Normal result (biased exponent > 0 and not a zero)
    //   if ((exponent_product_min_q - leading_zero_count_sgn + 1 >= 0) && !lzc_zeroes) begin
    //     // Undo initial product shift, remove the counted zeroes
    //     norm_shamt          = leading_zero_count + 2;
    //     normalized_exponent = exponent_product_min_q - leading_zero_count_sgn + 1; // account for shift
    //   // Subnormal result
    //   end else begin
    //     // Cap the shift distance to align mantissa with minimum exponent
    //     norm_shamt          = '0;
    //     normalized_exponent = -2; // subnormals encoded as 0
    //   end
    // // Addend-anchored case
    // end else begin
      // TODO (lbertaccini): Check here
    if (effective_carry_sum) begin
      norm_shamt          = 0;
      normalized_exponent = tentative_exponent_q + 1;
    end else begin
      norm_shamt          = leading_zero_count;
      normalized_exponent = tentative_exponent_q - leading_zero_count_sgn + 1;
    end
    // end
  end

  // Do the large normalization shift
  logic carry_shift;
  always_comb begin : sum_shift
    {carry_shift, sum_shifted}       = sum_q << (norm_shamt);
    if (carry_shift)
      sum_shifted = {carry_shift, sum_shifted[DST_PRECISION_BITS + 3:1]};
  end

  // The addend-anchored case needs a 1-bit normalization since the leading-one can be to the left
  // or right of the (non-carry) MSB of the sum.
  always_comb begin : small_norm

    // Default assignment, discarding carry bit
    {final_mantissa, sum_sticky_bits} = sum_shifted;
    final_exponent                    = normalized_exponent + carry_shift;

    // The normalized sum has overflown, align right and fix exponent
    if (effective_carry_sum) begin
      {final_mantissa, sum_sticky_bits} = sum_shifted >> 1;
      final_exponent                    = normalized_exponent + 1 + carry_shift;
    end else if (normalized_exponent > -1) begin
      if (sum_shifted[DST_PRECISION_BITS + 3]) begin // check the carry bit
        {final_mantissa, sum_sticky_bits} = sum_shifted >> 1;
        final_exponent                    = normalized_exponent + 1 + carry_shift;
      // The normalized sum is normal, nothing to do
      end else if ((sum_shifted[DST_PRECISION_BITS + 2]) && (normalized_exponent > 0)) begin // check the sum MSB
        // do nothing
      // The normalized sum is still denormal, align left - unless the result is not already subnormal
      end else if (normalized_exponent > 1) begin
        {final_mantissa, sum_sticky_bits} = sum_shifted << 1;
        final_exponent                    = normalized_exponent - 1;
      end else begin
        final_mantissa = '0; // flush denormals to zero
        final_exponent = '0;
      end
      // Otherwise we're denormal
    end else begin
      if ((sum_shifted[DST_PRECISION_BITS + 3]) && (normalized_exponent == -1) && carry_shift) begin // check the carry bit
        {final_mantissa, sum_sticky_bits} = sum_shifted >> 1;
        final_exponent                    = normalized_exponent + 1 + carry_shift;
      end else begin
        final_mantissa = '0; // flush denormals to zero
        final_exponent = '0;
      end
    end
  end

  // Update the sticky bit with the shifted-out bits
  assign sticky_after_norm = (| {sum_sticky_bits}) | sticky_before_add_q;

  // ----------------------------
  // Rounding and classification
  // ----------------------------
  logic                                 pre_round_sign;
  logic [DST_EXP_BITS-1:0]              pre_round_exponent;
  logic [DST_MAN_BITS-1:0]              pre_round_mantissa;
  logic [DST_EXP_BITS+DST_MAN_BITS-1:0] pre_round_abs; // absolute value of result before rounding
  logic [1:0]                           round_sticky_bits;

  logic of_before_round, of_after_round; // overflow
  logic uf_before_round, uf_after_round; // underflow
  logic result_zero;

  logic                                 rounded_sign;
  logic [DST_EXP_BITS+DST_MAN_BITS-1:0] rounded_abs; // absolute value of result after rounding

  // Classification before round. RISC-V mandates checking underflow AFTER rounding!
  assign of_before_round = final_exponent >= 2**(DST_EXP_BITS)-1; // infinity exponent is all ones
  assign uf_before_round = final_exponent == 0;               // exponent for subnormals capped to 0

  // Assemble result before rounding. In case of overflow, the largest normal value is set.
  assign pre_round_sign     = final_sign_q;
  assign pre_round_exponent = (of_before_round) ? 2**DST_EXP_BITS-2 : unsigned'(final_exponent[DST_EXP_BITS-1:0]);
  assign pre_round_mantissa = (of_before_round) ? '1 : final_mantissa[DST_MAN_BITS:1]; // bit 0 is R bit
  assign pre_round_abs      = {pre_round_exponent, pre_round_mantissa};

  // In case of overflow, the round and sticky bits are set for proper rounding
  assign round_sticky_bits  = (of_before_round) ? 2'b11 : {final_mantissa[0], sticky_after_norm};

  // Perform the rounding
  fpnew_rounding #(
    .AbsWidth ( DST_EXP_BITS + DST_MAN_BITS )
  ) i_fpnew_rounding (
    .abs_value_i             ( pre_round_abs           ),
    .sign_i                  ( pre_round_sign          ),
    .round_sticky_bits_i     ( round_sticky_bits       ),
    .rnd_mode_i              ( rnd_mode_q              ),
    .effective_subtraction_i ( effective_subtraction_q ),
    .abs_rounded_o           ( rounded_abs             ),
    .sign_o                  ( rounded_sign            ),
    .exact_zero_o            ( result_zero             )
  );

  // Classification after rounding
  assign uf_after_round = rounded_abs[DST_EXP_BITS+DST_MAN_BITS-1:DST_MAN_BITS] == '0; // exponent = 0
  assign of_after_round = rounded_abs[DST_EXP_BITS+DST_MAN_BITS-1:DST_MAN_BITS] == '1; // exponent all ones

  // -----------------
  // Result selection
  // -----------------
  logic [DST_WIDTH-1:0] regular_result;
  fpnew_pkg::status_t   regular_status;

  // Assemble regular result
  assign regular_result    = {rounded_sign, rounded_abs};
  assign regular_status.NV = 1'b0; // only valid cases are handled in regular path
  assign regular_status.DZ = 1'b0; // no divisions
  assign regular_status.OF = of_before_round | of_after_round;   // rounding can introduce overflow
  assign regular_status.UF = uf_after_round & regular_status.NX; // only inexact results raise UF
  assign regular_status.NX = (| round_sticky_bits) | of_before_round | of_after_round;

  // Final results for output pipeline
  fp_dst_t            result_d;
  fpnew_pkg::status_t status_d;

  // Select output depending on special case detection
  assign result_d = result_is_special_q ? special_result_q : regular_result;
  assign status_d = result_is_special_q ? special_status_q : regular_status;

  // ----------------
  // Output Pipeline
  // ----------------
  // Output pipeline signals, index i holds signal after i register stages
  fp_dst_t            [0:NUM_OUT_REGS] out_pipe_result_q;
  fpnew_pkg::status_t [0:NUM_OUT_REGS] out_pipe_status_q;
  TagType             [0:NUM_OUT_REGS] out_pipe_tag_q;
  AuxType             [0:NUM_OUT_REGS] out_pipe_aux_q;
  logic               [0:NUM_OUT_REGS] out_pipe_valid_q;
  // Ready signal is combinatorial for all stages
  logic [0:NUM_OUT_REGS] out_pipe_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign out_pipe_result_q[0] = result_d;
  assign out_pipe_status_q[0] = status_d;
  assign out_pipe_tag_q[0]    = mid_pipe_tag_q[NUM_MID_REGS];
  assign out_pipe_aux_q[0]    = mid_pipe_aux_q[NUM_MID_REGS];
  assign out_pipe_valid_q[0]  = mid_pipe_valid_q[NUM_MID_REGS];
  // Input stage: Propagate pipeline ready signal to inside pipe
  assign mid_pipe_ready[NUM_MID_REGS] = out_pipe_ready[0];
  // Generate the register stages
  for (genvar i = 0; i < NUM_OUT_REGS; i++) begin : gen_output_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign out_pipe_ready[i] = out_pipe_ready[i+1] | ~out_pipe_valid_q[i+1];
    // Valid: enabled by ready signal, synchronous clear with the flush signal
    `FFLARNC(out_pipe_valid_q[i+1], out_pipe_valid_q[i], out_pipe_ready[i], flush_i, 1'b0, clk_i, rst_ni)
    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = out_pipe_ready[i] & out_pipe_valid_q[i];
    // Generate the pipeline registers within the stages, use enable-registers
    `FFL(out_pipe_result_q[i+1], out_pipe_result_q[i], reg_ena, '0)
    `FFL(out_pipe_status_q[i+1], out_pipe_status_q[i], reg_ena, '0)
    `FFL(out_pipe_tag_q[i+1],    out_pipe_tag_q[i],    reg_ena, TagType'('0))
    `FFL(out_pipe_aux_q[i+1],    out_pipe_aux_q[i],    reg_ena, AuxType'('0))
  end
  // Output stage: Ready travels backwards from output side, driven by downstream circuitry
  assign out_pipe_ready[NUM_OUT_REGS] = out_ready_i;
  // Output stage: assign module outputs
  assign result_o        = out_pipe_result_q[NUM_OUT_REGS];
  assign status_o        = out_pipe_status_q[NUM_OUT_REGS];
  assign extension_bit_o = 1'b1; // always NaN-Box result
  assign tag_o           = out_pipe_tag_q[NUM_OUT_REGS];
  assign aux_o           = out_pipe_aux_q[NUM_OUT_REGS];
  assign out_valid_o     = out_pipe_valid_q[NUM_OUT_REGS];
  assign busy_o          = (| {inp_pipe_valid_q, mid_pipe_valid_q, out_pipe_valid_q});
endmodule
