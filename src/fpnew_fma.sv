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
//
// SPDX-License-Identifier: SHL-0.51

// Author: Stefan Mach <smach@iis.ee.ethz.ch>

`include "common_cells/registers.svh"

module fpnew_fma #(
  parameter fpnew_pkg::fp_format_e   FpFormat    = fpnew_pkg::fp_format_e'(0),
  parameter int unsigned             NumPipeRegs = 0,
  parameter fpnew_pkg::pipe_config_t PipeConfig  = fpnew_pkg::BEFORE,
  parameter type                     TagType     = logic,
  parameter type                     AuxType     = logic,
  // Do not change
  localparam int unsigned WIDTH = fpnew_pkg::fp_width(FpFormat),
  localparam int unsigned ExtRegEnaWidth = NumPipeRegs == 0 ? 1 : NumPipeRegs
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
  input logic                      mask_i,
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
  output logic                     mask_o,
  output AuxType                   aux_o,
  // Output handshake
  output logic                     out_valid_o,
  input  logic                     out_ready_i,
  // Indication of valid data in flight
  output logic                     busy_o,
  // External register enable override
  input  logic [ExtRegEnaWidth-1:0] reg_ena_i
);
  import fpnew_pkg::*;
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
  // Shift amount width: maximum internal mantissa size is 3p+4 bits
  localparam int unsigned SHIFT_AMOUNT_WIDTH = $clog2(3 * PRECISION_BITS + 5);
  // Pipelines
  localparam NUM_INP_REGS = (PipeConfig == fpnew_pkg::BEFORE
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? ((NumPipeRegs + 1) / 3) // Second to get distributed regs
                               : 0))-1; // no regs here otherwise
  localparam NUM_MID_REGS = (PipeConfig == fpnew_pkg::INSIDE
                          ? NumPipeRegs
                          : (PipeConfig == fpnew_pkg::DISTRIBUTED
                             ? ((NumPipeRegs + 2) / 3) // First to get distributed regs
                             : 0))-1; // no regs here otherwise
  localparam NUM_OUT_REGS = (PipeConfig == fpnew_pkg::AFTER
                            ? NumPipeRegs
                            : (PipeConfig == fpnew_pkg::DISTRIBUTED
                               ? (NumPipeRegs / 3) // Last to get distributed regs
                               : 0))-1; // no regs here otherwise

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
  // Input pipeline signals, index i holds signal after i register stages
  logic                                  [2:0][WIDTH-1:0] inp_pipe_operands_q_w;
  logic                                  [2:0]            inp_pipe_is_boxed_q_w;
  fpnew_pkg::roundmode_e                                  inp_pipe_rnd_mode_q_w;
  fpnew_pkg::operation_e                                  inp_pipe_op_q_w;
  logic                                                   inp_pipe_op_mod_q_w;
  TagType                                                 inp_pipe_tag_q_w;
  logic                                                   inp_pipe_mask_q_w;
  AuxType                                                 inp_pipe_aux_q_w;
  logic                                                   inp_pipe_valid_q_w;

  logic                  [0:NUM_INP_REGS][2:0][WIDTH-1:0] inp_pipe_operands_q;
  logic                  [0:NUM_INP_REGS][2:0]            inp_pipe_is_boxed_q;
  fpnew_pkg::roundmode_e [0:NUM_INP_REGS]                 inp_pipe_rnd_mode_q;
  fpnew_pkg::operation_e [0:NUM_INP_REGS]                 inp_pipe_op_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_op_mod_q;
  TagType                [0:NUM_INP_REGS]                 inp_pipe_tag_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_mask_q;
  AuxType                [0:NUM_INP_REGS]                 inp_pipe_aux_q;
  logic                  [0:NUM_INP_REGS]                 inp_pipe_valid_q;
  // Ready signal is combinatorial for all stages
  logic [0:NUM_INP_REGS+1] inp_pipe_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign inp_pipe_operands_q_w = operands_i;
  assign inp_pipe_is_boxed_q_w = is_boxed_i;
  assign inp_pipe_rnd_mode_q_w = rnd_mode_i;
  assign inp_pipe_op_q_w       = op_i;
  assign inp_pipe_op_mod_q_w   = op_mod_i;
  assign inp_pipe_tag_q_w      = tag_i;
  assign inp_pipe_mask_q_w     = mask_i;
  assign inp_pipe_aux_q_w      = aux_i;
  assign inp_pipe_valid_q_w    = in_valid_i;
  // Input stage: Propagate pipeline ready signal to updtream circuitry
  assign in_ready_o = inp_pipe_ready[0];
  // Generate the register stages
  for (genvar i = 0; i < NUM_INP_REGS+1; i++) begin : gen_input_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign inp_pipe_ready[i] = inp_pipe_ready[i] | ~inp_pipe_valid_q[i];
    // Valid: enabled by ready signal, synchronous clear with the flush signal
    if(i==0) begin
      `FFLARNC(inp_pipe_valid_q[i], inp_pipe_valid_q_w, inp_pipe_ready[0], flush_i, 1'b0, clk_i, rst_ni)
    end else begin
      `FFLARNC(inp_pipe_valid_q[i], inp_pipe_valid_q[i-1], inp_pipe_ready[i], flush_i, 1'b0, clk_i, rst_ni)
    end
    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = (inp_pipe_ready[i] & inp_pipe_valid_q[i]) | reg_ena_i[i];
    // Generate the pipeline registers within the stages, use enable-registers
    if(i==0) begin
      `FFL(inp_pipe_operands_q[i], inp_pipe_operands_q_w, reg_ena, '0)
      `FFL(inp_pipe_is_boxed_q[i], inp_pipe_is_boxed_q_w, reg_ena, '0)
      `FFL(inp_pipe_rnd_mode_q[i], inp_pipe_rnd_mode_q_w, reg_ena, fpnew_pkg::RNE)
      `FFL(inp_pipe_op_q[i],       inp_pipe_op_q_w,       reg_ena, fpnew_pkg::FMADD)
      `FFL(inp_pipe_op_mod_q[i],   inp_pipe_op_mod_q_w,   reg_ena, '0)
      `FFL(inp_pipe_tag_q[i],      inp_pipe_tag_q_w,      reg_ena, TagType'('0))
      `FFL(inp_pipe_mask_q[i],     inp_pipe_mask_q_w,     reg_ena, '0)
      `FFL(inp_pipe_aux_q[i],      inp_pipe_aux_q_w,      reg_ena, AuxType'('0))
    end else begin
      `FFL(inp_pipe_operands_q[i], inp_pipe_operands_q[i-1], reg_ena, '0)
      `FFL(inp_pipe_is_boxed_q[i], inp_pipe_is_boxed_q[i-1], reg_ena, '0)
      `FFL(inp_pipe_rnd_mode_q[i], inp_pipe_rnd_mode_q[i-1], reg_ena, fpnew_pkg::RNE)
      `FFL(inp_pipe_op_q[i],       inp_pipe_op_q[i-1],       reg_ena, fpnew_pkg::FMADD)
      `FFL(inp_pipe_op_mod_q[i],   inp_pipe_op_mod_q[i-1],   reg_ena, '0)
      `FFL(inp_pipe_tag_q[i],      inp_pipe_tag_q[i-1],      reg_ena, TagType'('0))
      `FFL(inp_pipe_mask_q[i],     inp_pipe_mask_q[i-1],     reg_ena, '0)
      `FFL(inp_pipe_aux_q[i],      inp_pipe_aux_q[i-1],      reg_ena, AuxType'('0))
    end
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
    .operands_i ( inp_pipe_operands_q[NUM_INP_REGS] ),
    .is_boxed_i ( inp_pipe_is_boxed_q[NUM_INP_REGS] ),
    .info_o     ( info_q                            )
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
  // | ADD      | \c 0        | ADD: Set operand A to .0
  // | ADD      | \c 1        | SUB: Set operand A to .0, invert sign of operand C
  // | MUL      | \c 0        | MUL: Set operand C to +0.0 or -0.0 depending on the rounding mode
  // | *others* | \c -        | *invalid*
  // \note \c op_mod_q always inverts the sign of the addend.
  always_comb begin : op_select

    // Default assignments - packing-order-agnostic
    operand_a = inp_pipe_operands_q[NUM_INP_REGS][0];
    operand_b = inp_pipe_operands_q[NUM_INP_REGS][1];
    operand_c = inp_pipe_operands_q[NUM_INP_REGS][2];
    info_a    = info_q[0];
    info_b    = info_q[1];
    info_c    = info_q[2];

    // op_mod_q inverts sign of operand C
    operand_c.sign = operand_c.sign ^ inp_pipe_op_mod_q[NUM_INP_REGS];

    unique case (inp_pipe_op_q[NUM_INP_REGS])
      fpnew_pkg::FMADD:  ; // do nothing
      fpnew_pkg::FNMSUB: operand_a.sign = ~operand_a.sign; // invert sign of product
      fpnew_pkg::ADD,
      fpnew_pkg::ADDS: begin // Set multiplicand to 
        operand_a = '{sign: 1'b0, exponent: BIAS, mantissa: '0};
        info_a    = '{is_normal: 1'b1, is_boxed: 1'b1, default: 1'b0}; //normal, boxed value.
      end
      fpnew_pkg::MUL: begin // Set addend to +0 or -0, depending whether the rounding mode is RDN
        if (inp_pipe_rnd_mode_q[NUM_INP_REGS] == fpnew_pkg::RDN)
          operand_c = '{sign: 1'b0, exponent: '0, mantissa: '0};
        else
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
  // Input classification //输入特殊情况分类
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
  // Special case handling 特殊情况处理
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

  // ------
  // Adder
  // ------
  logic [3*PRECISION_BITS+4:0] sum_pos, sum_neg; // added one bit for the carry
  logic                        sum_carry;        // observe carry bit from positive sum for sign fixing
  logic [3*PRECISION_BITS+3:0] sum;              // discard carry as sum won't overflow
  logic                        final_sign;

  //Mantissa adder (ab+c). In normal addition, it cannot overflow.
  assign sum_pos = product_shifted + addend_shifted + inject_carry_in;
  assign sum_carry = sum_pos[3*PRECISION_BITS+4];

  // Parallel adder for negative sum (only used for effective subtractions).
  // Note: inject_carry_in is used to complete the negation of the addend in the positive sum but
  // for the negative sum the addend is not negated, so no carry needs to be injected.
  assign sum_neg = addend_after_shift - product_shifted;

  // Complement negative sum (can only happen in subtraction -> overflows for positive results)
  assign sum        = (effective_subtraction && ~sum_carry) ? sum_neg : sum_pos;

  // In case of a mispredicted subtraction result, do a sign flip
  assign final_sign = (effective_subtraction && (sum_carry == tentative_sign))
                      ? 1'b1
                      : (effective_subtraction ? 1'b0 : tentative_sign);

  // ---------------
  // Internal pipeline
  // ---------------
  // Pipeline output signals as non-arrays
  logic                          effective_subtraction_q;
  logic signed [EXP_WIDTH-1:0]   exponent_product_q;
  logic signed [EXP_WIDTH-1:0]   exponent_difference_q;
  logic signed [EXP_WIDTH-1:0]   tentative_exponent_q;
  logic [SHIFT_AMOUNT_WIDTH-1:0] addend_shamt_q;
  logic                          sticky_before_add_q;
  logic [3*PRECISION_BITS+3:0]   sum_q;
  logic                          final_sign_q;
  fpnew_pkg::roundmode_e         rnd_mode_q;
  logic                          result_is_special_q;
  fp_t                           special_result_q;
  fpnew_pkg::status_t            special_status_q;
  // Internal pipeline signals, index i holds signal after i register stages
  logic                                                           mid_pipe_eff_sub_q_w;
  logic signed                           [EXP_WIDTH-1:0]          mid_pipe_exp_prod_q_w;
  logic signed                           [EXP_WIDTH-1:0]          mid_pipe_exp_diff_q_w;
  logic signed                           [EXP_WIDTH-1:0]          mid_pipe_tent_exp_q_w;
  logic                                  [SHIFT_AMOUNT_WIDTH-1:0] mid_pipe_add_shamt_q_w;
  logic                                                           mid_pipe_sticky_q_w;
  logic                                  [3*PRECISION_BITS+3:0]   mid_pipe_sum_q_w;
  logic                                                           mid_pipe_final_sign_q_w;
  fpnew_pkg::roundmode_e                                          mid_pipe_rnd_mode_q_w;
  logic                                                           mid_pipe_res_is_spec_q_w;
  fp_t                                                            mid_pipe_spec_res_q_w;
  fpnew_pkg::status_t                                             mid_pipe_spec_stat_q_w;
  TagType                                                         mid_pipe_tag_q_w;
  logic                                                           mid_pipe_mask_q_w;
  AuxType                                                         mid_pipe_aux_q_w;
  logic                                                           mid_pipe_valid_q_w;

  logic                  [0:NUM_MID_REGS]                         mid_pipe_eff_sub_q;
  logic signed           [0:NUM_MID_REGS][EXP_WIDTH-1:0]          mid_pipe_exp_prod_q;
  logic signed           [0:NUM_MID_REGS][EXP_WIDTH-1:0]          mid_pipe_exp_diff_q;
  logic signed           [0:NUM_MID_REGS][EXP_WIDTH-1:0]          mid_pipe_tent_exp_q;
  logic                  [0:NUM_MID_REGS][SHIFT_AMOUNT_WIDTH-1:0] mid_pipe_add_shamt_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_sticky_q;
  logic                  [0:NUM_MID_REGS][3*PRECISION_BITS+3:0]   mid_pipe_sum_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_final_sign_q;
  fpnew_pkg::roundmode_e [0:NUM_MID_REGS]                         mid_pipe_rnd_mode_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_res_is_spec_q;
  fp_t                   [0:NUM_MID_REGS]                         mid_pipe_spec_res_q;
  fpnew_pkg::status_t    [0:NUM_MID_REGS]                         mid_pipe_spec_stat_q;
  TagType                [0:NUM_MID_REGS]                         mid_pipe_tag_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_mask_q;
  AuxType                [0:NUM_MID_REGS]                         mid_pipe_aux_q;
  logic                  [0:NUM_MID_REGS]                         mid_pipe_valid_q;
  // Ready signal is combinatorial for all stages
  logic [0:NUM_MID_REGS+1] mid_pipe_ready;

  // Input stage: First element of pipeline is taken from upstream logic
  // 基本运算控制信号
  assign mid_pipe_eff_sub_q_w     = effective_subtraction;  // 有效减法标志（AB-C运算）
  assign mid_pipe_exp_prod_q_w    = exponent_product;       // 乘积项的指数值
  assign mid_pipe_exp_diff_q_w    = exponent_difference;    // 加数与乘积的指数差
  assign mid_pipe_tent_exp_q_w    = tentative_exponent;     // 暂定结果指数（max(乘积指数,加数指数)）
  // 移位相关参数
  assign mid_pipe_add_shamt_q_w   = addend_shamt;           // 加数右移量（用于对齐尾数）
  assign mid_pipe_sticky_q_w      = sticky_before_add;      // 移位丢失的粘滞位
  // 加法器输出结果
  assign mid_pipe_sum_q_w         = sum;                    // 尾数求和结果（3p+4位）
  assign mid_pipe_final_sign_q_w  = final_sign;             // 最终符号位
  // 特殊结果处理
  assign mid_pipe_rnd_mode_q_w    = inp_pipe_rnd_mode_q[NUM_INP_REGS]; // 舍入模式（继承输入流水线）
  assign mid_pipe_res_is_spec_q_w = result_is_special;                 // 特殊结果标识（NaN/inf/zero）
  assign mid_pipe_spec_res_q_w    = special_result;                    // 特殊结果值（qNaN/inf）
  assign mid_pipe_spec_stat_q_w   = special_status;                    // 特殊结果状态（异常标志）
  // 操作元数据
  assign mid_pipe_tag_q_w         = inp_pipe_tag_q[NUM_INP_REGS];      // 操作标签（多操作追踪）
  assign mid_pipe_mask_q_w        = inp_pipe_mask_q[NUM_INP_REGS];     // 操作掩码（SIMD控制）
  assign mid_pipe_aux_q_w         = inp_pipe_aux_q[NUM_INP_REGS];      // 辅助数据（异常处理）
  assign mid_pipe_valid_q_w       = inp_pipe_valid_q[NUM_INP_REGS];    // 数据有效标志
  // Input stage: Propagate pipeline ready signal to input pipe
  assign inp_pipe_ready[NUM_INP_REGS+1] = mid_pipe_ready[0];

  // Generate the register stages
  for (genvar i = 0; i < NUM_MID_REGS+1; i++) begin : gen_inside_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign mid_pipe_ready[i] = mid_pipe_ready[i+1] | ~mid_pipe_valid_q[i];
    // Valid: enabled by ready signal, synchronous clear with the flush signal
    if(i==0) begin
      `FFLARNC(mid_pipe_valid_q[i], mid_pipe_valid_q_w, mid_pipe_ready[0], flush_i, 1'b0, clk_i, rst_ni)
    end else begin
      `FFLARNC(mid_pipe_valid_q[i], mid_pipe_valid_q[i-1], mid_pipe_ready[i], flush_i, 1'b0, clk_i, rst_ni)
    end
    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = (mid_pipe_ready[i] & mid_pipe_valid_q[i]) | reg_ena_i[NUM_INP_REGS + i];
    // Generate the pipeline registers within the stages, use enable-registers
    if(i==0) begin
      `FFL(mid_pipe_eff_sub_q[i],     mid_pipe_eff_sub_q_w,     reg_ena, '0)
      `FFL(mid_pipe_exp_prod_q[i],    mid_pipe_exp_prod_q_w,    reg_ena, '0)
      `FFL(mid_pipe_exp_diff_q[i],    mid_pipe_exp_diff_q_w,    reg_ena, '0)
      `FFL(mid_pipe_tent_exp_q[i],    mid_pipe_tent_exp_q_w,    reg_ena, '0)
      `FFL(mid_pipe_add_shamt_q[i],   mid_pipe_add_shamt_q_w,   reg_ena, '0)
      `FFL(mid_pipe_sticky_q[i],      mid_pipe_sticky_q_w,      reg_ena, '0)
      `FFL(mid_pipe_sum_q[i],         mid_pipe_sum_q_w,         reg_ena, '0)
      `FFL(mid_pipe_final_sign_q[i],  mid_pipe_final_sign_q_w,  reg_ena, '0)
      `FFL(mid_pipe_rnd_mode_q[i],    mid_pipe_rnd_mode_q_w,    reg_ena, fpnew_pkg::RNE)
      `FFL(mid_pipe_res_is_spec_q[i], mid_pipe_res_is_spec_q_w, reg_ena, '0)
      `FFL(mid_pipe_spec_res_q[i],    mid_pipe_spec_res_q_w,    reg_ena, '0)
      `FFL(mid_pipe_spec_stat_q[i],   mid_pipe_spec_stat_q_w,   reg_ena, '0)
      `FFL(mid_pipe_tag_q[i],         mid_pipe_tag_q_w,         reg_ena, TagType'('0))
      `FFL(mid_pipe_mask_q[i],        mid_pipe_mask_q_w,        reg_ena, '0)
      `FFL(mid_pipe_aux_q[i],         mid_pipe_aux_q_w,         reg_ena, AuxType'('0))
    end else begin
      `FFL(mid_pipe_eff_sub_q[i],     mid_pipe_eff_sub_q[i-1],     reg_ena, '0)
      `FFL(mid_pipe_exp_prod_q[i],    mid_pipe_exp_prod_q[i-1],    reg_ena, '0)
      `FFL(mid_pipe_exp_diff_q[i],    mid_pipe_exp_diff_q[i-1],    reg_ena, '0)
      `FFL(mid_pipe_tent_exp_q[i],    mid_pipe_tent_exp_q[i-1],    reg_ena, '0)
      `FFL(mid_pipe_add_shamt_q[i],   mid_pipe_add_shamt_q[i-1],   reg_ena, '0)
      `FFL(mid_pipe_sticky_q[i],      mid_pipe_sticky_q[i-1],      reg_ena, '0)
      `FFL(mid_pipe_sum_q[i],         mid_pipe_sum_q[i-1],         reg_ena, '0)
      `FFL(mid_pipe_final_sign_q[i],  mid_pipe_final_sign_q[i-1],  reg_ena, '0)
      `FFL(mid_pipe_rnd_mode_q[i],    mid_pipe_rnd_mode_q[i-1],    reg_ena, fpnew_pkg::RNE)
      `FFL(mid_pipe_res_is_spec_q[i], mid_pipe_res_is_spec_q[i-1], reg_ena, '0)
      `FFL(mid_pipe_spec_res_q[i],    mid_pipe_spec_res_q[i-1],    reg_ena, '0)
      `FFL(mid_pipe_spec_stat_q[i],   mid_pipe_spec_stat_q[i-1],   reg_ena, '0)
      `FFL(mid_pipe_tag_q[i],         mid_pipe_tag_q[i-1],         reg_ena, TagType'('0))
      `FFL(mid_pipe_mask_q[i],        mid_pipe_mask_q[i-1],        reg_ena, '0)
      `FFL(mid_pipe_aux_q[i],         mid_pipe_aux_q[i-1],         reg_ena, AuxType'('0))
    end
  end
  // Output stage: assign selected pipe outputs to signals for later use
  assign effective_subtraction_q = mid_pipe_eff_sub_q[NUM_MID_REGS];
  assign exponent_product_q      = mid_pipe_exp_prod_q[NUM_MID_REGS];
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
        norm_shamt          = unsigned'(signed'(PRECISION_BITS) + 2 + exponent_product_q);
        normalized_exponent = 0; // subnormals encoded as 0
      end
    // Addend-anchored case
    end else begin
      norm_shamt          = addend_shamt_q; // Undo the initial shift
      normalized_exponent = tentative_exponent_q;
    end
  end

  // Do the large normalization shift
  assign sum_shifted       = sum_q << norm_shamt;

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
  assign pre_round_sign     = final_sign_q;
  assign pre_round_exponent = (of_before_round) ? 2**EXP_BITS-2 : unsigned'(final_exponent[EXP_BITS-1:0]);
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
    .rnd_mode_i              ( rnd_mode_q              ),
    .effective_subtraction_i ( effective_subtraction_q ),
    .abs_rounded_o           ( rounded_abs             ),
    .sign_o                  ( rounded_sign            ),
    .exact_zero_o            ( result_zero             )
  );

  // Classification after rounding
  assign uf_after_round = (rounded_abs[EXP_BITS+MAN_BITS-1:MAN_BITS] == '0) // denormal
        || ((pre_round_abs[EXP_BITS+MAN_BITS-1:MAN_BITS] == '0) && (rounded_abs[EXP_BITS+MAN_BITS-1:MAN_BITS] == 1) && 
           ((round_sticky_bits != 2'b11) || (!sum_sticky_bits[MAN_BITS*2 + 4] && ((rnd_mode_q == fpnew_pkg::RNE) || (rnd_mode_q == fpnew_pkg::RMM)))));
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

  // Final results for output pipeline
  fp_t                result_d;
  fpnew_pkg::status_t status_d;

  // Select output depending on special case detection
  assign result_d = result_is_special_q ? special_result_q : regular_result;
  assign status_d = result_is_special_q ? special_status_q : regular_status;

  // ----------------
  // Output Pipeline
  // ----------------
  // Output pipeline signals, index i holds signal after i register stages
  fp_t                                 out_pipe_result_q_w;
  fpnew_pkg::status_t                  out_pipe_status_q_w;
  TagType                              out_pipe_tag_q_w;
  logic                                out_pipe_mask_q_w;
  AuxType                              out_pipe_aux_q_w;
  logic                                out_pipe_valid_q_w;

  fp_t                [0:NUM_OUT_REGS] out_pipe_result_q;
  fpnew_pkg::status_t [0:NUM_OUT_REGS] out_pipe_status_q;
  TagType             [0:NUM_OUT_REGS] out_pipe_tag_q;
  logic               [0:NUM_OUT_REGS] out_pipe_mask_q;
  AuxType             [0:NUM_OUT_REGS] out_pipe_aux_q;
  logic               [0:NUM_OUT_REGS] out_pipe_valid_q;
  // Ready signal is combinatorial for all stages
  logic [0:NUM_OUT_REGS+1] out_pipe_ready;

  // Input stage: First element of pipeline is taken from inputs
  assign out_pipe_result_q_w = result_d;
  assign out_pipe_status_q_w = status_d;
  assign out_pipe_tag_q_w    = mid_pipe_tag_q[NUM_MID_REGS];
  assign out_pipe_mask_q_w   = mid_pipe_mask_q[NUM_MID_REGS];
  assign out_pipe_aux_q_w    = mid_pipe_aux_q[NUM_MID_REGS];
  assign out_pipe_valid_q_w  = mid_pipe_valid_q[NUM_MID_REGS];
  // Input stage: Propagate pipeline ready signal to inside pipe
  assign mid_pipe_ready[NUM_MID_REGS+1] = out_pipe_ready[0];
  // Generate the register stages
  for (genvar i = 0; i < NUM_OUT_REGS+1; i++) begin : gen_output_pipeline
    // Internal register enable for this stage
    logic reg_ena;
    // Determine the ready signal of the current stage - advance the pipeline:
    // 1. if the next stage is ready for our data
    // 2. if the next stage only holds a bubble (not valid) -> we can pop it
    assign out_pipe_ready[i] = out_pipe_ready[i+1] | ~out_pipe_valid_q[i];
    // Valid: enabled by ready signal, synchronous clear with the flush signal
    if(i==0) begin
      `FFLARNC(out_pipe_valid_q[i], out_pipe_valid_q_w, out_pipe_ready[0], flush_i, 1'b0, clk_i, rst_ni)
    end else begin
      `FFLARNC(out_pipe_valid_q[i], out_pipe_valid_q[i-1], out_pipe_ready[i], flush_i, 1'b0, clk_i, rst_ni)
    end
    // Enable register if pipleine ready and a valid data item is present
    assign reg_ena = (out_pipe_ready[i] & out_pipe_valid_q[i]) | reg_ena_i[NUM_INP_REGS + NUM_MID_REGS + i];
    // Generate the pipeline registers within the stages, use enable-registers
    if(i==0) begin
      `FFL(out_pipe_result_q[i], out_pipe_result_q_w, reg_ena, '0)
      `FFL(out_pipe_status_q[i], out_pipe_status_q_w, reg_ena, '0)
      `FFL(out_pipe_tag_q[i],    out_pipe_tag_q_w,    reg_ena, TagType'('0))
      `FFL(out_pipe_mask_q[i],   out_pipe_mask_q_w,   reg_ena, '0)
      `FFL(out_pipe_aux_q[i],    out_pipe_aux_q_w,    reg_ena, AuxType'('0))
    end else begin
      `FFL(out_pipe_result_q[i], out_pipe_result_q[i-1], reg_ena, '0)
      `FFL(out_pipe_status_q[i], out_pipe_status_q[i-1], reg_ena, '0)
      `FFL(out_pipe_tag_q[i],    out_pipe_tag_q[i-1],    reg_ena, TagType'('0))
      `FFL(out_pipe_mask_q[i],   out_pipe_mask_q[i-1],   reg_ena, '0)
      `FFL(out_pipe_aux_q[i],    out_pipe_aux_q[i-1],    reg_ena, AuxType'('0))
    end
  end
  // Output stage: Ready travels backwards from output side, driven by downstream circuitry
  assign out_pipe_ready[NUM_OUT_REGS+1] = out_ready_i;
  // Output stage: assign module outputs
  assign result_o        = out_pipe_result_q[NUM_OUT_REGS];
  assign status_o        = out_pipe_status_q[NUM_OUT_REGS];
  assign extension_bit_o = 1'b1; // always NaN-Box result
  assign tag_o           = out_pipe_tag_q[NUM_OUT_REGS];
  assign mask_o          = out_pipe_mask_q[NUM_OUT_REGS];
  assign aux_o           = out_pipe_aux_q[NUM_OUT_REGS];
  assign out_valid_o     = out_pipe_valid_q[NUM_OUT_REGS];
  assign busy_o          = (| {inp_pipe_valid_q, mid_pipe_valid_q, out_pipe_valid_q});
endmodule
