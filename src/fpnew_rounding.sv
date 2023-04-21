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

module fpnew_rounding #(
  parameter int unsigned          AbsWidth      = 2, // Width of the abolute value, without sign bit
  parameter logic                 EnableRSR     = 0,
  parameter int unsigned          RsrPrecision  = 12,
  //LFSR patameters
  parameter int unsigned          LfsrWidth     = 32,
  parameter logic [LfsrWidth-1:0] RstVal        = '1
) (
  // LFSR inputs
  input logic                  clk_i,
  input logic                  rst_ni,
  input logic [33:0]           id_i,
  // Input value
  input logic [AbsWidth-1:0]   abs_value_i,             // absolute value without sign
  input logic                  sign_i,
  input logic                  en_rsr_i,
  // Rounding information
  input logic [1:0]            round_sticky_bits_i,     // round and sticky bits {RS}
  input logic [RsrPrecision-1:0] stochastic_rounding_bits_i,
  input fpnew_pkg::roundmode_e rnd_mode_i,
  input logic                  effective_subtraction_i, // sign of inputs affects rounding of zeroes
  // Output value
  output logic [AbsWidth-1:0]  abs_rounded_o,           // absolute value without sign
  output logic                 sign_o,
  // Output classification
  output logic                 exact_zero_o             // output is an exact zero
);

  logic round_up; // Rounding decision

  // Take the rounding decision according to RISC-V spec, plus additional unbiased rounding modes
  // RoundMode | Mnemonic | Meaning
  // :--------:|:--------:|:-------
  //    000    |   RNE    | Round to Nearest, ties to Even
  //    001    |   RTZ    | Round towards Zero
  //    010    |   RDN    | Round Down (towards -\infty)
  //    011    |   RUP    | Round Up (towards \infty)
  //    100    |   RMM    | Round to Nearest, ties to Max Magnitude
  //    101    |   RSR    | Round by Stochastic Rounding
  //    100    |   RR     | Round by Random Rounding
  //  others   |          | *invalid*

  // LFSR generating random numbers for RSR mode
  logic [RsrPrecision-1:0] lfsr_out;

  if (EnableRSR) begin : gen_lfsr
    lfsr_sr #(
      .LfsrWidth       ( LfsrWidth           ),
      .OutWidth        ( RsrPrecision        ),
      .RstVal          ( RstVal              ),
      .CipherLayers    ( 0                   ),
      .CipherReg       ( 0                   )
    ) i_lfsr (
      .clk_i           ( clk_i               ),
      .rst_ni          ( rst_ni              ),
      .id_i            ( id_i                ),
      .en_i            ( en_rsr_i            ),
      .out_o           ( lfsr_out            )
    );
  end else begin
    assign lfsr_out = '0;
  end

  // Rounding results by stochastic rounding
  always_comb begin : rounding_decision
    unique case (rnd_mode_i)
      // Decide according to round/sticky bits
      fpnew_pkg::RNE:
        unique case (round_sticky_bits_i)
          2'b00,
          2'b01: round_up = 1'b0;           // < ulp/2 away, round down
          2'b10: round_up = abs_value_i[0]; // = ulp/2 away, round towards even result
          2'b11: round_up = 1'b1;           // > ulp/2 away, round up
          default: round_up = fpnew_pkg::DONT_CARE;
        endcase
      fpnew_pkg::RTZ: round_up = 1'b0; // always round down
      fpnew_pkg::RDN: round_up = (| round_sticky_bits_i) ? sign_i  : 1'b0; // to 0 if +, away if -
      fpnew_pkg::RUP: round_up = (| round_sticky_bits_i) ? ~sign_i : 1'b0; // to 0 if -, away if +
      fpnew_pkg::RMM: round_up = round_sticky_bits_i[1]; // round down if < ulp/2 away, else up
      // Decide stochastically, comparing trailing bits and pseudo-random number
      fpnew_pkg::RSR: begin
        if (EnableRSR) begin
          round_up = (lfsr_out < stochastic_rounding_bits_i) ? 1'b1 : 1'b0;
        end else begin
          round_up = fpnew_pkg::DONT_CARE;
        end
      end
      // Decide randomly
      fpnew_pkg::RR: begin
        if (EnableRSR) begin
          round_up = lfsr_out[RsrPrecision-1]; // round up if MSB LSFR is 1
        end else begin
          round_up = fpnew_pkg::DONT_CARE;
        end
      end
      default: round_up = fpnew_pkg::DONT_CARE; // propagate x
    endcase
  end

  // Perform the rounding, exponent change and overflow to inf happens automagically
  assign abs_rounded_o = abs_value_i + round_up;

  // True zero result is a zero result without dirty round/sticky bits
  assign exact_zero_o = (abs_value_i == '0) && (round_sticky_bits_i == '0);

  // In case of effective subtraction (thus signs of addition operands must have differed) and a
  // true zero result, the result sign is '-' in case of RDN and '+' for other modes.
  assign sign_o = (exact_zero_o && effective_subtraction_i)
                  ? (rnd_mode_i == fpnew_pkg::RDN)
                  : sign_i;

endmodule
