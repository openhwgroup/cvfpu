// Copyright (c) 2018 ETH Zurich, University of Bologna
// All rights reserved.
//
// This code is under development and not yet released to the public.
// Until it is released, the code is under the copyright of ETH Zurich and
// the University of Bologna, and may contain confidential and/or unpublished
// work. Any reuse/redistribution is strictly forbidden without written
// permission from ETH Zurich.
//
// Bug fixes and contributions will eventually be released under the
// SolderPad open hardware license in the context of the PULP platform
// (http://www.pulp-platform.org), under the copyright of ETH Zurich and the
// University of Bologna.

// Author: Stefan Mach <smach@iis.ee.ethz.ch>

module fpnew_rounding #(
  parameter int unsigned AbsWidth // Width of the abolute value, without sign bit
) (
  // Input value
  input logic [AbsWidth-1:0]   abs_value_i,             // absolute value without sign
  input logic                  sign_i,
  // Rounding information
  input logic [1:0]            round_sticky_bits_i,     // round and sticky bits {RS}
  input fpnew_pkg::roundmode_e rnd_mode_i,
  input logic                  effective_subtraction_i, // sign of inputs affects rounding of zeroes
  // Output value
  output logic [AbsWidth-1:0]  abs_rounded_o,           // absolute value without sign
  output logic                 sign_o,
  // Output classification
  output logic                 zero_o                   // output is a true zero
);

  logic round_up; // Rounding decision

  // Take the rounding decision according to RISC-V spec
  // RoundMode | Mnemonic | Meaning
  // :--------:|:--------:|:-------
  //    000    |   RNE    | Round to Nearest, ties to Even
  //    001    |   RTZ    | Round towards Zero
  //    010    |   RDN    | Round Down (towards -\infty)
  //    011    |   RUP    | Round Up (towards \infty)
  //    100    |   RMM    | Round to Nearest, ties to Max Magnitude
  //  others   |          | *invalid*
  always_comb begin : rounding_decision
    unique case (rnd_mode_i)
      fpnew_pkg::RNE: // Decide accoring to round/sticky bits
        unique case (round_sticky_bits_i)
          2'b00,
          2'b01: round_up = 1'b0;           // < ulp/2 away, round down
          2'b10: round_up = abs_value_i[0]; // = ulp/2 away, round towards even result
          2'b11: round_up = 1'b1;           // > ulp/2 away, round up
          default: round_up = 1'bx; // propagate X
        endcase
      fpnew_pkg::RTZ: round_up = 1'b0;                   // always round down
      fpnew_pkg::RDN: round_up = sign_i;                 // round towards zero if +, away if -
      fpnew_pkg::RUP: round_up = ~sign_i;                // round towards zero if -, away if +
      fpnew_pkg::RMM: round_up = round_sticky_bits_i[1]; // round down if < ulp/2 away, else up
      default: round_up = 1'bx; // propagate x
    endcase
  end

  // Perform the rounding, exponent change and overflow to inf happens automagically
  assign abs_rounded_o = abs_value_i + round_up;

  // True zero result is a zero result without dirty round/sticky bits
  assign zero_o = (abs_value_i == '0) && (round_sticky_bits_i == '0);

  // In case of effective subtraction (thus signs of addition operands must have differed) and a
  // true zero result, the result sign is '-' in case of RDN and '+' for other modes.
  assign sign_o = (zero_o && effective_subtraction_i) ? (rnd_mode_i == fpnew_pkg::RDN) : sign_i;

endmodule
