///////////////////////////////////////////
// fmalza.sv
//
// Written:  6/23/2021 me@KatherineParry.com, David_Harris@hmc.edu
// Modified:
//
// Purpose: Leading Zero Anticipator
//
// Documentation: RISC-V System on Chip Design Chapter 13 (Figure 13.14)
//    See also [Schmookler & Nowka, Leading zero anticipation and detection, IEEE Sym. Computer Arithmetic, 2001]
//
// A component of the CORE-V-WALLY configurable RISC-V project.
// https://github.com/openhwgroup/cvw
//
// Copyright (C) 2021-23 Harvey Mudd College & Oklahoma State University
//
// SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
//
// Licensed under the Solderpad Hardware License v 2.1 (the “License”); you may not use this file
// except in compliance with the License, or, at your option, the Apache License version 2.0. You
// may obtain a copy of the License at
//
// https://solderpad.org/licenses/SHL-2.1/
//
// Unless required by applicable law or agreed to in writing, any work distributed under the
// License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
// either express or implied. See the License for the specific language governing permissions
// and limitations under the License.
////////////////////////////////////////////////////////////////////////////////////////////////

module fmalza #(WIDTH, NF) (
  input logic [WIDTH-1:0]             A,              // addend
  input logic [2*NF+1:0]              Pm,             // product
  input logic                         Cin,            // carry in
  input logic                         sub,            // subtraction
  output logic [$clog2(WIDTH+1)-1:0]  SCnt            // normalization shift count for the positive result
);

  logic [WIDTH:0]                     F;              // most significant bit of F indicates leading digit
  logic [WIDTH-1:0]                   B;              // zero-extended product with same size as aligned A
  logic [WIDTH-1:0]                   P, G, K;        // propagate, generate, kill for each column
  logic [WIDTH-1:0]                   Pp1, Gm1, Km1;  // propagate shifted right by 1, generate/kill shifted left 1

  assign B = {{(NF+2){1'b0}}, Pm, 2'b0};              // Zero extend product

  assign P = A^B;
  assign G = A&B;
  assign K = ~A&~B;

  assign Pp1 = {sub, P[WIDTH-1:1]};                   // shift P right by 1 (for P_i+1) , use subtract flag in most significant bit
  assign Gm1 = {G[WIDTH-2:0], Cin};                   // shift G left by 1 (for G_i-1) and bring in Cin
  assign Km1 = {K[WIDTH-2:0], ~Cin};                  // shift K left by 1 (for K_i-1) and bring in Cin

  // Apply function to determine Leading pattern
  //      - note: Schmookler01 uses the numbering system where 0 is the most significant bit
  assign F[WIDTH]     = ~sub&P[WIDTH-1];
  assign F[WIDTH-1:0] = (Pp1&(G&~Km1 | K&~Gm1)) | (~Pp1&(K&~Km1 | G&~Gm1));

  lzc #(
    .WIDTH ( WIDTH+1 ),
    .MODE  ( 1       ) // MODE = 1 counts leading zeroes
  ) i_lzc (
    .in_i    ( F          ),
    .cnt_o   ( SCnt       ),
    .empty_o ( /*unused*/ )
  );

endmodule
