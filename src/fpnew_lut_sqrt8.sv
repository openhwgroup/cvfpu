// Copyright 2024 ETH Zurich and University of Bologna.
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

module fpnew_lut_sqrt8 (
  input  logic [7:0] input_i,
  output logic [7:0] out_o,
  output logic [4:0] status_o
);

  always_comb begin
    case (input_i)
      8'b00000000: begin
        out_o    = 8'h00;
        status_o = 5'h00;
      end
      8'b00000001: begin
        out_o    = 8'h1c;
        status_o = 5'h00;
      end
      8'b00000010: begin
        out_o    = 8'h1e;
        status_o = 5'h01;
      end
      8'b00000011: begin
        out_o    = 8'h1f;
        status_o = 5'h01;
      end
      8'b00000100: begin
        out_o    = 8'h20;
        status_o = 5'h00;
      end
      8'b00000101: begin
        out_o    = 8'h20;
        status_o = 5'h01;
      end
      8'b00000110: begin
        out_o    = 8'h21;
        status_o = 5'h01;
      end
      8'b00000111: begin
        out_o    = 8'h21;
        status_o = 5'h01;
      end
      8'b00001000: begin
        out_o    = 8'h22;
        status_o = 5'h01;
      end
      8'b00001001: begin
        out_o    = 8'h22;
        status_o = 5'h01;
      end
      8'b00001010: begin
        out_o    = 8'h23;
        status_o = 5'h01;
      end
      8'b00001011: begin
        out_o    = 8'h23;
        status_o = 5'h01;
      end
      8'b00001100: begin
        out_o    = 8'h24;
        status_o = 5'h00;
      end
      8'b00001101: begin
        out_o    = 8'h24;
        status_o = 5'h01;
      end
      8'b00001110: begin
        out_o    = 8'h25;
        status_o = 5'h01;
      end
      8'b00001111: begin
        out_o    = 8'h25;
        status_o = 5'h01;
      end
      8'b00010000: begin
        out_o    = 8'h26;
        status_o = 5'h01;
      end
      8'b00010001: begin
        out_o    = 8'h26;
        status_o = 5'h01;
      end
      8'b00010010: begin
        out_o    = 8'h27;
        status_o = 5'h01;
      end
      8'b00010011: begin
        out_o    = 8'h27;
        status_o = 5'h01;
      end
      8'b00010100: begin
        out_o    = 8'h28;
        status_o = 5'h00;
      end
      8'b00010101: begin
        out_o    = 8'h28;
        status_o = 5'h01;
      end
      8'b00010110: begin
        out_o    = 8'h29;
        status_o = 5'h01;
      end
      8'b00010111: begin
        out_o    = 8'h29;
        status_o = 5'h01;
      end
      8'b00011000: begin
        out_o    = 8'h2a;
        status_o = 5'h01;
      end
      8'b00011001: begin
        out_o    = 8'h2a;
        status_o = 5'h01;
      end
      8'b00011010: begin
        out_o    = 8'h2b;
        status_o = 5'h01;
      end
      8'b00011011: begin
        out_o    = 8'h2b;
        status_o = 5'h01;
      end
      8'b00011100: begin
        out_o    = 8'h2c;
        status_o = 5'h00;
      end
      8'b00011101: begin
        out_o    = 8'h2c;
        status_o = 5'h01;
      end
      8'b00011110: begin
        out_o    = 8'h2d;
        status_o = 5'h01;
      end
      8'b00011111: begin
        out_o    = 8'h2d;
        status_o = 5'h01;
      end
      8'b00100000: begin
        out_o    = 8'h2e;
        status_o = 5'h01;
      end
      8'b00100001: begin
        out_o    = 8'h2e;
        status_o = 5'h01;
      end
      8'b00100010: begin
        out_o    = 8'h2f;
        status_o = 5'h01;
      end
      8'b00100011: begin
        out_o    = 8'h2f;
        status_o = 5'h01;
      end
      8'b00100100: begin
        out_o    = 8'h30;
        status_o = 5'h00;
      end
      8'b00100101: begin
        out_o    = 8'h30;
        status_o = 5'h01;
      end
      8'b00100110: begin
        out_o    = 8'h31;
        status_o = 5'h01;
      end
      8'b00100111: begin
        out_o    = 8'h31;
        status_o = 5'h01;
      end
      8'b00101000: begin
        out_o    = 8'h32;
        status_o = 5'h01;
      end
      8'b00101001: begin
        out_o    = 8'h32;
        status_o = 5'h01;
      end
      8'b00101010: begin
        out_o    = 8'h33;
        status_o = 5'h01;
      end
      8'b00101011: begin
        out_o    = 8'h33;
        status_o = 5'h01;
      end
      8'b00101100: begin
        out_o    = 8'h34;
        status_o = 5'h00;
      end
      8'b00101101: begin
        out_o    = 8'h34;
        status_o = 5'h01;
      end
      8'b00101110: begin
        out_o    = 8'h35;
        status_o = 5'h01;
      end
      8'b00101111: begin
        out_o    = 8'h35;
        status_o = 5'h01;
      end
      8'b00110000: begin
        out_o    = 8'h36;
        status_o = 5'h01;
      end
      8'b00110001: begin
        out_o    = 8'h36;
        status_o = 5'h01;
      end
      8'b00110010: begin
        out_o    = 8'h37;
        status_o = 5'h01;
      end
      8'b00110011: begin
        out_o    = 8'h37;
        status_o = 5'h01;
      end
      8'b00110100: begin
        out_o    = 8'h38;
        status_o = 5'h00;
      end
      8'b00110101: begin
        out_o    = 8'h38;
        status_o = 5'h01;
      end
      8'b00110110: begin
        out_o    = 8'h39;
        status_o = 5'h01;
      end
      8'b00110111: begin
        out_o    = 8'h39;
        status_o = 5'h01;
      end
      8'b00111000: begin
        out_o    = 8'h3a;
        status_o = 5'h01;
      end
      8'b00111001: begin
        out_o    = 8'h3a;
        status_o = 5'h01;
      end
      8'b00111010: begin
        out_o    = 8'h3b;
        status_o = 5'h01;
      end
      8'b00111011: begin
        out_o    = 8'h3b;
        status_o = 5'h01;
      end
      8'b00111100: begin
        out_o    = 8'h3c;
        status_o = 5'h00;
      end
      8'b00111101: begin
        out_o    = 8'h3c;
        status_o = 5'h01;
      end
      8'b00111110: begin
        out_o    = 8'h3d;
        status_o = 5'h01;
      end
      8'b00111111: begin
        out_o    = 8'h3d;
        status_o = 5'h01;
      end
      8'b01000000: begin
        out_o    = 8'h3e;
        status_o = 5'h01;
      end
      8'b01000001: begin
        out_o    = 8'h3e;
        status_o = 5'h01;
      end
      8'b01000010: begin
        out_o    = 8'h3f;
        status_o = 5'h01;
      end
      8'b01000011: begin
        out_o    = 8'h3f;
        status_o = 5'h01;
      end
      8'b01000100: begin
        out_o    = 8'h40;
        status_o = 5'h00;
      end
      8'b01000101: begin
        out_o    = 8'h40;
        status_o = 5'h01;
      end
      8'b01000110: begin
        out_o    = 8'h41;
        status_o = 5'h01;
      end
      8'b01000111: begin
        out_o    = 8'h41;
        status_o = 5'h01;
      end
      8'b01001000: begin
        out_o    = 8'h42;
        status_o = 5'h01;
      end
      8'b01001001: begin
        out_o    = 8'h42;
        status_o = 5'h01;
      end
      8'b01001010: begin
        out_o    = 8'h43;
        status_o = 5'h01;
      end
      8'b01001011: begin
        out_o    = 8'h43;
        status_o = 5'h01;
      end
      8'b01001100: begin
        out_o    = 8'h44;
        status_o = 5'h00;
      end
      8'b01001101: begin
        out_o    = 8'h44;
        status_o = 5'h01;
      end
      8'b01001110: begin
        out_o    = 8'h45;
        status_o = 5'h01;
      end
      8'b01001111: begin
        out_o    = 8'h45;
        status_o = 5'h01;
      end
      8'b01010000: begin
        out_o    = 8'h46;
        status_o = 5'h01;
      end
      8'b01010001: begin
        out_o    = 8'h46;
        status_o = 5'h01;
      end
      8'b01010010: begin
        out_o    = 8'h47;
        status_o = 5'h01;
      end
      8'b01010011: begin
        out_o    = 8'h47;
        status_o = 5'h01;
      end
      8'b01010100: begin
        out_o    = 8'h48;
        status_o = 5'h00;
      end
      8'b01010101: begin
        out_o    = 8'h48;
        status_o = 5'h01;
      end
      8'b01010110: begin
        out_o    = 8'h49;
        status_o = 5'h01;
      end
      8'b01010111: begin
        out_o    = 8'h49;
        status_o = 5'h01;
      end
      8'b01011000: begin
        out_o    = 8'h4a;
        status_o = 5'h01;
      end
      8'b01011001: begin
        out_o    = 8'h4a;
        status_o = 5'h01;
      end
      8'b01011010: begin
        out_o    = 8'h4b;
        status_o = 5'h01;
      end
      8'b01011011: begin
        out_o    = 8'h4b;
        status_o = 5'h01;
      end
      8'b01011100: begin
        out_o    = 8'h4c;
        status_o = 5'h00;
      end
      8'b01011101: begin
        out_o    = 8'h4c;
        status_o = 5'h01;
      end
      8'b01011110: begin
        out_o    = 8'h4d;
        status_o = 5'h01;
      end
      8'b01011111: begin
        out_o    = 8'h4d;
        status_o = 5'h01;
      end
      8'b01100000: begin
        out_o    = 8'h4e;
        status_o = 5'h01;
      end
      8'b01100001: begin
        out_o    = 8'h4e;
        status_o = 5'h01;
      end
      8'b01100010: begin
        out_o    = 8'h4f;
        status_o = 5'h01;
      end
      8'b01100011: begin
        out_o    = 8'h4f;
        status_o = 5'h01;
      end
      8'b01100100: begin
        out_o    = 8'h50;
        status_o = 5'h00;
      end
      8'b01100101: begin
        out_o    = 8'h50;
        status_o = 5'h01;
      end
      8'b01100110: begin
        out_o    = 8'h51;
        status_o = 5'h01;
      end
      8'b01100111: begin
        out_o    = 8'h51;
        status_o = 5'h01;
      end
      8'b01101000: begin
        out_o    = 8'h52;
        status_o = 5'h01;
      end
      8'b01101001: begin
        out_o    = 8'h52;
        status_o = 5'h01;
      end
      8'b01101010: begin
        out_o    = 8'h53;
        status_o = 5'h01;
      end
      8'b01101011: begin
        out_o    = 8'h53;
        status_o = 5'h01;
      end
      8'b01101100: begin
        out_o    = 8'h54;
        status_o = 5'h00;
      end
      8'b01101101: begin
        out_o    = 8'h54;
        status_o = 5'h01;
      end
      8'b01101110: begin
        out_o    = 8'h55;
        status_o = 5'h01;
      end
      8'b01101111: begin
        out_o    = 8'h55;
        status_o = 5'h01;
      end
      8'b01110000: begin
        out_o    = 8'h56;
        status_o = 5'h01;
      end
      8'b01110001: begin
        out_o    = 8'h56;
        status_o = 5'h01;
      end
      8'b01110010: begin
        out_o    = 8'h57;
        status_o = 5'h01;
      end
      8'b01110011: begin
        out_o    = 8'h57;
        status_o = 5'h01;
      end
      8'b01110100: begin
        out_o    = 8'h58;
        status_o = 5'h00;
      end
      8'b01110101: begin
        out_o    = 8'h58;
        status_o = 5'h01;
      end
      8'b01110110: begin
        out_o    = 8'h59;
        status_o = 5'h01;
      end
      8'b01110111: begin
        out_o    = 8'h59;
        status_o = 5'h01;
      end
      8'b01111000: begin
        out_o    = 8'h5a;
        status_o = 5'h01;
      end
      8'b01111001: begin
        out_o    = 8'h5a;
        status_o = 5'h01;
      end
      8'b01111010: begin
        out_o    = 8'h5b;
        status_o = 5'h01;
      end
      8'b01111011: begin
        out_o    = 8'h5b;
        status_o = 5'h01;
      end
      8'b01111100: begin
        out_o    = 8'h7c;
        status_o = 5'h00;
      end
      8'b01111101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b01111110: begin
        out_o    = 8'h7e;
        status_o = 5'h00;
      end
      8'b01111111: begin
        out_o    = 8'h7e;
        status_o = 5'h00;
      end
      8'b10000000: begin
        out_o    = 8'h80;
        status_o = 5'h00;
      end
      8'b10000001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10000010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10000011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10000100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10000101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10000110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10000111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10001000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10001001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10001010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10001011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10001100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10001101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10001110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10001111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10010000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10010001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10010010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10010011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10010100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10010101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10010110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10010111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10011000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10011001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10011010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10011011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10011100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10011101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10011110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10011111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10100000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10100001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10100010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10100011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10100100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10100101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10100110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10100111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10101000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10101001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10101010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10101011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10101100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10101101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10101110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10101111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10110000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10110001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10110010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10110011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10110100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10110101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10110110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10110111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10111000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10111001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10111010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10111011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10111100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10111101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10111110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b10111111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11000000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11000001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11000010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11000011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11000100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11000101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11000110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11000111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11001000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11001001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11001010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11001011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11001100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11001101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11001110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11001111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11010000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11010001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11010010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11010011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11010100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11010101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11010110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11010111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11011000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11011001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11011010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11011011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11011100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11011101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11011110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11011111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11100000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11100001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11100010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11100011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11100100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11100101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11100110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11100111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11101000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11101001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11101010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11101011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11101100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11101101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11101110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11101111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11110000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11110001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11110010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11110011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11110100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11110101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11110110: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11110111: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11111000: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11111001: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11111010: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11111011: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11111100: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11111101: begin
        out_o    = 8'h7e;
        status_o = 5'h10;
      end
      8'b11111110: begin
        out_o    = 8'h7e;
        status_o = 5'h00;
      end
      8'b11111111: begin
        out_o    = 8'h7e;
        status_o = 5'h00;
      end
      default: begin
        out_o    = 8'h00;
        status_o = 5'h00;
      end
    endcase
  end

endmodule

