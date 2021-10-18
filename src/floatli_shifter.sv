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

// Authors: Luca Bertaccini <lbertaccini@iis.ee.ethz.ch>, Stefan Mach <smach@iis.ee.ethz.ch>

module floatli_shifter #(
  parameter int unsigned PRECISION_BITS      = 24,
  parameter int unsigned SHIFT_AMOUNT_WIDTH  = 7
) (
  input  logic [(3*PRECISION_BITS+4)/3:0]   sum,       // discard carry as sum won't overflow
  input  logic [SHIFT_AMOUNT_WIDTH-1:0] norm_shamt,

  output logic [4*PRECISION_BITS+3:0]   sum_shifted
);

  // Do the large normalization shift
  assign sum_shifted       = sum << norm_shamt;

endmodule
