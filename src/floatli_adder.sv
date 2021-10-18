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

module floatli_adder #(
  parameter int unsigned PRECISION_BITS = 24
) (
  input  logic [(3*PRECISION_BITS+4)/3:0] product_shifted, //p+2
  input  logic [(3*PRECISION_BITS+4)/3:0] addend_shifted, //p+2
  input  logic                              inject_carry_in,

  output logic [(3*PRECISION_BITS+4)/3+1:0]   sum_raw       // discard carry as sum won't overflow
);

  //Mantissa adder (ab+c). In normal addition, it cannot overflow.
  assign sum_raw = product_shifted + addend_shifted + inject_carry_in; //p+3

endmodule
