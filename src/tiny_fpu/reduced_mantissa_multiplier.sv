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

module reduced_mantissa_multiplier #(
  parameter int unsigned PRECISION_BITS = 24
) (

  input  logic [PRECISION_BITS-1:0]   mantissa_a,
  input  logic [1:0]   mantissa_b,
  output logic [PRECISION_BITS+1:0] product
);

  assign product = mantissa_a * mantissa_b;

endmodule
