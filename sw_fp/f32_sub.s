# Copyright 2020 ETH Zurich
# Copyright and related rights are licensed under the Apache
# License, Version 2.0; you may not use this file except in
# compliance with the License.  You may obtain a copy of the License at
# https://www.apache.org/licenses/LICENSE-2.0. Unless required by applicable law
# or agreed to in writing, software, hardware and materials distributed under
# this License is distributed on an "AS IS"?" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied. See the License for the
# specific language governing permissions and limitations under the License.
#
# File:   f32_sub.s
# Author: Matteo Perotti <mperotti@iis.ee.ethz.ch>
# Date:   17.06.2020
#
# Description: RISC-V assembly implementation of single precision soft-float subtraction
#
#

.global f32_sub
.global f32_add # Dependency: f32_add

f32_sub:
  lui a5, 0x80000                                   # change sign
  xor a1, a1, a5                                    # change sign of opB
  j f32_add                                         # jump to fadd
