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
# File:   f32_add.s
# Author: Matteo Perotti <mperotti@iis.ee.ethz.ch>
# Date:   17.06.2020
#
# Description: RISC-V assembly implementation of single precision soft-float addition
#
#

.global f32_add
.global __clzsi2 # Dependency: __clzsi2

f32_add:
  addi sp,sp,-16
  sw ra,12(sp)
  sw s0,8(sp)
  sw s1,4(sp)
  sw s2,0(sp)
  slli s0, a0, 1                                    # remove signA
  slli s1, a1, 1                                    # remove signB
  lui a5, 0xFF000                                   # load minimum pattern for {inf, NaN}
  bgeu s0, a5, infnan                               # branch if expA == FF -> opA == {inf, NaN}
  bgeu s1, a5, infnan_B_only                        # branch if expB == FF -> opB == {inf, NaN} -> opA != {inf, NaN}
  beq s0, s1, equal                                 # branch if opA == opB
  beqz s0, zero_operand                             # branch to a special case if unsigned opA == 0
  beqz s1, zero_operand                             # branch to a special case if unsigned opB == 0
# The two numbers are normal or denormal
  srli s0, s0, 24                                   # logically isolate expA
  srli s1, s1, 24                                   # logically isolate expB
  sub a6, s1, s0                                    # compute (expB - expA)
  bltu s0, s1, 1f                                   # branch if (expB > expA)
  neg a6, a6                                        # if (expB < expA) compute expA - expB
  j 2f
1:
  mv s0, s1                                         # if (expB > expA) save expB in s0
  mv s1, a0                                         # if (expB > expA) shift opA with opB (0)
  mv a0, a1                                         # if (expB > expA) shift opA with opB (1)
  mv a1, s1                                         # if (expB > expA) shift opA with opB (2)
2:
  sltiu s1, a6, 26                                  # test if the lower is negligible (25 of exp diff is still ok if one of the operands is negative!)
  beqz s1, exit                                     # immediately return the greater number if the lower is negligible
# if we are here, s1 contains 1, a5 contains 0xFF000000
  slli s1, s1, 23                                   # prepare the implicit 1
  or a0, a0, s1                                     # add the implicit 1 to the mantissaA
  or a1, a1, s1                                     # add the implicit 1 to the mantissaB
  not a5, a5                                        # prepare for clearing the sign and exps (1)
  slli s1, s1, 8                                    # prepare the test for the sign
  sltu a7, a0, s1                                   # test if opA is positive (set if positive)
  and a0, a0, a5                                    # clear the sign and exponent of opA
  bnez a7, 3f                                       # jump if opA is positive
  neg a0, a0                                        # if greater number < 0, calculate negate the greater number
3:
  sltu a7, a1, s1                                   # test if opB is positive (set if positive)
  and a1, a1, a5                                    # clear the sign and exponent of opB
  bnez a7, 4f                                       # jump if opB is positive
  neg a1, a1                                        # if lower number < 0, calculate negate the lower number
4:
  beq s0, a6, denormal                              # branch if MAX(exp) == abs(diff(exp)) -> branch if min(exp) == 0 (there is a denormal)
# effective addition. If we are here, s1 = 0x80000000
sum:
  addi s0, s0, -1                                   # subtract one from MAX(exp). This is done becuse the implicit 1 is kept and the exponent added
  sra a5, a1, a6                                    # shift right the lower operand by the exp diff
  add a0, a0, a5                                    # add the two operands (the second is shifted)
  srli s1, s1, 26                                   # prepare the 32
  sub a7, s1, a6                                    # calculates 32-diff(exp)
  sll a1, a1, a7                                    # left shift the lower number by 32-diff(exp)
  bnez a6, fraction_saved                           # check if correct the previous shift, which cannot shift by 32
  mv a1, zero                                       # if the shamt was 32, the fractional part does not exist
fraction_saved:
  slli s1, s1, 26                                   # prepare the mask for the sign
  and s1, s1, a0                                    # isolate the sign of the sum
  bge a0, zero, pos_or_zero                         # if the result is >= 0, no need for c2
  neg a1, a1                                        # if (the result of the addition is negative) 2's complement the "discarded" bits of the sum (check later if there is a carry to propagate to a0!)
  not a0, a0                                        # if (the result of the addition is negative) 1's complement the sum (we will check now if there is the propagating 1 to sum)
  bnez a1, pos_or_zero                              # if there was not a carry from a1 to a0, skip the carry adding
  addi a0, a0, 1                                    # add the carry of the c2 of a1 to a0 only if needed
# the addition gave positive or zero result (or we have in a0 the absolute value of the sum)
pos_or_zero:
  lui a5, 0x00800                                   # prepare to check if we had numerical cancellation
  bltu a0, a5, num_canc                             # branch if we have numerical cancellation?
  slli a5, a5, 1                                    # prepare to check if we need to normalize
  bltu a0, a5, rounding                             # branch if we should not normalize
  andi a5, a0, 1                                    # no numerical cancellation, but we should normalize -> save the G bit
  srli a0, a0, 1                                    # no numerical cancellation, but we should normalize -> right shift the sum by one to normalize
  srli a1, a1, 1                                    # normalization -> right shift also the round bit and the components of the sticky bit
  slli a5, a5, 31                                   # prepare to put the G bit to the left to the round bit
  or a1, a1, a5                                     # put G bit to the left to the round bit
  addi s0, s0, 1                                    # add 1 to MAX(exp)
  addi a5, zero, 254                                # ready to check for ovf
  bge s0, a5, inf                                   # branch if MAX(exp) >= 254 (ovf)
# normalization already performed, no numerical cancellation -> round to nearest even
rounding:
  lui a5, 0x80000                                   # prepare to compare decimal bits to 0.5
  slli s0, s0, 23                                   # bring MAX(exp) to its position in the sum
  add a0, a0, s0                                    # bring MAX(exp) to its position in the sum
  bltu a1, a5, complete_sum                         # check if we can guess to round up (jump away if we do not round)
  addi a0, a0, 1                                    # guess the first rounding (no need for normalization, everything's smooth because we add the exponent)
  bne a1, a5, complete_sum                          # check for a tie -> in the case, round to nearest even (if decimal bits are equal to 0.5, clear the LSB) (we have already added 1 in case of rounding)
  srli a5, a5, 31                                   # prepare to round to nearest even
  not a5, a5                                        # prepare to round to nearest even
  and a0, a0, a5                                    # round to nearest even -> clear the LSB
complete_sum:
  or a0, a0, s1                                     # add the correct sign
exit:
  lw s2,0(sp)
  lw s1,4(sp)
  lw s0,8(sp)
  lw ra,12(sp)
  addi sp,sp,16
  ret                                               # return

# we had numerical cancellation
num_canc:
# a5 keeps 0x00800000
  srli a5, a1, 31                                   # save the round bit before shifting it out
  slli a1, a1, 1                                    # left shift the decimal bits of the sum
  slli a0, a0, 1                                    # left shift the sum and add carry (the round bit)
  add a0, a0, a5                                    # add the eventual round bit
  mv a5, s0                                         # save MAX(exp)
  addi s0, s0, -1                                   # subtract one from MAX(exp)
  beqz a5, denormal_or_strong_cancellation          # jump if (MAX(exp) was 0) -> we have a denormal. Otherwise, maybe we can have soft cancellation with a normal result
  lui a5, 0x00800                                   # prepare condition to check for strong or soft cancellation
  bgeu a0, a5, rounding                             # jump if (MAX(exp) was not 0 && there is no more cancellation) -> soft cancellation -> maybe we need for a rounding
denormal_or_strong_cancellation:
  mv s2, a0                                         # save the sum
  call __clzsi2                                     # count leading zeros of the sum
  mv a5, a0                                         # save clz result
  mv a0, s2                                         # restore the sum
  addi a5, a5, -8                                   # subtract 8 to the number of leading zeros (8 bits of the shifted exponent)
  sub s0, s0, a5                                    # subtract this value to MAX(exp)
  sll a0, a0, a5                                    # left shift the sum by the same amount
  ble s0, zero, denormal_cancellation               # branch if the final exponent is lower than zero (denormal)
  slli s0, s0, 23                                   # the final exponent is higher than zero. Prepare to add it to the sum
  add a0, a0, s0                                    # add the exponent to the sum
  or a0, a0, s1                                     # add the proper sign to the sum
  j exit                                            # return the result
denormal_cancellation:
  neg s0, s0                                        # if the final exponent is lower than zero, c2 of the final exponent to shift-right the result
  srl a0, a0, s0                                    # adjust to the right
  or a0, a0, s1                                     # append the leading sign of the sum
  j exit                                            # return the denormal

# min(exp) == 0 (one of the operands is a denormal)
denormal:
  lui a5, 0x00800                                    # clean the implicit bit of the lower number (it's a denormal)
  xor a1, a1, a5                                    # clean the implicit bit of the lower number (it's a denormal)
  beqz s0, 5f                                       # jump if (MAX(exp) == 0) -> both numbers are denormals
  addi a6, a6, -1                                   # if (expA != 0) -> only the lower operand is denormal, subtract 1 to the exponent difference (if only one is a denormal, the exp difference should be adjusted this way)
5:
  bnez s0, 6f                                       # jump if (MAX(exp) != 0) -> only the lower number is a denormal
  xor a0, a0, a5                                    # if (expA == 0) -> both denormal -> clean the implicit bit of the higher number (if both are denormal, denormalize also the higher number)
  addi s0, s0, 1                                    # if (expA == 0) -> both denormal -> add 1 to the max exponent (if both are denormal we won't have the added implicit one to the exponent)
6:
  j sum                                             # go on with the sum

# Special case: opA == 0 || opB == 0
zero_operand:
  bnez s0, zero_operand_return                      # opA == 0?
  mv   a0, a1                                       # if opA == 0, return opB
zero_operand_return:
  j exit                                            # ret (opA if opB was 0)

# Special case: unsigned opA == unsigned opB
equal:
  beq a0, a1, equal_samesign                        # are the signs equal too?
  mv  a0, zero                                      # if not, the result is zero
  j exit                                            # return if the result was zero
# a5 = 0xFF
equal_samesign:
  and a5, a5, s0                                    # the signs are equal. test unsigned opA (in a5 we have 0xFF)
  bnez a5, equal_nodenormal                         # branch if opA is not a denormal
# opA and opB are both denormal and are equal. s0 contains unsigned 2*opA
  mv a0, s0                                         # replace the return value with 2*opA
  bgeu s0, a1, exit                                 # if opB (i.e. opA) is unsigned-less then its 2 times with no sign, opB (i.e. opA) was positive because bit[30] of opB was 0 before the multiplication. If equal, we had double zero (positive).
  lui a5, 0x80000                                   # prepare to re-set the sign bit if the denormal was negative
  or a0, a0, a5                                     # re-set the sign bit if the denormal was negative
  j exit                                            # return

# Special case: (unsigned opA == unsigned opB) && we have no denormals
equal_nodenormal:
  srli s0, s0, 24                                   # isolate the exponent in a convenient position
  sltiu a5, s0, 0xFE                                # test if we can multiply the number by 2 with no ovf (check if the exponent is < 254)
  beqz a5, 7f                                       # branch if we will overflow
# a5 is 0
  lui a5, 0x00800                                   # if we can multiply by 2 with no ovf, double the result
  add a0, a0, a5                                    # if we can multiply by 2 with no ovf, double the result
  j exit                                            # return 2*opA if we do not ovf
7:
# a5 is 1
  slli a5, a5, 31                                   # we will ovf: save the sign of opA
  and s1, a5, a0                                    # we will ovf: save the sign of opA in s1
# prepare the infinite
inf:
  lui a0, 0x7F800                                   # load unsigned infinite in the result
  or a0, a0, s1                                     # update the sign
  j exit                                            # return the correct infinite

# Special case: (opB == {inf, NaN} && opA != {inf, NaN})
infnan_B_only:
  mv a0, a1                                         # (opB == {inf, NaN} && opA != {inf, NaN}) -> put opB also in a0 (now the operation is between {inf, NaN})
  mv s0, s1                                         # (opB == {inf, NaN} && opA != {inf, NaN}) -> put |opB|<<1 also in s0 (because we will check s0)
  j infnan_end                                      # jump -> (opB == {inf, NaN} && opA != {inf, NaN})
# Special case: opA == {inf, NaN} and maybe also opB == {inf, NaN}
infnan:
# a5 = 0xFF000
  bgeu s1, a5, infnan_end                           # (opB == {inf, NaN} && opA == {inf, NaN}) -> jump
  mv a1, a0                                         # (opB != {inf, NaN} && opA == {inf, NaN}) -> (now the operation is between {inf, NaN})
# We have in a0 and a1 two {inf, NaN} values -> process them
infnan_end:
  slli s0, s0, 8                                    # prepare to check if the first value is a NaN
  bnez s0, produce_nan                              # if the first value is a NaN -> return a qNaN
  bne a0, a1, produce_nan                           # the first value is inf -> if the operands are different -> second value is either a NaN or an opposite inf -> return a qNaN
  j exit                                            # return the correct inf
produce_nan:
# a5 = 0xFF000
  srli a5, a5, 2                                    # dirty way of returning quiet NaNs
  or a0, a0, a5                                     # if operands are (+inf) + (-inf) || if opA == NaN || opB == NaN -> return quiet NaN
  j exit                                            # return quiet NaN
