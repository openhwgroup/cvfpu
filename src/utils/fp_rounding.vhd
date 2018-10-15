-------------------------------------------------------------------------------
-- Title      : Floating-Point Rounding Module
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_rounding.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-21
-- Last update: 2018-03-23
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Performs IEEE 754-compliant rounding for RISC-V rounding modes.
-------------------------------------------------------------------------------
-- Copyright 2018 ETH Zurich and University of Bologna.
-- Copyright and related rights are licensed under the Solderpad Hardware
-- License, Version 0.51 (the "License"); you may not use this file except in
-- compliance with the License.  You may obtain a copy of the License at
-- http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
-- or agreed to in writing, software, hardware and materials distributed under
-- this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
-- CONDITIONS OF ANY KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations under the License.
-------------------------------------------------------------------------------

library IEEE, work;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.fpnew_pkg.all;


--! @brief Floating-Point Rounding Module
--! @details Performs IEEE 754-compliant rounding for RISC-V rounding modes
--! after floating-point operations.
entity fp_rounding is

  generic (
    EXP_BITS : positive := 5;
    MAN_BITS : natural := 10);

  port (
    ResultAbs_DI     : in  std_logic_vector(EXP_BITS+MAN_BITS-1 downto 0);
    ResultSign_DI    : in  std_logic;
    RoundSticky_SI   : in  std_logic_vector(1 downto 0);
    RoundMode_SI     : in  rvRoundingMode_t;
    OFBeforeRnd_SI   : in  boolean;
    ResZero_SI       : in  boolean;
    EffSub_SI        : in  boolean;
    ---------------------------------------------------------------------------
    RoundedResult_DO : out std_logic_vector(EXP_BITS+MAN_BITS downto 0));

end entity fp_rounding;



--! @brief Architecture using conditional incrementation
--! @details Takes the rounding decision and then increments the result if
--! needed
architecture condincr of fp_rounding is

  alias ResSign_DI : std_logic is ResultSign_DI;

  -- Internal pre-round signals after possible change for overflow
  signal ResAbs_D      : std_logic_vector(ResultAbs_DI'range);
  signal RoundSticky_S : std_logic_vector(RoundSticky_SI'range);

  -- Rounding decision
  signal RndUp_S : std_logic;

  -- Rounded result
  signal ResSignRnd_D : std_logic;
  signal ResAbsRnd_D  : std_logic_vector(ResAbs_D'range);

begin  -- architecture condincr

  -- If we overflowed, set the preround result to the largest normal and set
  -- RND and Sticky bits to ensure proper IEEE rounding of overflow. Otherwise
  -- strip the sign bit off for the absolute value
  ResAbs_D <= MAXNORMAL(EXP_BITS, MAN_BITS) when OFBeforeRnd_SI else
              ResultAbs_DI;
  RoundSticky_S <= "11" when OFBeforeRnd_SI else
                   RoundSticky_SI;

  -- Take the rounding decision according to RISC-V spec ([1] p.48)
  -- RoundMode | Mnemonic | Meaning
  -- :--------:|:--------:|:-------
  --    000    |   RNE    | Round to Nearest, ties to Even
  --    001    |   RTZ    | Round towards Zero
  --    010    |   RDN    | Round Down (towards -\infty)
  --    011    |   RUP    | Round Up (towards \infty)
  --    100    |   RMM    | Round to Nearest, ties to Max Magnitude
  --  others   |          | *invalid*
  -----------------------------------------------------------------------------
  -- Hint: conditional signal assignment evaluates to first true condition
  RndUp_S <=
    -- Exact result needs no rounding no matter the rounding mode
    '0' when RoundSticky_S = "00" else

    -- RNE rounds towards zero if <ulp/2 away
    '0'         when (RoundMode_SI = RNE and RoundSticky_S = "01") else
    -- RNE rounds towards even result when ulp/2 away
    ResAbs_D(0) when (RoundMode_SI = RNE and RoundSticky_S = "10") else
    -- RNE rounds away from zero if >ulp/2 away
    '1'         when RoundMode_SI = RNE else

    -- RTZ always rounds towards zero
    '0' when RoundMode_SI = RTZ else

    -- RDN rounds towards zero if positive and away from zero if negative
    ResSign_DI when RoundMode_SI = RDN else

    -- RUP rounds away from zero if positive and towards zero if negative
    (not ResSign_DI) when RoundMode_SI = RUP else

    -- RMM rounds down if <ulp/2 away
    '0' when (RoundMode_SI = RMM and RoundSticky_S = "01") else
    -- RMM rounds up (max magnitude) when >=ulp/2 away
    '1' when RoundMode_SI = RMM else

    -- All other cases are illegal and we can optimize away
    '-';

  -- Change the final sign in case of true zero result rounding
  ResSignRnd_D <= '1' when (ResZero_SI and EffSub_SI and RoundMode_SI = RDN) else
                  '0' when (ResZero_SI and EffSub_SI) else
                  ResSign_DI;


  -- Do the actual rounding, exponent change will automagically happen
  ResAbsRnd_D <= std_logic_vector(unsigned(ResAbs_D)
                                  + resize(unsigned'(""&RndUp_S), ResAbsRnd_D'length));

  -- Stitch the result together. OF after round case is handled correctly by
  -- construction (largest normal rounds to inf)
  RoundedResult_DO <= ResSignRnd_D & ResAbsRnd_D;


end architecture condincr;
