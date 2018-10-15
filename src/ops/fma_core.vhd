-------------------------------------------------------------------------------
-- Title      : Fused Multiply-Add Unit Core for Generic Float Formats
-- Project    :
-------------------------------------------------------------------------------
-- File       : fma_core.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-02-14
-- Last update: 2018-03-25
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
-- [1]: Handbook of Floating-Point Arithmetic
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
use work.fpnew_comps_pkg.all;


entity fma_core is

  generic (
    EXP_BITS : natural := 5;
    MAN_BITS : natural := 10);

  port (
    SignA_DI, SignB_DI, SignC_DI             : in std_logic;  -- Sign bits
    ExpA_DI, ExpB_DI, ExpC_DI                : in unsigned(EXP_BITS-1 downto 0);  -- biased
    MantA_DI, MantB_DI, MantC_DI             : in std_logic_vector(MAN_BITS-1 downto 0);
    IsNormalA_DI, IsNormalB_DI, IsNormalC_DI : in std_logic;  -- implicit bits
    IsZeroA_SI, IsZeroB_SI                   : in boolean;

    ResSign_DO : out std_logic;
    ResExp_DO  : out unsigned(FMAEXPWIDTH(EXP_BITS, MAN_BITS)-1 downto 0);
    ResMant_DO : out std_logic_vector(MAN_BITS + 2 downto 0));

end entity fma_core;


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Single-Path FMA architecture
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
architecture single_path of fma_core is


  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------
  -- Bias is symmetric (IEEE style)
  constant BIAS : natural := 2**(EXP_BITS-1)-1;

  -- Exponent field all ones
  constant MAXEXP : natural := 2**(EXP_BITS)-1;

  -- Number of Precision bits including implicit bit, referred to as 'p'
  constant PREC_BITS : natural := MAN_BITS+1;

  -- Lower 2p+3 bits of added result are needed for leading-zero detection
  constant LOWSUMWIDTH : natural := 2*PREC_BITS+3;
  -- Leading-zero count of this part will be clog2 wide
  constant LZCWIDTH    : natural := clog2(LOWSUMWIDTH);

  -- To eliminate false datapath leakage in the exponent datapath, we use the
  -- maximum of the LZC width and EXP_BITS+2 (which is enough to hold all
  -- meaningful values) for the internal exponent widths. Note that for most
  -- reasonable FP formats, EXP_BITS+2 will be wider than LZCWIDTH.
  constant EXPWIDTH : natural := FMAEXPWIDTH(EXP_BITS,MAN_BITS);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------
  -- Input Exponents in signed form
  signal ExpA_D, ExpB_D, ExpC_D : signed(EXPWIDTH-1 downto 0);

  -- Internal exponents need 2 extra bits for handling sign and over/underflow
  signal ExpAdd_D             : signed(EXPWIDTH-1 downto 0);
  signal ExpProd_D, ExpTent_D : signed(EXPWIDTH-1 downto 0);

  -- Exponent difference for controlling muxes and shifters
  signal ExpDiff_S : integer range 1-(2*MAXEXP-BIAS) to MAXEXP+BIAS-2;

  -- Shift amounts. Maximum size is 3p+5. Using constrained integral types is
  -- ignored in synthesis but the tools are smart enough to see how many bits
  -- are really needed for the shifters.
  signal AddShamt_S : unsigned(clog2(3*PREC_BITS+5)-1 downto 0);
  signal SumShamt_S : unsigned(clog2(3*PREC_BITS+5)-1 downto 0);
--   signal AddShamt_S : natural range 0 to 3*PREC_BITS+5;
--   signal SumShamt_S : natural range 0 to 3*PREC_BITS+5;

  -- Mantissae with implicit bit and their product, sign
  signal MantA_D, MantB_D, MantC_D : unsigned(PREC_BITS-1 downto 0);
  signal Prod_D                    : unsigned(2*PREC_BITS-1 downto 0);
  signal SignProd_D                : std_logic;

  -- Product is constant shifted into a 3p+5 operand, added carry for addition
  signal ProdShift_D : unsigned(3*PREC_BITS+5 downto 0);  -- 3p+6 bit

  -- Tentative sign and effective subtraction
  signal SignTent_D  : std_logic;
  signal EffSub_S    : boolean;
  signal SignFinal_D : std_logic;

  -- C (width p) is right shifted up to 3p+4 places, requiring a 4p+4 container
  -- with one extra carry bit for the addition later
  signal MantCExt_D   : unsigned(4*PREC_BITS+4 downto 0);  -- 4p+5 bit
  signal MantCShift_D : unsigned(4*PREC_BITS+4 downto 0);  -- 4p+5 bit
  -- First 3p+5 bits and carry go towards adder, the rest is used for sticky
  alias AddendRaw_D   : unsigned(3*PREC_BITS+5 downto 0) is MantCShift_D(MantCShift_D'high downto PREC_BITS-1);  --3p+6 bit
  alias AddShiftOut_D : unsigned(PREC_BITS-2 downto 0) is MantCShift_D(PREC_BITS-2 downto 0);  -- p-1 bit
--   signal AddendRaw_D   : unsigned(3*PREC_BITS+5 downto 0);
--   signal AddShiftOut_D : unsigned(PREC_BITS-1 downto 0);

  -- The actual addend for the addition, with 2 extra carry bits (leakage)
  signal Addend_D : unsigned(3*PREC_BITS+6 downto 0);

  -- Sticky bit from right shifting
  signal AddShiftSticky_D : std_logic;

  -- Sum of product and addend is again 3p+5 wide, with 2 carry-out for
  -- inversion (leakage)
  signal SumRaw_D   : unsigned(3*PREC_BITS+6 downto 0);
  alias SumCarry_D  : std_logic is SumRaw_D(SumRaw_D'high);
  -- Sum keeps 1 carry in case sum starts with 2.xxx
  alias SumPreInv_D : unsigned(3*PREC_BITS+5 downto 0) is SumRaw_D(3*PREC_BITS+5 downto 0);

  -- Sum keeps 1 carry in case sum starts with 2.xxx
  signal Sum_D : unsigned(3*PREC_BITS+5 downto 0);

  -- The lower 2p+3 bits of the sum are needed for realignment
  alias SumLower_D : unsigned(LOWSUMWIDTH-1 downto 0) is Sum_D(LOWSUMWIDTH-1 downto 0);

  -- Leading zero count after addition
  signal LZCount_S : unsigned(LZCWIDTH-1 downto 0);
  signal LZNoOne_S : boolean;

  -- Exponent after left shift
  signal ExpSum_D   : unsigned(EXPWIDTH-1 downto 0);
  signal ExpFinal_D : unsigned(EXPWIDTH-1 downto 0);

  -- Sum after left shift
  signal SumShift_D, SumShiftFinal_D : unsigned(Sum_D'range);

  -- First p+1 bits of sum after the carry are used for the result, rest is used for sticky
  alias ResMantPreRnd_D : unsigned(PREC_BITS downto 0) is
    SumShiftFinal_D(SumShift_D'high-1 downto SumShift_D'high-1-PREC_BITS);

  alias SumShiftOut_D : unsigned(SumShift_D'high-1-PREC_BITS-1 downto 0) is
    SumShiftFinal_D(SumShift_D'high-1-PREC_BITS-1 downto 0);

  signal SumShiftSticky_D : std_logic;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Exponent Datapath and Control Signal Generation
  -----------------------------------------------------------------------------
  -- Cast Input exponents to signed to avoid mixed type expressions later
  ExpA_D <= resize(signed("0"&ExpA_DI), EXPWIDTH);
  ExpB_D <= resize(signed("0"&ExpB_DI), EXPWIDTH);
  ExpC_D <= resize(signed("0"&ExpC_DI), EXPWIDTH);

  -- Real exponents are (ex = Ex - bias + 1 - nx) with Ex the encoded exponent
  -- and nx the implicit bit. We're biasing internally
  ExpAdd_D <= ExpC_D + 1 - to_integer(IsNormalC_DI);

  -- Biased product exponent is the sum of encoded exponents minus the bias
  -- in case of zero product, be sure to correct exponent
  ExpProd_D <= to_signed(2 - BIAS, ExpProd_D'length) when (IsZeroA_SI or IsZeroB_SI) else
               (ExpA_D + 1 - to_integer(IsNormalA_DI)
                + ExpB_D + 1 - to_integer(IsNormalB_DI)
                - BIAS);

  -- Exponent difference is the addend exponent minus the product exponent
  -- (Ec + 1 - nc) - (Ea + 1 - na) - (Eb + 1 - nb) + bias
  ExpDiff_S <= to_integer(ExpAdd_D - ExpProd_D);

  -- Sign of product is xor of signs
  SignProd_D <= SignA_DI xor SignB_DI;

  -- Assume product sign will be final sign
  SignTent_D <= SignProd_D;

  -- Effective Subtraction is xor of signs
  EffSub_S <= SignC_DI /= SignProd_D;   -- xor

  -- Combinatorial process for shift amounts and resulting exponents
  -- see [1] p.261f.
  p_shiftAmounts : process(all)
  begin

    -- Addend (right) shifter
    if (ExpDiff_S <= (-2*PREC_BITS + 1)) then
      AddShamt_S <= to_unsigned(3*PREC_BITS + 3, AddShamt_S'length);
    elsif (ExpDiff_S <= (PREC_BITS + 2)) then
      AddShamt_S <= to_unsigned(PREC_BITS + 4 - ExpDiff_S, AddShamt_S'length);
    else
      AddShamt_S <= (others  => '0');
    end if;

    -- Tentative exponent after addend shifter
    if (ExpDiff_S <= 0) then
      ExpTent_D <= ExpProd_D;
    else
      ExpTent_D <= ExpAdd_D;
    end if;

    -- Final sum (left) shifter and exponent after sum shift
    -- Product-anchored or cancellation
    if (ExpDiff_S <= 0 or (EffSub_S and ExpDiff_S <= 2)) then
      -- Normal result (1 is smallest normal encoded exponent)
      if ((ExpProd_D - to_integer(LZCount_S) + 2 >= 1)
          and not LZNoOne_S)
      then
        SumShamt_S <= PREC_BITS + 2 + resize(LZCount_S, SumShamt_S'length);
        ExpSum_D   <= unsigned(ExpProd_D) - resize(LZCount_S, ExpProd_D'length) + 2;
      -- Subnormal result
      else
        SumShamt_S <= unsigned(PREC_BITS + 3 + resize(ExpProd_D,SumShamt_S'length));  -- p+4-emin-eprod
        ExpSum_D   <= (others => '0');
      end if;
    -- Addend-anchored
    else
      SumShamt_S <= AddShamt_S;
      ExpSum_D   <= unsigned(ExpTent_D);
    end if;

  end process;  -- p_shamtGen

  -----------------------------------------------------------------------------
  -- Mantissa Datapath
  -----------------------------------------------------------------------------

  -- Add the implicit bits to the mantissae
  MantA_D <= unsigned(IsNormalA_DI & MantA_DI);
  MantB_D <= unsigned(IsNormalB_DI & MantB_DI);
  MantC_D <= unsigned(IsNormalC_DI & MantC_DI);


  -- Mantissa Multiplier (A*B)
  Prod_D <= MantA_D * MantB_D;


  -- Product is constant-shifted into a 3p+5 vector plus carry as such:
  -- |0| 000...000 | Prod_D | 00 |
  --  C <-  p+3  -> <- 2p -> < 2>
  ProdShift_D <= resize(Prod_D, ProdShift_D'length) sll 2;

  -- Addend is placed to the left of 4p+4 word plus carry and right shifted up to 3p+4
  -- BEFORE THE SHIFT:
  -- |0| MantC_D | 000..000 | >> AddShamt_S
  --  C <-  p  -> <- 3p+4 ->
  MantCExt_D   <= resize(MantC_D, MantCShift_D'length) sll 3*PREC_BITS+4;
  -- AFTER THE SHIFT:
  -- |0| 000........000 | MantC_D | 000.............000 |
  --  C <- AddShamt_S -> <-  p  -> <- 3p+4-AddShamt_S ->
  -- (AddendRaw_D, AddShiftOut_D) <=  MantCExt_D srl AddShamt_S; -- not
  -- supported by synopsys
  MantCShift_D <= MantCExt_D srl to_integer(AddShamt_S);

  -- Sticky from shifting addend
  AddShiftSticky_D <= or_reduce(std_logic_vector(AddShiftOut_D));

  -- Invert addend in case of effective subtraction
  Addend_D <= not resize(AddendRaw_D, Addend_D'length) when EffSub_S and AddShiftSticky_D = '1' else
              0-resize(AddendRaw_D, Addend_D'length) when EffSub_S else
              resize(AddendRaw_D, Addend_D'length);

  -- Mantissa adder
  SumRaw_D <= Addend_D+ProdShift_D;

  -- Complement negative sum (subtraction overflows if result is positive)
  Sum_D <= 0-SumPreInv_D when (EffSub_S and SumCarry_D = '1') else
           SumPreInv_D;
  -- Invert the sign if we're subtracting and overflow
  SignFinal_D <= '1' when (EffSub_S and SumCarry_D /= SignTent_D) else
                 '0' when EffSub_S else
                 SignTent_D;

  -- Leading Zero Counter
  i_lzc : find_first_one
    generic map (
      WIDTH => SumLower_D'length,
      FLIP => 1)

    port map (
      in_i        => std_logic_vector(SumLower_D),
      first_one_o => LZCount_S,
      no_ones_o   => LZNoOne_S);

  -- Sum is left-shifted and first p+1 bits are relevant for the result
  SumShift_D <= Sum_D sll to_integer(SumShamt_S);

  -- 2-bit renormalization if addend-anchored
  p_2bitnorm : process (all) is
  begin  -- process p_1bitnorm

    -- default bindings
    SumShiftFinal_D <= SumShift_D;
    ExpFinal_D      <= ExpSum_D;

    -- Handle Addend-Anchored case where we could be denormalized but not
    -- subnormal. TODO see if we need to check MSB on denormal C.
    if (ExpDiff_S > 2 or (ExpDiff_S > -2 and not EffSub_S)) then

      -- Sum has overflown, align right and fix exp
      if SumShift_D(SumShift_D'LEFT) = '1' then
        SumShiftFinal_D <= SumShift_D srl 1;
        ExpFinal_D      <= ExpSum_D + 1;

      -- nothing to do in normalized case
      elsif SumShift_D(SumShift_D'LEFT-1) = '1' then
        null;

      -- unless we're denormal, align left
      elsif ExpSum_D > 1 then
        SumShiftFinal_D <= SumShift_D sll 1;
        ExpFinal_D      <= ExpSum_D - 1;

      -- Otherwise we're denormal
      else
        ExpFinal_D <= (others => '0');
      end if;

    end if;

  end process p_2bitnorm;


  -- Update sticky with shifted-out bits
  SumShiftSticky_D <= or_reduce(std_logic_vector(SumShiftOut_D)) or AddShiftSticky_D;

  -----------------------------------------------------------------------------
  -- Output
  -----------------------------------------------------------------------------
  ResSign_DO <= SignFinal_D;
  ResExp_DO  <= ExpFinal_D;
  ResMant_DO <= std_logic_vector(ResMantPreRnd_D & SumShiftSticky_D);


end architecture single_path;
