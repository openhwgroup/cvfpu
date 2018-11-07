-------------------------------------------------------------------------------
-- Title      : Floating-Point Fused Multiply-Add Unit
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_fma.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-02-14
-- Last update: 2018-04-18
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Parametric floating-point fused multiply-add unit
--              Supported operations from fpnew_pkg.fpOp_t:
--              - FMADD
--              - FNMSUB
--              - ADD
--              - MUL
--
--              [1]: "The RISC-V Instruction Set Manual, Volume I: User-Level
--                   ISA, Document Version 2.2", Editors Andrew Waterman and
--                   Krste Asanović, RISC-V Foundation, May 2017
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

--! @brief Floating-Point Fused Multiply-Add Unit
--! @details IEEE 754-compliant, parametric floating-point fused multiply-add
--! unit.
--! Supported operations from fpnew_pkg.fpOp_t:
--! - FMADD
--! - FNMSUB
--! - ADD
--! - MUL
entity fp_fma is

  generic (
    EXP_BITS  : natural := 5;
    MAN_BITS  : natural := 10;
    LATENCY   : natural := 0;
    TAG_WIDTH : natural := 0);

  port (
    Clk_CI                    : in  std_logic;
    Reset_RBI                 : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI, B_DI, C_DI          : in  std_logic_vector(EXP_BITS+MAN_BITS downto 0);
    ABox_SI, BBox_SI, CBox_SI : in  std_logic;
    RoundMode_SI              : in  rvRoundingMode_t;
    Op_SI                     : in  fpOp_t;
    OpMod_SI                  : in  std_logic;
    Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI                : in  std_logic;
    InReady_SO                : out std_logic;
    Flush_SI                  : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO                      : out std_logic_vector(EXP_BITS+MAN_BITS downto 0);
    Status_DO                 : out rvStatus_t;
    Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    OutValid_SO               : out std_logic;
    OutReady_SI               : in  std_logic);

end entity fp_fma;



architecture rtl of fp_fma is

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------
  constant MAXEXP : unsigned(EXP_BITS-1 downto 0) := MAXEXP(EXP_BITS);

  constant WIDTH : natural := EXP_BITS+MAN_BITS+1;

  -- To eliminate false datapath leakage in the exponent datapath, we use the
  -- maximum of the LZC width and EXP_BITS+2 (which is enough to hold all
  -- meaningful values) for the internal exponent widths. Note that for most
  -- reasonable FP formats, EXP_BITS+2 will be wider than LZCWIDTH.
  constant EXPWIDTH : natural := FMAEXPWIDTH(EXP_BITS, MAN_BITS);

  -- The quiet bit index is the topmost bit of the mantissa of a NaN value
  constant QUIETBIT : natural := MAN_BITS-1;

  -- Bit-Patterns of special values, only read from these!
  signal INFEXP    : std_logic_vector(EXP_BITS-1 downto 0);
  signal INFMANT   : std_logic_vector(MAN_BITS-1 downto 0);
  signal DENORMEXP : std_logic_vector(EXP_BITS-1 downto 0);
  -- The largest normal FP value, without sign bit
  signal MAXNORMAL : std_logic_vector(EXP_BITS+MAN_BITS-1 downto 0);


  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Provide aliased signal names for parts of input FP numbers
  alias SignA_DI : std_logic is A_DI(A_DI'high);
  alias SignC_DI : std_logic is C_DI(C_DI'high);

  alias AbsA_DI : std_logic_vector(EXP_BITS+MAN_BITS-1 downto 0)
    is A_DI(EXP_BITS+MAN_BITS-1 downto 0);
  alias AbsC_DI : std_logic_vector(EXP_BITS+MAN_BITS-1 downto 0)
    is C_DI(EXP_BITS+MAN_BITS-1 downto 0);

  -- Input signals after potential altering by Op_SI
  signal A_D : std_logic_vector(A_DI'range);
  signal B_D : std_logic_vector(B_DI'range);
  signal C_D : std_logic_vector(C_DI'range);

  -- Provide aliased signal names for parts of operands
  alias SignA_D : std_logic is A_D(A_D'high);
  alias SignB_D : std_logic is B_D(B_D'high);
  alias SignC_D : std_logic is C_D(C_D'high);

  alias ExpA_D : std_logic_vector(EXP_BITS-1 downto 0)
    is A_D(EXP_BITS+MAN_BITS-1 downto MAN_BITS);
  alias ExpB_D : std_logic_vector(EXP_BITS-1 downto 0)
    is B_D(EXP_BITS+MAN_BITS-1 downto MAN_BITS);
  alias ExpC_D : std_logic_vector(EXP_BITS-1 downto 0)
    is C_D(EXP_BITS+MAN_BITS-1 downto MAN_BITS);

  alias MantA_D : std_logic_vector(MAN_BITS-1 downto 0)
    is A_D(MAN_BITS-1 downto 0);
  alias MantB_D : std_logic_vector(MAN_BITS-1 downto 0)
    is B_D(MAN_BITS-1 downto 0);
  alias MantC_D : std_logic_vector(MAN_BITS-1 downto 0)
    is C_D(MAN_BITS-1 downto 0);

  -- Input NaN-Boxing
  signal ABox_S, BBox_S, CBox_S : std_logic;

  -- FP classification signals
  signal IsNormalA_D, IsNormalB_D, IsNormalC_D : std_logic;
  signal IsInfA_S, IsInfB_S, IsInfC_S          : boolean;
  signal IsNaNA_S, IsNaNB_S, IsNaNC_S          : boolean;
  signal IsZeroA_S, IsZeroB_S, IsZeroC_S       : boolean;

  signal SignalingNaN_S : boolean;
  signal InputNaN_S     : boolean;
  signal InputInf_S     : boolean;

  -- RISC-V FP FLAGS
  signal StatusFlags_D, SpecialFlags_D : rvStatus_t;

  -- Special case control flow
  signal SpecialCase_S : boolean;  -- FMA core not needed for special cases
  signal EffSub_S      : boolean;

  -- Special result for special case handling
  signal SpecialResult_D : std_logic_vector(Z_DO'range);
  alias SpecialResSign_D : std_logic
    is SpecialResult_D(SpecialResult_D'high);
  alias SpecialResMant_D : std_logic_vector(MAN_BITS-1 downto 0)
    is SpecialResult_D(MAN_BITS-1 downto 0);
  alias SpecialResExp_D : std_logic_vector(EXP_BITS-1 downto 0)
    is SpecialResult_D(EXP_BITS+MAN_BITS-1 downto MAN_BITS);


  -- Regular result fma_core outputs
  signal ResSign_D       : std_logic;
  signal ResMantPreRnd_D : std_logic_vector(MAN_BITS+2 downto 0);
  alias ResRndSticky_S   : std_logic_vector(1 downto 0)
    is ResMantPreRnd_D(1 downto 0);
  signal ResExpPreRnd_D : unsigned(EXPWIDTH-1 downto 0);
  signal ResAbsPreRnd_D : std_logic_vector(EXP_BITS+MAN_BITS-1 downto 0);

  -- Output classification and rounding
  signal UFBeforeRnd_S, OFBeforeRnd_S   : boolean;
  signal OFAfterRound_S, UFAfterRound_S : boolean;
  signal RndUp_S                        : std_logic;
  signal ResZero_S                      : boolean;

  -- Regular result after rounding
  signal RegularResult_D : std_logic_vector(Z_DO'range);
  alias RegularResSign_D : std_logic
    is RegularResult_D(RegularResult_D'high);
  alias RegularResMant_D : std_logic_vector(MAN_BITS-1 downto 0)
    is RegularResult_D(MAN_BITS-1 downto 0);
  alias RegularResExp_D : std_logic_vector(EXP_BITS-1 downto 0)
    is RegularResult_D(EXP_BITS+MAN_BITS-1 downto MAN_BITS);


  -- Final result (pre-pipelining)
  signal Result_D : std_logic_vector(Z_DO'range);
  signal Status_D : rvStatus_t;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Special Value Constant Signals
  -----------------------------------------------------------------------------

  -- Infinity and NaN constants
  INFEXP    <= (others => '1');
  INFMANT   <= (others => '0');
  DENORMEXP <= (others => '0');

  -- Largest normal has an exponent of MAXEXP-1 and all ones mantissa
  MAXNORMAL(EXP_BITS+MAN_BITS-1 downto MAN_BITS)
    <= std_logic_vector(MAXEXP-1);
  MAXNORMAL(MAN_BITS-1 downto 0) <= (others => '1');


  -----------------------------------------------------------------------------
  -- Operation Select
  -----------------------------------------------------------------------------

  --! @brief Operation selection and operand adjustment
  --! @details Alters input values according to the selected operation:
  --! | \c Op_SI | \c OpMod_SI | Operation: Adjustment
  --! |:--------:|:-----------:|-------
  --! | FMADD    | \c 0        | FMADD: none
  --! | FMADD    | \c 1        | FMSUB: Invert sign of operand C
  --! | FNMSUB   | \c 0        | FNMSUB: Invert sign of operand A
  --! | FNMSUB   | \c 1        | FNMADD: Invert sign of operands A and C
  --! | ADD      | \c 0        | ADD: Set operand A to +1.0
  --! | ADD      | \c 1        | SUB: Set operand A to +1.0, invert sign of operand C
  --! | MUL      | \c 0        | MUL: Set operand C to +0.0
  --! | *others* | \c -        | *invalid*
  --! \note \c OpMod_SI always inverts the sign of the addend.
  p_opSel : process (all) is

  begin  -- process p_opS

    -- Default assignments: Leave inputs as they are
    A_D <= A_DI;
    B_D <= B_DI;
    C_D <= C_DI;

    ABox_S <= ABox_SI;
    BBox_S <= BBox_SI;
    CBox_S <= CBox_SI;

    -- OpMod_SI inverts sign of operand C
    if OpMod_SI = '1' then
      C_D <= (not SignC_DI) & AbsC_DI;
    end if;

    case (Op_SI) is
      -- FMADD, FMSUB
      when (FMADD) =>
        -- nothing to do
        null;

      -- FNMSUB, FNMADD
      when (FNMSUB) =>
        -- invert sign of product (through the multiplicand for example)
        A_D <= (not SignA_DI) & AbsA_DI;

      -- ADD, SUB
      when (ADD) =>
        -- set multiplicand to positive one
        A_D    <= ONE(EXP_BITS, MAN_BITS);
        ABox_S <= '1';

      -- MUL
      when (MUL) =>
        -- set addend to negative zero
        C_D    <= NEGZERO(EXP_BITS, MAN_BITS);
        CBox_S <= '1';

      -- Unused operations -> OPTIMIZE AWAY
      when others =>
        A_D <= (others => '-');
        B_D <= (others => '-');
        C_D <= (others => '-');

        ABox_S <= '-';
        BBox_S <= '-';
        CBox_S <= '-';

    end case;

  end process p_opSel;


  -----------------------------------------------------------------------------
  -- Input Classification
  -----------------------------------------------------------------------------

  -- Normal if non-zero exponents
  IsNormalA_D <= '1' when unsigned(ExpA_D) /= 0 else '0';
  IsNormalB_D <= '1' when unsigned(ExpB_D) /= 0 else '0';
  IsNormalC_D <= '1' when unsigned(ExpC_D) /= 0 else '0';

  -- Infinities have all-ones exponents and zero mantissa
  IsInfA_S <= ExpA_D = INFEXP and MantA_D = INFMANT;
  IsInfB_S <= ExpB_D = INFEXP and MantB_D = INFMANT;
  IsInfC_S <= ExpC_D = INFEXP and MantC_D = INFMANT;

  -- Nans have all-ones exponents and non-zero mantissa
  -- Improperly boxed operands are treated as canonical NaNs
  IsNaNA_S <= (unsigned(ExpA_D) = MAXEXP and unsigned(MantA_D) /= 0) or ABox_S = '0';
  IsNaNB_S <= (unsigned(ExpB_D) = MAXEXP and unsigned(MantB_D) /= 0) or BBox_S = '0';
  IsNaNC_S <= (unsigned(ExpC_D) = MAXEXP and unsigned(MantC_D) /= 0) or CBox_S = '0';

  -- Zeroes are encoded by all-zero eponent and mantissa
  IsZeroA_S <= unsigned(ExpA_D & MantA_D) = 0;
  IsZeroB_S <= unsigned(ExpB_D & MantB_D) = 0;
  IsZeroC_S <= unsigned(ExpC_D & MantC_D) = 0;

  -- An input is Inf
  InputInf_S <= IsInfA_S or IsInfB_S or IsInfC_S;

  -- An input is NaN
  InputNaN_S <= IsNaNA_S or IsNaNB_S or IsNaNC_S;

  -- Detect a signaling NaN at the inputs (one is enough to trigger condition)
  -- Improperly boxed operands are treated as canonical NaNs
  SignalingNaN_S <= (IsNaNA_S and ABox_S = '1' and MantA_D(QUIETBIT) = '0')
                    or (IsNaNB_S and BBox_S = '1' and MantB_D(QUIETBIT) = '0')
                    or (IsNaNC_S and CBox_S = '1' and MantC_D(QUIETBIT) = '0');

  -- Effective subtraction in FMA occurs when product and addend signs differ
  EffSub_S <= (SignA_D xor SignB_D) /= SignC_D;  -- 3-way xor to boolean


  -----------------------------------------------------------------------------
  -- FMA Special Case Handling
  -----------------------------------------------------------------------------

  --! @brief Invalid operation and special case detection
  --! @details Handles invalid operations and special cases involving NaNs and
  --! infinities and returns the apporpriate result.
  p_invalidOps : process (all) is
  begin  -- process p_invalidOps

    -- Default Special Result: Canonical Quiet NaN ([1]: p.48)
    SpecialResult_D <= NAN(EXP_BITS, MAN_BITS);

    -- Default control flags all clear
    SpecialFlags_D <= (others => '0');

    -- Don't bypass FMA by default
    SpecialCase_S <= false;

    -- Handle potentially mixed nan & infinity input => important for the case
    -- where infinity and zero are multiplied and added to a qnan.
    -- RISC-V mandates raising the NV exception in these cases! ([1]: p.48)
    -- (inf * 0) + c or (0 * inf) + c INVALID, no matter c (even quiet NaNs)
    if (IsInfA_S and IsZeroB_S) or (IsZeroA_S and IsInfB_S) then

      -- Bypass the FMA since we know the result
      SpecialCase_S      <= true;
      -- Output is the canonical quiet NaN (already set)
      SpecialFlags_D(NV) <= '1';        -- Invalid OP exception is set

    -- NaN Inputs cause canonical quiet NaN at the output and maybe invalid OP
    elsif InputNaN_S then

      -- Bypass the FMA since we know the result
      SpecialCase_S <= true;

      -- Output is the canonical quiet NaN (already set)
      -- Invalid OP exception if NaN was signalling
      if SignalingNaN_S then
        SpecialFlags_D(NV) <= '1';
      end if;

    -- Other Infinity input cases
    elsif InputInf_S then

      -- Bypass the FMA since we know the result
      SpecialCase_S <= true;

      -- Effective addition of opposite infinities (±inf - ±inf) is invalid!
      if ((IsInfA_S or IsInfB_S) and IsInfC_S and EffSub_S) then
        -- Output is the canonical quiet NaN (already set)
        SpecialFlags_D(NV) <= '1';      -- Invalid OP exception is set

      -- Handle cases where output will be inf because of inf product input
      elsif (IsInfA_S or IsInfB_S) then

        -- Output will be inf with the product sign.
        SpecialResult_D  <= INF(EXP_BITS, MAN_BITS);
        SpecialResSign_D <= SignA_D xor SignB_D;

        -- Overflow and Inexact flags are set
        SpecialFlags_D(OvF) <= '1';
        SpecialFlags_D(NX)  <= '1';

      -- Handle cases where addend is inf
      elsif IsInfC_S then

        -- Output will be operand C (i.e. inf with sign of C)
        SpecialResult_D  <= INF(EXP_BITS, MAN_BITS);
        SpecialResSign_D <= SignC_D;

        -- Overflow and Inexact flags are set
        SpecialFlags_D(OvF) <= '1';
        SpecialFlags_D(NX)  <= '1';

      end if;

    else
      null;

    end if;

  end process p_invalidOps;

  -----------------------------------------------------------------------------
  -- FMA computation
  -----------------------------------------------------------------------------
  -- FMA core instance
  i_fma_core : fma_core
    generic map (
      EXP_BITS => EXP_BITS,
      MAN_BITS => MAN_BITS)
    port map (
      SignA_DI     => SignA_D,
      SignB_DI     => SignB_D,
      SignC_DI     => SignC_D,
      ExpA_DI      => unsigned(ExpA_D),
      ExpB_DI      => unsigned(ExpB_D),
      ExpC_DI      => unsigned(ExpC_D),
      MantA_DI     => MantA_D,
      MantB_DI     => MantB_D,
      MantC_DI     => MantC_D,
      IsNormalA_DI => IsNormalA_D,
      IsNormalB_DI => IsNormalB_D,
      IsNormalC_DI => IsNormalC_D,
      IsZeroA_SI   => IsZeroA_S,
      IsZeroB_SI   => IsZeroB_S,
      ResSign_DO   => ResSign_D,
      ResExp_DO    => ResExpPreRnd_D,
      ResMant_DO   => ResMantPreRnd_D);


  -----------------------------------------------------------------------------
  -- Rounding:
  -- - Overflow results from fma_core will have exponents larger than MAXEXP
  -- - Denormals will have exponent = 0, RISC-V mandates checking underflow
  --   AFTER rounding
  -----------------------------------------------------------------------------

  -- Assemble Pre-Round Absolute Result (can have wrong exponent in overflow)
  ResAbsPreRnd_D <= std_logic_vector(ResExpPreRnd_D(EXP_BITS-1 downto 0))
                    & ResMantPreRnd_D(ResMantPreRnd_D'high-1 downto 2);

  -- Check for overflow before rounding
  OFBeforeRnd_S <= ResExpPreRnd_D >= MAXEXP;

  -- Not used in RISC-V but free: underflow before rounding
  UFBeforeRnd_S <= ResMantPreRnd_D(ResMantPreRnd_D'high) = '0';

  -- Detect true Zero result
  ResZero_S <= unsigned(ResMantPreRnd_D) = 0;

  i_fp_rounding : fp_rounding
    generic map (
      EXP_BITS => EXP_BITS,
      MAN_BITS => MAN_BITS)
    port map (
      ResultAbs_DI     => ResAbsPreRnd_D,
      ResultSign_DI    => ResSign_D,
      RoundSticky_SI   => ResRndSticky_S,
      RoundMode_SI     => RoundMode_SI,
      OFBeforeRnd_SI   => OFBeforeRnd_S,
      ResZero_SI       => ResZero_S,
      EffSub_SI        => EffSub_S,
      RoundedResult_DO => RegularResult_D);

  -- Detect over- and underflow after rounding
  OFAfterRound_S <= RegularResExp_D = INFEXP;
  UFAfterRound_S <= RegularResExp_D = DENORMEXP;


  -- Regular result status flags
  StatusFlags_D <= (NV  => '0',         -- We only handle valid cases here
                    DZ  => '0',         -- No division
                    OvF => to_sl(OFAfterRound_S or OFBeforeRnd_S),
                    UF  => to_sl(UFAfterRound_S) and to_sl(not ResZero_S),
                    NX  => or_reduce(ResRndSticky_S) or to_sl(OFAfterRound_S) or to_sl(OFBeforeRnd_S));


  -----------------------------------------------------------------------------
  -- Pipeline registers at the outputs of the unit
  -----------------------------------------------------------------------------

  -- Select output depending on special case detection and feed into pipeline
  Result_D <= SpecialResult_D when SpecialCase_S else
              RegularResult_D;
  Status_D <= SpecialFlags_D when SpecialCase_S else
              StatusFlags_D;


  i_fp_pipe : fp_pipe
    generic map (
      WIDTH     => WIDTH,
      LATENCY   => LATENCY,
      TAG_WIDTH => TAG_WIDTH)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      Result_DI      => Result_D,
      Status_DI      => Status_D,
      Tag_DI         => Tag_DI,
      InValid_SI     => InValid_SI,
      InReady_SO     => InReady_SO,
      Flush_SI       => Flush_SI,
      ResultPiped_DO => Z_DO,
      StatusPiped_DO => Status_DO,
      TagPiped_DO    => Tag_DO,
      OutValid_SO    => OutValid_SO,
      OutReady_SI    => OutReady_SI);

end architecture rtl;

