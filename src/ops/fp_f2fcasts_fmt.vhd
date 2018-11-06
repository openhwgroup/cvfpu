-------------------------------------------------------------------------------
-- Title      : Floating-Point Conversion Unit
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_f2fcasts.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-22
-- Last update: 2018-04-18
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Parametric floating-point conversion unit for floating-point to
--              floating-point casts as well as integer-float and float-integer
--              casts.
--              Supported operations from fpnew_pkg.fpOp:
--              - F2F
-------------------------------------------------------------------------------
-- Copyright (C) 2018 ETH Zurich, University of Bologna
-- All rights reserved.
--
-- This code is under development and not yet released to the public.
-- Until it is released, the code is under the copyright of ETH Zurich and
-- the University of Bologna, and may contain confidential and/or unpublished
-- work. Any reuse/redistribution is strictly forbidden without written
-- permission from ETH Zurich.
--
-- Bug fixes and contributions will eventually be released under the
-- SolderPad open hardware license in the context of the PULP platform
-- (http://www.pulp-platform.org), under the copyright of ETH Zurich and the
-- University of Bologna.
-------------------------------------------------------------------------------

library IEEE, work;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.fpnew_pkg.all;
use work.fpnew_fmts_pkg.all;
use work.fpnew_comps_pkg.all;

--! @brief Floating-Point Conversion Unit
--! @details Parametric floating-point conversion unit for floating-point to
--! floating-point casts as well as integer-float and float-integer casts.
--! Supported operations from fpnew_pkg.fpOp:
--! - F2F
entity fp_f2fcasts_fmt is

  generic (
    SRCENCODING : fpFmtEncoding_t := DEFAULTENCODING(FP32);
    DSTENCODING : fpFmtEncoding_t := DEFAULTENCODING(FP16);

    LATENCY   : natural := 0;
    TAG_WIDTH : natural := 0);

  port (
    Clk_CI       : in  std_logic;
    Reset_RBI    : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI         : in  std_logic_vector(WIDTH(SRCENCODING)-1 downto 0);
    ABox_SI      : in  std_logic;
    RoundMode_SI : in  rvRoundingMode_t;
    Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI   : in  std_logic;
    InReady_SO   : out std_logic;
    Flush_SI     : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO         : out std_logic_vector(WIDTH(DSTENCODING)-1 downto 0);
    Status_DO    : out rvStatus_t;
    Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO      : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO  : out std_logic;
    OutReady_SI  : in  std_logic);

end entity fp_f2fcasts_fmt;


architecture rtl of fp_f2fcasts_fmt is

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------

  -- Merger of the two formats
  constant SUPERFMT : fpFmtEncoding_t := (ExpBits => maximum(SRCENCODING.ExpBits, DSTENCODING.ExpBits),
                                             ManBits => maximum(SRCENCODING.ManBits, DSTENCODING.ManBits));

  -- Mantissa also holds implicit bit
  constant MANTWIDTH : natural := SUPERFMT.ManBits+1;

  -- Make exponent wide enough to hold internal exponents or readjustment
  -- shift amount in signed form
  constant EXPWIDTH : natural := maximum(SUPERFMT.ExpBits+1, clog2(MANTWIDTH)+1);


  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- The sign will not change during casts, only its position
  signal Sign_D : std_logic;

  -- We're using unbiased exponents internally
  signal InputExp_D : signed(EXPWIDTH-1 downto 0);

  -- We're adding the implicit bit back into the mantissa
  signal InputMant_D : std_logic_vector(MANTWIDTH-1 downto 0);

  -- Classification of input
  signal InputMantZero_S, InputZero_S, InputInf_S : boolean;
  signal InputNan_S, SigNan_S, InputNormal_S      : boolean;
  signal OFBeforeRound_S                          : boolean;
  signal OFAfterRound_S, UFAfterRound_S           : boolean;

  -- Special Result calculation
  signal SpecialRes_S    : boolean;
  signal SpecialResult_D : std_logic_vector(Z_DO'range);
  signal SpecialStatus_D : rvStatus_t;

  -- Normalization
  signal MantLeadingZeroes_S : unsigned(clog2(MANTWIDTH)-1 downto 0);
  signal ExpNormShift_S      : signed(InputExp_D'range);

  -- Internal Normalized representation
  signal InternalExp_D  : signed(InputExp_D'range);
  signal InternalMant_D : std_logic_vector(InputMant_D'range);

  -- Destination Biased, Renormalized representation
  signal DestExp_D, FinalExp_D : signed(InputExp_D'range);
  -- Final mantissa holds round and sticky bits
  signal MantPreshift_D        : std_logic_vector(2*MANTWIDTH downto 0);
  signal ShiftedMant_D         : std_logic_vector(2*MANTWIDTH downto 0);
  signal FinalMant_D           : std_logic_vector(MANTWIDTH+1 downto 0);

  -- final shift amount for mantissa
  signal MantShamt_S : integer;

  -- reassembled value before rounding
  signal PreRndRes_D   : std_logic_vector(WIDTH(DSTENCODING)-2 downto 0);
  signal RoundSticky_S : std_logic_vector(1 downto 0);

  -- rounded result
  signal ResRounded_D, ResRoundedSignCorr_D : std_logic_vector(Z_DO'range);
  signal RegularStatus_D                    : rvStatus_t;

  -- final result
  signal Result_D : std_logic_vector(Z_DO'range);
  signal Status_D : rvStatus_t;

begin  -- architecture rtl


  -----------------------------------------------------------------------------
  -- Input acquisition
  -----------------------------------------------------------------------------

  -- Get the sign from the input pattern
  Sign_D <= A_DI(WIDTH(SRCENCODING)-1);

  -- Fetch Exponent from input and expand to internal size
  InputExp_D <= signed(resize(unsigned(A_DI(WIDTH(SRCENCODING)-2 downto SRCENCODING.ManBits)), EXPWIDTH));

  -- Classify input
  InputMantZero_S <= unsigned(A_DI(SRCENCODING.ManBits-1 downto 0)) = 0;
  InputInf_S      <= (InputExp_D = signed("0" & MAXEXP(SRCENCODING))) and InputMantZero_S;
  InputNan_S      <= ((InputExp_D = signed("0" & MAXEXP(SRCENCODING))) and (not InputMantZero_S)) or ABox_SI = '0';
  SigNan_S        <= InputNan_S and ABox_SI = '1' and A_DI(SRCENCODING.ManBits-1) = '0';
  InputZero_S     <= (InputExp_D = 0) and InputMantZero_S;
  InputNormal_S   <= InputExp_D /= 0;


  -- Move the mantissa to the left of the internal representation
  p_mantInit : process (all) is
  begin  -- process  p_mantLeft

    -- initialize all bits to 0
    InputMant_D <= (others => '0');

    -- set implicit bit
    if InputNormal_S then
      InputMant_D(InputMant_D'high) <= '1';
    else
      InputMant_D(InputMant_D'high) <= '0';
    end if;

    -- copy mantissa bits after implicit bit
    InputMant_D(InputMant_D'high-1 downto InputMant_D'high-SRCENCODING.ManBits) <= A_DI(SRCENCODING.ManBits-1 downto 0);

  end process p_mantInit;


  -----------------------------------------------------------------------------
  -- Special case handling
  -----------------------------------------------------------------------------

  -- Handle special results (nan and zero only, inf needs to be rounded)
  SpecialRes_S <= InputZero_S or InputNan_S;

  -- signalling nan raises invalid exception
  SpecialStatus_D <= (NV => to_sl(InputNan_S and SigNan_S),
                     others => '0');


  -- Format-specific special case bit-patterns
  p_specialRes : process (all) is
    variable specialResult : std_logic_vector(Z_DO'range);
  begin  -- process p_specialRes

    -- default special result is ones (NaN boxing)
    specialResult := (others => '1');

    -- detect nan
    if InputNan_S then
      specialResult(WIDTH(DSTENCODING)-1 downto 0) := NAN(DSTENCODING);

    -- detect zero
    elsif InputZero_S then
      specialResult(WIDTH(DSTENCODING)-1)          := Sign_D;
      specialResult(WIDTH(DSTENCODING)-2 downto 0) := (others => '0');
    end if;

    SpecialResult_D <= specialResult;

  end process p_specialRes;


  -----------------------------------------------------------------------------
  -- Bring input to internal representation
  -----------------------------------------------------------------------------

  -- Normalize input mantissa using LZC
  i_lzc : find_first_one
    generic map (
      WIDTH => MANTWIDTH,
      FLIP  => 1)
    port map (
      in_i        => InputMant_D,
      first_one_o => MantLeadingZeroes_S,
      no_ones_o   => open);

  InternalMant_D <= std_logic_vector(unsigned(InputMant_D) sll to_integer(MantLeadingZeroes_S));

  -- Extend the leading zero count to internal exponent width
  ExpNormShift_S <= signed(resize(MantLeadingZeroes_S, EXPWIDTH));

  -- Adjust internal exponent to shift and also denormal adjustment
  InternalExp_D <= InputExp_D - ExpNormShift_S + to_integer(to_sl(not InputNormal_S));


  -----------------------------------------------------------------------------
  -- Perform the casting
  -----------------------------------------------------------------------------

  -- Remove old bias, apply new one
  DestExp_D <= InternalExp_D - BIAS(SRCENCODING) + BIAS(DSTENCODING);


  p_finalAdjustPrepare : process (all) is
    variable MantShamtInt_S : integer;
  begin  -- process p_finalAdjust

    -- Default assignments
    FinalExp_D <= DestExp_D;

    -- Place mantissa to the left of shifter space
    MantPreshift_D <= std_logic_vector(resize(unsigned(InternalMant_D), MantPreshift_D'length) sll MANTWIDTH+1);

    -- By default shift mantissa to the required number of bits
    MantShamtInt_S := SUPERFMT.ManBits - DSTENCODING.ManBits;

    -- No overflow by default
    OFBeforeRound_S <= false;

    -- Check for exponent overflow and adjust mantissa accordingly
    if (DestExp_D >= signed("0" & MAXEXP(DSTENCODING))) or InputInf_S then
      -- set up largest normal number
      FinalExp_D      <= signed("0" & MAXEXP(SUPERFMT))-1;
      MantPreshift_D  <= (others => '1');
      OFBeforeRound_S <= true;

    -- Check for exponent underflow and adjust mantissa shift amount
    elsif DestExp_D < 1 then
      FinalExp_D     <= (others => '0');
      MantShamtInt_S := to_integer(MantShamtInt_S - DestExp_D + 1);

    end if;

    -- Sanitize mantissa shift amount to be in bounds
    if MantShamtInt_S > MANTWIDTH+1 then
      MantShamtInt_S := MANTWIDTH+1;
    end if;

    -- assign final shift amount
    MantShamt_S <= MantShamtInt_S;

  end process p_finalAdjustPrepare;

  -- Do the final shift
  ShiftedMant_D <= std_logic_vector(unsigned(MantPreshift_D) srl MantShamt_S);

  -- mantissa and round bit
  FinalMant_D(FinalMant_D'high downto 1) <= ShiftedMant_D(ShiftedMant_D'high downto MANTWIDTH);

  -- sticky bit
  FinalMant_D(0) <= or_reduce(ShiftedMant_D(MANTWIDTH-1 downto 0));

  -- reassemble final result to format-specific locations
  p_preRndAssemble : process (all) is
  begin  -- process

    -- default: fill with ones (NaN-boxing)
    PreRndRes_D <= (others => '1');

    -- Assemble the preround result
    PreRndRes_D(WIDTH(DSTENCODING)-2 downto DSTENCODING.ManBits) <= std_logic_vector(FinalExp_D(DSTENCODING.ExpBits-1 downto 0));
    PreRndRes_D(DSTENCODING.ManBits-1 downto 0)                  <= FinalMant_D(DSTENCODING.ManBits+1 downto 2);  -- RS are behind

  end process;

  RoundSticky_S <= FinalMant_D(1 downto 0);


  -----------------------------------------------------------------------------
  -- Final Round and Postprocessing
  -----------------------------------------------------------------------------

  -- Round the result
  i_fp_rounding : fp_rounding
    generic map (
      EXP_BITS => DSTENCODING.ExpBits,
      MAN_BITS => DSTENCODING.ManBits)
    port map (
      ResultAbs_DI     => PreRndRes_D,
      ResultSign_DI    => Sign_D,
      RoundSticky_SI   => RoundSticky_S,
      RoundMode_SI     => RoundMode_SI,
      OFBeforeRnd_SI   => false,        -- we handled this already
      ResZero_SI       => false,        -- we don't round zeroes
      EffSub_SI        => false,        -- dito
      RoundedResult_DO => ResRounded_D);


  p_fixSign : process (all) is
  begin  -- process

    ResRoundedSignCorr_D <= ResRounded_D;

    -- the rounding block injects the sign in the topmost position
    -- restore the potentially nan-boxed value by writing one
    ResRoundedSignCorr_D(ResRoundedSignCorr_D'high) <= '1';

    -- this will put it where it belongs (maybe topmost again)
    ResRoundedSignCorr_D(WIDTH(DSTENCODING)-1) <= Sign_D;

  end process;

  RegularStatus_D <= (NV  => '0',
                      DZ  => '0',
                      OvF => to_sl(OFAfterRound_S or OFBeforeRound_S),
                      UF  => to_sl(UFAfterRound_S),
                      NX  => or_reduce(RoundSticky_S) or to_sl(OFAfterRound_S or OFBeforeRound_S));


  -- Classify final result
  OFAfterRound_S <= unsigned(ResRoundedSignCorr_D(WIDTH(DSTENCODING)-2 downto DSTENCODING.ManBits)) = unsigned'(MAXEXP(DSTENCODING));
  UFAfterRound_S <= unsigned(ResRoundedSignCorr_D(WIDTH(DSTENCODING)-2 downto DSTENCODING.ManBits)) = 0;


  -----------------------------------------------------------------------------
  -- Pipeline registers at the outputs of the unit
  -----------------------------------------------------------------------------

  -- Select final result
  Result_D <= SpecialResult_D when SpecialRes_S else
              ResRoundedSignCorr_D;

  Status_D <= SpecialStatus_D when SpecialRes_S else
              RegularStatus_D;

  i_fp_pipe : fp_pipe
    generic map (
      WIDTH     => WIDTH(DSTENCODING),
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

  -- Always one-extend output (NaN-boxing)
  Zext_SO <= '0';

end architecture rtl;
