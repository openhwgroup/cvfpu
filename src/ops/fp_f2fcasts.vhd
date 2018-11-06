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
use work.fpnew_fmts_pkg.all;
use work.fpnew_comps_pkg.all;

--! @brief Floating-Point Conversion Unit
--! @details Parametric floating-point conversion unit for floating-point to
--! floating-point casts as well as integer-float and float-integer casts.
--! Supported operations from fpnew_pkg.fpOp:
--! - F2F
entity fp_f2fcasts is

  generic (
    FORMATS : activeFormats_t := (Active   => (FP32 to FP16ALT => true, others => false),
                                  Encoding => DEFAULTENCODING);

    LATENCY   : natural := 0;
    TAG_WIDTH : natural := 0);

  port (
    Clk_CI       : in  std_logic;
    Reset_RBI    : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI         : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
    ABox_SI      : in  fmtLogic_t;
    RoundMode_SI : in  rvRoundingMode_t;
    SrcFmt_SI    : in  fpFmt_t;
    DstFmt_SI    : in  fpFmt_t;
    Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI   : in  std_logic;
    InReady_SO   : out std_logic;
    Flush_SI     : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO         : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
    Status_DO    : out rvStatus_t;
    Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO      : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO  : out std_logic;
    OutReady_SI  : in  std_logic);

end entity fp_f2fcasts;


architecture rtl of fp_f2fcasts is

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------

  -- Check how many bits are needed to hold all active formats
  constant SUPERFMT : fpFmtEncoding_t := SUPERFORMAT(FORMATS);

  -- Check the largest float format
  constant LARGESTFMT : fpFmtEncoding_t := FORMATS.Encoding(largestActive(FORMATS));

  -- Mantissa also holds implicit bit
  constant MANTWIDTH : natural := SUPERFMT.ManBits+1;

  -- Make exponent wide enough to hold internal exponents or readjustment
  -- shift amount in signed form
  constant EXPWIDTH : natural := maximum(SUPERFMT.ExpBits+1, clog2(MANTWIDTH)+1);

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

  --! @brief Holds the internally encoded exponent for all formats
  type fmtExponent_t is array (fpFmt_t) of signed(EXPWIDTH-1 downto 0);

  --! @brief Holds the internally encoded mantissa for all formats
  type fmtMantissa_t is array (fpFmt_t) of std_logic_vector(MANTWIDTH-1 downto 0);

  --! @breif Holds a pre-round absolute result for each format
  type fmtPreRnd_t is array (fpFmt_t) of std_logic_vector(MAXWIDTH(FORMATS)-2 downto 0);

--! @breif Holds a result for each format
  type fmtResults_t is array (fpFmt_t) of std_logic_vector(Z_DO'range);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Sanitized formats (disabled formats default to first enabled one)
  signal SrcFmt_S, DstFmt_S : fpFmt_t := smallestActive(FORMATS);

  -- The sign will not change during casts, only its position
  signal Sign_D : std_logic;

  -- We're using unbiased exponents internally
  signal FmtInputExp_D : fmtExponent_t;
  signal InputExp_D    : signed(EXPWIDTH-1 downto 0);

  -- We're adding the implicit bit back into the mantissa
  signal FmtInputMant_D : fmtMantissa_t;
  signal InputMant_D    : std_logic_vector(MANTWIDTH-1 downto 0);

  -- Classification of input
  signal InputMantZero_S, InputZero_S, InputInf_S : fmtBooleans_t;
  signal InputNan_S, SigNan_S, InputNormal_S      : fmtBooleans_t;
  signal OFBeforeRound_S                          : boolean;
  signal OFAfterRound_S, UFAfterRound_S           : fmtBooleans_t;

  -- Special Result calculation
  signal SpecialRes_S    : boolean;
  signal SpecialResult_D : fmtResults_t;
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
  signal FmtPreRndRes_D : fmtPreRnd_t;
  signal PreRndRes_D    : std_logic_vector(MAXWIDTH(FORMATS)-2 downto 0);
  signal RoundSticky_S  : std_logic_vector(1 downto 0);

  -- rounded result
  signal ResRounded_D, ResRoundedSignCorr_D : std_logic_vector(Z_DO'range);
  signal RegularStatus_D                    : rvStatus_t;

  -- final result
  signal Result_D : std_logic_vector(Z_DO'range);
  signal Status_D : rvStatus_t;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Limit available formats to active ones
  -----------------------------------------------------------------------------
  SrcFmt_S <= SrcFmt_SI when FORMATS.Active(SrcFmt_SI) else
              smallestActive(FORMATS);
  DstFmt_S <= DstFmt_SI when FORMATS.Active(DstFmt_SI) else
              smallestActive(FORMATS);


  ------------------------------------------------------------------------------
  -- Parallel components generation (mostly muxing for format-specific indexes)
  ------------------------------------------------------------------------------
  g_fmtSpecific : for fmt in fpFmt_t generate
    g_activeFmts : if FORMATS.Active(fmt) generate

      -- Fetch Exponent from input and expand to internal size
      FmtInputExp_D(fmt) <= signed(resize(unsigned(A_DI(WIDTH(fmt, FORMATS)-2 downto FORMATS.Encoding(fmt).ManBits)), EXPWIDTH));

      -- Classify input
      InputMantZero_S(fmt) <= unsigned(A_DI(FORMATS.Encoding(fmt).ManBits-1 downto 0)) = 0;
      InputInf_S(fmt)      <= (FmtInputExp_D(fmt) = signed("0" & MAXEXP(fmt, FORMATS))) and InputMantZero_S(fmt);
      InputNan_S(fmt)      <= ((FmtInputExp_D(fmt) = signed("0" & MAXEXP(fmt, FORMATS))) and (not InputMantZero_S(fmt))) or ABox_SI(fmt) = '0';
      SigNan_S(fmt)        <= InputNan_S(fmt) and ABox_SI(fmt) = '1' and A_DI(FORMATS.Encoding(fmt).ManBits-1) = '0';
      InputZero_S(fmt)     <= (FmtInputExp_D(fmt) = 0) and InputMantZero_S(fmt);
      InputNormal_S(fmt)   <= FmtInputExp_D(fmt) /= 0;

      -- Format-specific special case bit-patterns
      p_specialRes : process (all) is
        variable specialResult : std_logic_vector(Z_DO'range);
      begin  -- process p_specialRes

        -- default special result is ones (NaN boxing)
        specialResult := (others => '1');

        -- detect nan
        if InputNan_S(SrcFmt_S) then
          specialResult(WIDTH(fmt, FORMATS)-1 downto 0) := NAN(fmt, FORMATS);

        -- detect zero
        elsif InputZero_S(SrcFmt_S) then
          specialResult(WIDTH(fmt, FORMATS)-1) := Sign_D;
          specialResult(WIDTH(fmt, FORMATS)-2 downto 0) := (others => '0');
        end if;

        SpecialResult_D(fmt) <= specialResult;

      end process p_specialRes;


      -- Move the mantissa to the left of the internal representation
      p_mantInit : process (all) is
      begin  -- process  p_mantLeft

        -- initialize all bits to 0
        FmtInputMant_D(fmt) <= (others => '0');

        -- set implicit bit
        if InputNormal_S(fmt) then
          FmtInputMant_D(fmt)(InputMant_D'high) <= '1';
        else
          FmtInputMant_D(fmt)(InputMant_D'high) <= '0';
        end if;

        -- copy mantissa bits after implicit bit
        FmtInputMant_D(fmt)(InputMant_D'high-1 downto InputMant_D'high-FORMATS.Encoding(fmt).ManBits) <= A_DI(FORMATS.Encoding(fmt).ManBits-1 downto 0);

      end process p_mantInit;


      -- reassemble final result to format-specific locations
      p_preRndAssemble : process (all) is
      begin  -- process

        -- default: fill with ones (NaN-boxing)
        FmtPreRndRes_D(fmt) <= (others => '1');

        -- Assemble the preround result
        FmtPreRndRes_D(fmt)(WIDTH(fmt, FORMATS)-2 downto FORMATS.Encoding(fmt).ManBits) <= std_logic_vector(FinalExp_D(FORMATS.Encoding(fmt).ExpBits-1 downto 0));
        FmtPreRndRes_D(fmt)(FORMATS.Encoding(fmt).ManBits-1 downto 0)                   <= FinalMant_D(FORMATS.Encoding(fmt).ManBits+1 downto 2);  -- RS are behind

      end process;

      -- Classify final result
      OFAfterRound_S(fmt) <= unsigned(ResRoundedSignCorr_D(WIDTH(fmt, FORMATS)-2 downto FORMATS.Encoding(fmt).ManBits)) = unsigned'(MAXEXP(fmt, FORMATS));
      UFAfterRound_S(fmt) <= unsigned(ResRoundedSignCorr_D(WIDTH(fmt, FORMATS)-2 downto FORMATS.Encoding(fmt).ManBits)) = 0;

    end generate g_activeFmts;
  end generate g_fmtSpecific;


  -----------------------------------------------------------------------------
  -- Input acquisition
  -----------------------------------------------------------------------------

  -- Get the sign from the input pattern
  Sign_D <= A_DI(WIDTH(SrcFmt_S, FORMATS)-1);

  InputExp_D  <= FmtInputExp_D(SrcFmt_S);
  InputMant_D <= FmtInputMant_D(SrcFmt_S);

  -----------------------------------------------------------------------------
  -- Special case handling
  -----------------------------------------------------------------------------

  -- Handle special results (nan and zero only, inf needs to be rounded)
  SpecialRes_S <= InputZero_S(SrcFmt_S) or InputNan_S(SrcFmt_S);

  -- signalling nan raises invalid exception
  SpecialStatus_D <= (NV     => to_sl(InputNan_S(SrcFmt_S) and SigNan_S(SrcFmt_S)),
                      others => '0');


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
  InternalExp_D <= InputExp_D - ExpNormShift_S + to_integer(to_sl(not InputNormal_S(SrcFmt_S)));


  -----------------------------------------------------------------------------
  -- Perform the casting
  -----------------------------------------------------------------------------

  -- Remove old bias, apply new one
  DestExp_D <= InternalExp_D - BIAS(SrcFmt_S, FORMATS) + BIAS(DstFmt_S, FORMATS);


  p_finalAdjustPrepare : process (all) is
    variable MantShamtInt_S : integer;
  begin  -- process p_finalAdjust

    -- Default assignments
    FinalExp_D <= DestExp_D;

    -- Place mantissa to the left of shifter space
    MantPreshift_D <= std_logic_vector(resize(unsigned(InternalMant_D), MantPreshift_D'length) sll MANTWIDTH+1);

    -- By default shift mantissa to the required number of bits
    MantShamtInt_S := SUPERFMT.ManBits - FORMATS.Encoding(DstFmt_S).ManBits;

    -- No overflow by default
    OFBeforeRound_S <= false;

    -- Check for exponent overflow and adjust mantissa accordingly
    if (DestExp_D >= signed("0" & MAXEXP(DstFmt_S, FORMATS))) or InputInf_S(SrcFmt_S) then
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

  -- Result before rounding
  PreRndRes_D <= FmtPreRndRes_D(DstFmt_S);

  RoundSticky_S <= FinalMant_D(1 downto 0);

  -----------------------------------------------------------------------------
  -- Final Round and Postprocessing
  -----------------------------------------------------------------------------

  -- Round the result
  i_fp_rounding : fp_rounding
    generic map (
      EXP_BITS => LARGESTFMT.ExpBits,
      MAN_BITS => LARGESTFMT.ManBits)
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
    ResRoundedSignCorr_D(WIDTH(DstFmt_S, FORMATS)-1) <= Sign_D;

  end process;

  RegularStatus_D <= (NV  => '0',
                      DZ  => '0',
                      OvF => to_sl(OFAfterRound_S(DstFmt_S) or OFBeforeRound_S),
                      UF  => to_sl(UFAfterRound_S(DstFmt_S)),
                      NX  => or_reduce(RoundSticky_S) or to_sl(OFAfterRound_S(DstFmt_S) or OFBeforeRound_S));

  -----------------------------------------------------------------------------
  -- Pipeline registers at the outputs of the unit
  -----------------------------------------------------------------------------

  -- Select final result
  Result_D <= SpecialResult_D(DstFmt_S) when SpecialRes_S else
              ResRoundedSignCorr_D;

  Status_D <= SpecialStatus_D when SpecialRes_S else
              RegularStatus_D;

  i_fp_pipe : fp_pipe
    generic map (
      WIDTH     => MAXWIDTH(FORMATS),
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
