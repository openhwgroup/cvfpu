-------------------------------------------------------------------------------
-- Title      : Floating-Point Conversion Unit
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_f2icasts.vhd
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
--              - F2I
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
--! - F2I
entity fp_f2icasts is

  generic (
    FORMATS : activeFormats_t := (Active   => (FP32 to FP16ALT => true, others => false),
                                  Encoding => DEFAULTENCODING);

    INTFORMATS : activeIntFormats_t := (Active => (others => true),
                                        Length => INTFMTLENGTHS);

    LATENCY   : natural := 0;
    TAG_WIDTH : natural := 0);

  port (
    Clk_CI       : in  std_logic;
    Reset_RBI    : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI         : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
    ABox_SI      : in  fmtLogic_t;
    RoundMode_SI : in  rvRoundingMode_t;
    OpMod_SI     : in  std_logic;
    SrcFmt_SI    : in  fpFmt_t;
    DstFmt_SI    : in  intFmt_t;
    Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI   : in  std_logic;
    InReady_SO   : out std_logic;
    Flush_SI     : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO         : out std_logic_vector(MAXWIDTH(INTFORMATS)-1 downto 0);
    Status_DO    : out rvStatus_t;
    Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO      : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO  : out std_logic;
    OutReady_SI  : in  std_logic);


end entity fp_f2icasts;


architecture rtl of fp_f2icasts is


  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------

  -- Check how many bits are needed to hold all active formats
  constant SUPERFMT : fpFmtEncoding_t := SUPERFORMAT(FORMATS);

  -- Largest integer format we need to handle
  constant INTWIDTH : natural := MAXWIDTH(INTFORMATS);

  -- Mantissa needs to be wide enough to hold implicit bit and integer width
  constant MANTWIDTH : natural := maximum(SUPERFMT.ManBits+1, INTWIDTH);

  -- Make exponent wide enough to hold signed exponents
  constant EXPWIDTH : natural := SUPERFMT.ExpBits+1;

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

  --! @brief Holds the internally encoded exponent for all formats
  type fmtExponent_t is array (fpFmt_t) of signed(EXPWIDTH-1 downto 0);

  --! @brief Holds the internally encoded mantissa for all formats
  type fmtMantissa_t is array (fpFmt_t) of std_logic_vector(MANTWIDTH-1 downto 0);

  --! @breif Holds a result for each format
  type intFmtResults_t is array (intFmt_t) of std_logic_vector(Z_DO'range);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Sanitized formats (disabled formats default to first enabled one)
  signal SrcFmt_S : fpFmt_t := smallestActive(FORMATS);
  signal DstFmt_S : intFmt_t;

  -- The sign will not change during casts
  signal Sign_D : std_logic;

  -- We're using signed exponents internally
  signal FmtInputExp_D : fmtExponent_t;
  signal InputExp_D    : signed(EXPWIDTH-1 downto 0);

  -- Mantissa for each format, includes implicit bit
  signal FmtInputMant_D : fmtMantissa_t;
  signal InputMant_D    : std_logic_vector(MANTWIDTH-1 downto 0);

  -- Classification of input
  signal InputMantZero_S, InputZero_S, InputInf_S : fmtBooleans_t;
  signal InputNan_S, InputNormal_S                : fmtBooleans_t;
  signal OFBeforeRound_S                          : boolean;
  signal OFAfterRound_S, UFAfterRound_S           : intFmtBooleans_t;

  -- Special Result calculation
  signal SpecialRes_S    : boolean;
  signal SpecialResult_D : intFmtResults_t;
  signal SpecialStatus_D : rvStatus_t;

  -- Internal unbiased exponent
  signal InternalExp_D : signed(InputExp_D'range);

  -- Destination shifter
  signal MantPreshift_S : std_logic_vector(MANTWIDTH+SUPERFMT.ManBits+2 downto 0);
  signal ShiftedMant_S  : std_logic_vector(MantPreshift_S'range);
  -- final shift amount for mantissa can go both ways (left/right)
  signal MantShamt_S    : integer;

  -- Final value holds round and sticky bits
  signal IntFmtFinalMant_D : intFmtResults_t;
  signal FinalMant_D       : std_logic_vector(Z_DO'range);

  -- Rounding happens on whole result, add round/sticky
  signal RoundSticky_S : std_logic_vector(1 downto 0);

  -- rounded result will have one extra (wrong) sign bit
  signal ResRounded_D         : std_logic_vector(INTWIDTH downto 0);
  signal ResRoundedSignCorr_D : std_logic_vector(Z_DO'range);
  signal RegularStatus_D      : rvStatus_t;
  signal RoundedResZero_S     : boolean;

  -- final result
  signal Result_D : std_logic_vector(Z_DO'range);
  signal Status_D : rvStatus_t;

  -- Control information about the output
  signal Zext_S                  : std_logic;
  signal TagInt_D, TagIntPiped_D : std_logic_vector(TAG_WIDTH downto 0);

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Limit available formats to active ones
  -----------------------------------------------------------------------------
  SrcFmt_S <= SrcFmt_SI when FORMATS.Active(SrcFmt_SI) else
              findFirstActive(FORMATS);
  DstFmt_S <= DstFmt_SI when INTFORMATS.Active(DstFmt_SI) else
              findFirstActive(INTFORMATS);


  ------------------------------------------------------------------------------
  -- Parallel components generation (muxing for float format-specific indices)
  ------------------------------------------------------------------------------
  g_fmtSpecific : for fmt in fpFmt_t generate
    g_activeFmts : if FORMATS.Active(fmt) generate

      -- Fetch Exponent from input and expand to internal size
      FmtInputExp_D(fmt) <= signed(resize(unsigned(A_DI(WIDTH(fmt, FORMATS)-2 downto FORMATS.Encoding(fmt).ManBits)), EXPWIDTH));

      -- Classify input
      InputMantZero_S(fmt) <= unsigned(A_DI(FORMATS.Encoding(fmt).ManBits-1 downto 0)) = 0;
      InputInf_S(fmt)      <= (FmtInputExp_D(fmt) = signed("0" & MAXEXP(fmt, FORMATS))) and InputMantZero_S(fmt);
      InputNan_S(fmt)      <= ((FmtInputExp_D(fmt) = signed("0" & MAXEXP(fmt, FORMATS))) and (not InputMantZero_S(fmt))) or ABox_SI(fmt) = '0';
      InputZero_S(fmt)     <= (FmtInputExp_D(fmt) = 0) and InputMantZero_S(fmt);
      InputNormal_S(fmt)   <= FmtInputExp_D(fmt) /= 0;

      -- Leave the mantissa to the right of the internal representation for now
      -- which has length max(MANT,OUTPUT)
      p_mantInit : process (all) is
      begin  -- process  p_mantInit

        -- initialize all bits to 0
        FmtInputMant_D(fmt) <= (others => '0');

        -- set implicit bit
        if InputNormal_S(fmt) then
          FmtInputMant_D(fmt)(FORMATS.Encoding(fmt).ManBits) <= '1';
        else
          FmtInputMant_D(fmt)(FORMATS.Encoding(fmt).ManBits) <= '0';
        end if;

        -- copy mantissa bits after implicit bit
        FmtInputMant_D(fmt)(FORMATS.Encoding(fmt).ManBits-1 downto 0) <= A_DI(FORMATS.Encoding(fmt).ManBits-1 downto 0);

      end process p_mantInit;

    end generate g_activeFmts;
  end generate g_fmtSpecific;

  -----------------------------------------------------------------------------
  -- Parallel components generation for integer-format specific indices
  -----------------------------------------------------------------------------
  g_intFmtSpecific : for ifmt in intFmt_t generate
    g_activeFmts : if INTFORMATS.Active(ifmt) generate


      -- Special Case Handling
      p_specialCases : process(all)
        variable SpecialResultInt_D : std_logic_vector(INTFORMATS.Length(ifmt)-1 downto 0);
      begin  -- process p_specialCases

        -- default assignment
        SpecialResult_D(ifmt) <= (others => '0');

        if (SpecialRes_S) then
          -- By default overflow to positive max, which is 2**len or 2**(len-1)
          -- MSB one in case of unsigned ops
          SpecialResultInt_D(SpecialResultInt_D'high-1 downto 0) := (others => '1');
          SpecialResultInt_D(SpecialResultInt_D'high)            := OpMod_SI;

          -- if we have a negative special case except for nans (OF or neg INF or unsigned), tie to -max or 0
          if (not InputNan_S(SrcFmt_SI) and Sign_D = '1') then
            SpecialResultInt_D := not SpecialResultInt_D;
          end if;

          SpecialResult_D(ifmt)(SpecialResultInt_D'range)                  <= SpecialResultInt_D;
          -- Sign-extend integer result as per RISC-V ISA 2.3draft
          SpecialResult_D(ifmt)(INTWIDTH-1 downto INTFORMATS.Length(ifmt)) <= (others => SpecialResultInt_D(SpecialResultInt_D'high));
        end if;
      end process;

      p_resAssemble : process (all) is
      begin  -- process p_resAssemble

        -- default assignment
        IntFmtFinalMant_D(ifmt) <= (others => '0');

        -- mantissa
        IntFmtFinalMant_D(ifmt)(INTFORMATS.Length(ifmt)-1 downto 0)        <= ShiftedMant_S(INTFORMATS.Length(ifmt)+SUPERFMT.ManBits+1 downto SUPERFMT.ManBits+2);
        -- Sign-extend integer result as per RISC-V ISA 2.3draft
        IntFmtFinalMant_D(ifmt)(INTWIDTH-1 downto INTFORMATS.Length(ifmt)) <= (others => IntFmtFinalMant_D(ifmt)(INTFORMATS.Length(ifmt)-1));

      end process p_resAssemble;
    end generate g_activeFmts;
  end generate g_intFmtSpecific;

  -----------------------------------------------------------------------------
  -- Input acquisition
  -----------------------------------------------------------------------------

  -- Get the sign from the input pattern
  Sign_D <= A_DI(WIDTH(SrcFmt_S, FORMATS)-1);

  -- Get the exponent from the input pattern
  InputExp_D <= FmtInputExp_D(SrcFmt_S);

  -----------------------------------------------------------------------------
  -- Special case detection
  -----------------------------------------------------------------------------

  -- Handle special results (nan, inf, overflow, negative unsigned. zero is
  -- handled automatically by the regular path)
  SpecialRes_S <= InputNan_S(SrcFmt_S) or InputInf_S(SrcFmt_S) or OFBeforeRound_S
                  or (Sign_D = '1' and OpMod_SI = '1' and not RoundedResZero_S);

  ---- special result raises invalid exception
  SpecialStatus_D <= (NV     => '1',
                      others => '0');


  -----------------------------------------------------------------------------
  -- Bring input to internal representation
  -----------------------------------------------------------------------------

  -- Mantissa lies on the right end of the representation
  InputMant_D <= FmtInputMant_D(SrcFmt_S);

  -- Remove bias from exponent
  InternalExp_D <= InputExp_D - BIAS(SrcFmt_S, FORMATS);


  -----------------------------------------------------------------------------
  -- Shift into binary representation
  -----------------------------------------------------------------------------
  p_finalAdjustPrepare : process (all) is
  begin  -- process p_finalAdjust

    -- Extend shifter space by mant+1 to the right of the mantissa
    MantPreshift_S <= std_logic_vector(resize(unsigned(InputMant_D), MantPreshift_S'length) sll SUPERFMT.ManBits+2);

    -- Mantissa shift amount is dictated by exponent and mantissa length
    MantShamt_S <= FORMATS.Encoding(SrcFmt_S).ManBits - to_integer(InternalExp_D);

    -- Default no overflow
    OFBeforeRound_S <= false;

    -- Check for exponent overflow: when converting to unsigned the range is
    -- larger by one than in the signed case. cap the shift amount
    if InternalExp_D >= (INTFORMATS.Length(DstFmt_SI) - to_integer((not OpMod_SI))) then
      MantShamt_S     <= FORMATS.Encoding(SrcFmt_S).ManBits - INTFORMATS.Length(DstFmt_SI);
      OFBeforeRound_S <= true;

    -- Check for exponent underflow and cap mantissa shift amount. all bits are
    -- going to be in the sticky
    elsif InternalExp_D < -1 then
      MantShamt_S <= FORMATS.Encoding(SrcFmt_S).ManBits + 2;
    end if;

  end process p_finalAdjustPrepare;

  -- Do the actual shift
  ShiftedMant_S <= std_logic_vector(unsigned(MantPreshift_S) srl MantShamt_S);

  -- Result before rounding, add round/sticky
  FinalMant_D <= IntFmtFinalMant_D(DstFmt_SI);

  RoundSticky_S <= ShiftedMant_S(SUPERFMT.ManBits+1) & or_reduce(ShiftedMant_S(SUPERFMT.ManBits downto 0));

  -----------------------------------------------------------------------------
  -- Final Round and Postprocessing
  -----------------------------------------------------------------------------

  -- Round the result
  i_fp_rounding : fp_rounding
    generic map (
      EXP_BITS => INTWIDTH,
      MAN_BITS => 0)
    port map (
      ResultAbs_DI     => FinalMant_D,
      ResultSign_DI    => Sign_D,
      RoundSticky_SI   => RoundSticky_S,
      RoundMode_SI     => RoundMode_SI,
      OFBeforeRnd_SI   => false,        -- we handled this already
      ResZero_SI       => false,        -- we don't round zeroes
      EffSub_SI        => false,        -- dito
      RoundedResult_DO => ResRounded_D);


  ResRoundedSignCorr_D <= std_logic_vector(-signed(ResRounded_D(ResRounded_D'high-1 downto 0))) when Sign_D = '1' else
                          ResRounded_D(ResRounded_D'high-1 downto 0);

  RoundedResZero_S <= unsigned(ResRoundedSignCorr_D) = 0;

  RegularStatus_D <= (NX => or_reduce(RoundSticky_S), others => '0');

  -----------------------------------------------------------------------------
  -- Pipeline registers at the outputs of the unit
  -----------------------------------------------------------------------------

  -- Select final result
  Result_D <= SpecialResult_D(DstFmt_S) when SpecialRes_S else
              ResRoundedSignCorr_D;

  Status_D <= SpecialStatus_D when SpecialRes_S else
              RegularStatus_D;

  -- Output should be sign-extended downstream
  Zext_S <= not Result_D(Result_D'high);

  -- Pipe through the zext indicator as well
  TagInt_D <= Zext_S & Tag_DI;


  i_fp_pipe : fp_pipe
    generic map (
      WIDTH     => INTWIDTH,
      LATENCY   => LATENCY,
      TAG_WIDTH => TAG_WIDTH+1)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      Result_DI      => Result_D,
      Status_DI      => Status_D,
      Tag_DI         => TagInt_D,
      InValid_SI     => InValid_SI,
      InReady_SO     => InReady_SO,
      Flush_SI       => Flush_SI,
      ResultPiped_DO => Z_DO,
      StatusPiped_DO => Status_DO,
      TagPiped_DO    => TagIntPiped_D,
      OutValid_SO    => OutValid_SO,
      OutReady_SI    => OutReady_SI);

  Zext_SO <= TagIntPiped_D(TagIntPiped_D'high);
  Tag_DO  <= TagIntPiped_D(Tag_DO'range);


end architecture rtl;
