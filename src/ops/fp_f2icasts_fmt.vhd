-------------------------------------------------------------------------------
-- Title      : Floating-Point Conversion Unit
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_f2icasts_fmt.vhd
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
--! - F2I
entity fp_f2icasts_fmt is

  generic (
    SRCENCODING : fpFmtEncoding_t    := DEFAULTENCODING(FP32);
    INTFORMATS  : activeIntFormats_t := (Active => (others => true),
                                        Length  => INTFMTLENGTHS);

    LATENCY   : natural := 0;
    TAG_WIDTH : natural := 0);

  port (
    Clk_CI       : in  std_logic;
    Reset_RBI    : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI         : in  std_logic_vector(WIDTH(SRCENCODING)-1 downto 0);
    ABox_SI      : in  std_logic;
    RoundMode_SI : in  rvRoundingMode_t;
    OpMod_SI     : in  std_logic;
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


end entity fp_f2icasts_fmt;


architecture rtl of fp_f2icasts_fmt is

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------

  -- Largest integer format we need to handle
  constant INTWIDTH : natural := MAXWIDTH(INTFORMATS);

  -- Mantissa needs to be wide enough to hold implicit bit and integer width
  constant MANTWIDTH : natural := maximum(SRCENCODING.ManBits+1, INTWIDTH);

  -- Make exponent wide enough to hold signed exponents
  constant EXPWIDTH : natural := SRCENCODING.ExpBits+1;

  constant FPWIDTH : natural := WIDTH(SRCENCODING);

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

   --! @breif Holds a result for each format
  type intFmtResults_t is array (intFmt_t) of std_logic_vector(Z_DO'range);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Sanitized formats (disabled formats default to first enabled one)
  signal DstFmt_S : intFmt_t;

  -- The sign will not change during casts
  signal Sign_D : std_logic;

  -- We're using signed exponents internally
  signal InputExp_D  : signed(EXPWIDTH-1 downto 0);
  signal InputMant_D : std_logic_vector(MANTWIDTH-1 downto 0);

  -- Classification of input
  signal InputMantZero_S, InputZero_S, InputInf_S : boolean;
  signal InputNan_S, InputNormal_S                : boolean;
  signal OFBeforeRound_S                          : boolean;
  signal OFAfterRound_S, UFAfterRound_S           : boolean;

  -- Special Result calculation
  signal SpecialRes_S    : boolean;
  signal SpecialResult_D : intFmtResults_t;
  signal SpecialStatus_D : rvStatus_t;

  -- Internal unbiased exponent
  signal InternalExp_D : signed(InputExp_D'range);

  -- Destination shifter
  signal MantPreshift_S : std_logic_vector(MANTWIDTH+SRCENCODING.ManBits+2 downto 0);
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
  DstFmt_S <= DstFmt_SI when INTFORMATS.Active(DstFmt_SI) else
              findFirstActive(INTFORMATS);

  ------------------------------------------------------------------------------
  -- Float Setup
  ------------------------------------------------------------------------------

  -- Get the sign from the input pattern
  Sign_D <= A_DI(FPWIDTH-1);

  -- Fetch Exponent from input and expand to internal size
  InputExp_D <= signed(resize(unsigned(A_DI(FPWIDTH-2 downto SRCENCODING.ManBits)), EXPWIDTH));

  -- Remove bias from exponent
  InternalExp_D <= InputExp_D - BIAS(SRCENCODING.ExpBits);

  -- Classify input
  InputMantZero_S <= unsigned(A_DI(SRCENCODING.ManBits-1 downto 0)) = 0;
  InputInf_S      <= (InputExp_D = signed("0" & MAXEXP(SRCENCODING.ExpBits))) and InputMantZero_S;
  InputNan_S      <= ((InputExp_D = signed("0" & MAXEXP(SRCENCODING.ExpBits))) and (not InputMantZero_S)) or ABox_SI = '0';
  InputZero_S     <= (InputExp_D = 0) and InputMantZero_S;
  InputNormal_S   <= InputExp_D /= 0;

  -- Leave the mantissa to the right of the internal representation for now
  -- which has length max(MANT,OUTPUT)
  p_mantInit : process (all) is
  begin  -- process  p_mantInit

    -- initialize all bits to 0
    InputMant_D <= (others => '0');

    -- set implicit bit
    if InputNormal_S then
      InputMant_D(SRCENCODING.ManBits) <= '1';
    else
      InputMant_D(SRCENCODING.ManBits) <= '0';
    end if;

    -- copy mantissa bits after implicit bit
    InputMant_D(SRCENCODING.ManBits-1 downto 0) <= A_DI(SRCENCODING.ManBits-1 downto 0);

  end process p_mantInit;

  -----------------------------------------------------------------------------
  -- Special case detection
  -----------------------------------------------------------------------------

  -- Handle special results (nan, inf, overflow, negative unsigned. zero is
  -- handled automatically by the regular path)
  SpecialRes_S <= InputNan_S or InputInf_S or OFBeforeRound_S
                  or (Sign_D = '1' and OpMod_SI = '1' and not RoundedResZero_S);

  ---- special result raises invalid exception
  SpecialStatus_D <= (NV     => '1',
                      others => '0');


  g_ifmt : for ifmt in intFmt_t generate 
  begin
      -- Special Case Handling
      p_specialCases : process(all)
        variable SpecialResultInt_D : std_logic_vector(Z_DO'range);
      begin  -- process p_specialCases
        -- default assignment
        SpecialResult_D(ifmt) <= (others => '0');
        SpecialResultInt_D    := (others => '0');
        
        if INTFORMATS.Active(ifmt) then
            if (SpecialRes_S) then
              -- By default overflow to positive max, which is 2**len or 2**(len-1)
              -- MSB one in case of unsigned ops
              SpecialResultInt_D(INTFORMATS.Length(ifmt)-2 downto 0) := (others => '1');
              SpecialResultInt_D(INTFORMATS.Length(ifmt)-1) := OpMod_SI;
    
              -- if we have a negative special case except for nans (OF or neg INF or unsigned), tie to -max or 0
              if (not InputNan_S and Sign_D = '1') then
                SpecialResultInt_D := not SpecialResultInt_D;
              end if;
    
              SpecialResult_D(ifmt)(INTFORMATS.Length(ifmt)-1 downto 0)        <= SpecialResultInt_D(INTFORMATS.Length(ifmt)-1 downto 0);
              -- Sign-extend integer result as per RISC-V ISA 2.3draft
              SpecialResult_D(ifmt)(INTWIDTH-1 downto INTFORMATS.Length(ifmt)) <= (others => SpecialResultInt_D(INTFORMATS.Length(ifmt)-1));
    
            end if;
        end if;
      end process;
    end generate g_ifmt;

  -- Shift into binary representation
  p_finalAdjustPrepare : process (all) is
  begin  -- process p_finalAdjust

    -- Extend shifter space by mant+1 to the right of the mantissa
    MantPreshift_S <= std_logic_vector(resize(unsigned(InputMant_D), MantPreshift_S'length) sll SRCENCODING.ManBits+2);

    -- Mantissa shift amount is dictated by exponent and mantissa length
    MantShamt_S <= SRCENCODING.ManBits - to_integer(InternalExp_D);

    -- Default no overflow
    OFBeforeRound_S <= false;

    -- Check for exponent overflow: when converting to unsigned the range is
    -- larger by one than in the signed case. cap the shift amount
    if InternalExp_D >= (INTFORMATS.Length(DstFmt_SI) - to_integer(not OpMod_SI)) then
      MantShamt_S     <= SRCENCODING.ManBits - INTFORMATS.Length(DstFmt_SI);
      OFBeforeRound_S <= true;

    -- Check for exponent underflow and cap mantissa shift amount. all bits are
    -- going to be in the sticky
    elsif InternalExp_D < -1 then
      MantShamt_S <= SRCENCODING.ManBits + 2;
    end if;

  end process p_finalAdjustPrepare;

  -- Do the actual shift
  ShiftedMant_S <= std_logic_vector(unsigned(MantPreshift_S) srl MantShamt_S);

 -- Assemble Result
  p_resAssemble : process (all) is
  begin  -- process p_resAssemble

    for ifmt in intFmt_t loop
      if INTFORMATS.Active(ifmt) then

        -- default assignment
        IntFmtFinalMant_D(ifmt) <= (others => '0');

        -- mantissa
        IntFmtFinalMant_D(ifmt)(INTFORMATS.Length(ifmt)-1 downto 0)        <= ShiftedMant_S(INTFORMATS.Length(ifmt)+SRCENCODING.ManBits+1 downto SRCENCODING.ManBits+2);
        -- Sign-extend integer result as per RISC-V ISA 2.3draft
        IntFmtFinalMant_D(ifmt)(INTWIDTH-1 downto INTFORMATS.Length(ifmt)) <= (others => IntFmtFinalMant_D(ifmt)(INTFORMATS.Length(ifmt)-1));

      end if;
    end loop;

  end process p_resAssemble;


  -- Result before rounding, add round/sticky
  FinalMant_D <= IntFmtFinalMant_D(DstFmt_SI);

  RoundSticky_S <= ShiftedMant_S(SRCENCODING.ManBits+1) & or_reduce(ShiftedMant_S(SRCENCODING.ManBits downto 0));

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
