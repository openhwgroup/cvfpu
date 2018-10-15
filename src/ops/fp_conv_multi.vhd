-------------------------------------------------------------------------------
-- Title      : Multiformat Casts
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_conv_multi.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-04-08
-- Last update: 2018-04-18
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
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

entity fp_conv_multi is

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
    A_DI         : in  std_logic_vector(MAXWIDTH(FORMATS, INTFORMATS)-1 downto 0);
    ABox_SI      : in  fmtLogic_t;
    RoundMode_SI : in  rvRoundingMode_t;
    Op_SI        : in  fpOp_t;
    OpMod_SI     : in  std_logic;
    FpFmt_SI     : in  fpFmt_t;
    FpFmt2_SI    : in  fpFmt_t;
    IntFmt_SI    : in  intFmt_t;
    Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI   : in  std_logic;
    InReady_SO   : out std_logic;
    Flush_SI     : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO         : out std_logic_vector(MAXWIDTH(FORMATS, INTFORMATS)-1 downto 0);
    Status_DO    : out rvStatus_t;
    Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO      : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO  : out std_logic;
    OutReady_SI  : in  std_logic);

end entity fp_conv_multi;


architecture parallel_paths of fp_conv_multi is

  -----------------------------------------------------------------------------
  -- Constant Definitions
  -----------------------------------------------------------------------------
  constant FP_WIDTH  : natural := MAXWIDTH(FORMATS);
  constant INT_WIDTH : natural := MAXWIDTH(INTFORMATS);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  signal OutReady_S                               : std_logic;
  signal F2IInValid_S, I2FInValid_S, F2FInValid_S : std_logic;
  signal F2IInReady_S, I2FInReady_S, F2FInReady_S : std_logic;

  signal F2IOutResult_D                                 : std_logic_vector(INT_WIDTH-1 downto 0);
  signal I2FOutResult_D, F2FOutResult_D                 : std_logic_vector(FP_WIDTH-1 downto 0);
  signal F2IOutStatus_D, I2FOutStatus_D, F2FOutStatus_D : rvStatus_t;
  signal F2IResult_D, I2FResult_D, F2FResult_D          : std_logic_vector(Z_DO'range);

  signal F2IOutTag_D, I2FOutTag_D, F2FOutTag_D : std_logic_vector(-1 downto 0);  --dummy

  signal F2IZext_S, I2FZext_S, F2FZext_S : std_logic;

  signal F2IOutValid_S, I2FOutValid_S, F2FOutValid_S : std_logic;

  signal Result_D                : std_logic_vector(Z_DO'range);
  signal Status_D                : rvStatus_t;
  signal Zext_S                  : std_logic;
  signal TagInt_D, TagIntPiped_D : std_logic_vector(TAG_WIDTH downto 0);
  signal OutValid_S              : std_logic;

begin  -- architecture parallel_paths

  -----------------------------------------------------------------------------
  -- Input side signals
  -----------------------------------------------------------------------------

  InReady_SO <= F2IInReady_S or I2FInReady_S or F2FInReady_S;

  F2IInValid_S <= InValid_SI and to_sl(Op_SI = F2I);
  I2FInValid_S <= InValid_SI and to_sl(Op_SI = I2F);
  F2FInValid_S <= InValid_SI and to_sl(Op_SI = F2F);

  -----------------------------------------------------------------------------
  -- Instances of parallel cast units
  -----------------------------------------------------------------------------

  g_f2i : if anySet(FORMATS.Active) and anySet(INTFORMATS.Active) generate

    i_fp_f2icasts : fp_f2icasts
      generic map (
        FORMATS    => FORMATS,
        INTFORMATS => INTFORMATS,
        LATENCY    => 0,
        TAG_WIDTH  => 0)
      port map (
        Clk_CI       => Clk_CI,
        Reset_RBI    => Reset_RBI,
        A_DI         => A_DI(FP_WIDTH-1 downto 0),
        ABox_SI      => ABox_SI,
        RoundMode_SI => RoundMode_SI,
        OpMod_SI     => OpMod_SI,
        SrcFmt_SI    => FpFmt_SI,
        DstFmt_SI    => IntFmt_SI,
        Tag_DI       => (others => '-'),
        InValid_SI   => F2IInValid_S,
        InReady_SO   => F2IInReady_S,
        Flush_SI     => '0',
        Z_DO         => F2IOutResult_D,
        Status_DO    => F2IOutStatus_D,
        Tag_DO       => F2IOutTag_D,
        Zext_SO      => F2IZext_S,
        OutValid_SO  => F2IOutValid_S,
        OutReady_SI  => OutReady_S);

    -- Extend as needed
    F2IResult_D(F2IOutResult_D'range)                          <= F2IOutResult_D;
    F2IResult_D(F2IResult_D'high downto F2IOutResult_D'high+1) <= (others => '1') when F2IZext_S = '0' else
                                                                  (others => '0');
  end generate g_f2i;
  g_nof2i : if not (anySet(FORMATS.Active) and anySet(INTFORMATS.Active)) generate

    F2IOutValid_S <= '0';
    F2IInReady_S  <= '0';

  end generate g_nof2i;


  g_i2f : if anySet(FORMATS.Active) and anySet(INTFORMATS.Active) generate

    i_fp_i2fcasts : fp_i2fcasts
      generic map (
        FORMATS    => FORMATS,
        INTFORMATS => INTFORMATS,
        LATENCY    => 0,
        TAG_WIDTH  => 0)
      port map (
        Clk_CI       => Clk_CI,
        Reset_RBI    => Reset_RBI,
        A_DI         => A_DI(INT_WIDTH-1 downto 0),
        RoundMode_SI => RoundMode_SI,
        OpMod_SI     => OpMod_SI,
        SrcFmt_SI    => IntFmt_SI,
        DstFmt_SI    => FpFmt_SI,
        Tag_DI       => (others => '-'),
        InValid_SI   => I2FInValid_S,
        InReady_SO   => I2FInReady_S,
        Flush_SI     => '0',
        Z_DO         => I2FOutResult_D,
        Status_DO    => I2FOutStatus_D,
        Tag_DO       => I2FOutTag_D,
        Zext_SO      => I2FZext_S,
        OutValid_SO  => I2FOutValid_S,
        OutReady_SI  => OutReady_S);

    -- Extend as needed
    I2FResult_D(I2FOutResult_D'range)                          <= I2FOutResult_D;
    I2FResult_D(I2FResult_D'high downto I2FOutResult_D'high+1) <= (others => '1') when I2FZext_S = '0' else
                                                                  (others => '0');

  end generate g_i2f;
  g_noi2f : if not (anySet(FORMATS.Active) and anySet(INTFORMATS.Active)) generate

    I2FOutValid_S <= '0';
    I2FInReady_S  <= '0';

  end generate g_noi2f;


  g_f2f : if anySet(FORMATS.Active) generate

    i_fp_f2fcasts : fp_f2fcasts
      generic map (
        FORMATS   => FORMATS,
        LATENCY   => 0,
        TAG_WIDTH => 0)
      port map (
        Clk_CI       => Clk_CI,
        Reset_RBI    => Reset_RBI,
        A_DI         => A_DI(FP_WIDTH-1 downto 0),
        ABox_SI      => ABox_SI,
        RoundMode_SI => RoundMode_SI,
        SrcFmt_SI    => FpFmt2_SI,
        DstFmt_SI    => FpFmt_SI,
        Tag_DI       => (others => '-'),
        InValid_SI   => F2FInValid_S,
        InReady_SO   => F2FInReady_S,
        Flush_SI     => '0',
        Z_DO         => F2FOutResult_D,
        Status_DO    => F2FOutStatus_D,
        Tag_DO       => F2FOutTag_D,
        Zext_SO      => F2FZext_S,
        OutValid_SO  => F2FOutValid_S,
        OutReady_SI  => OutReady_S);

    -- Extend as needed
    F2FResult_D(F2FOutResult_D'range)                          <= F2FOutResult_D;
    F2FResult_D(F2FResult_D'high downto F2FOutResult_D'high+1) <= (others => '1') when F2FZext_S = '0' else
                                                                  (others => '0');
  end generate g_f2f;
  g_nof2f : if not anySet(FORMATS.Active) generate

    F2FOutValid_S <= '0';
    F2FInReady_S  <= '0';

  end generate g_nof2f;


  -----------------------------------------------------------------------------
  -- Pipeline registers at the outputs of the unit
  -----------------------------------------------------------------------------

  Result_D <= F2IResult_D when F2IOutValid_S = '1' else
              I2FResult_D when I2FOutValid_S = '1' else
              F2FResult_D when F2FOutValid_S = '1' else
              (others => '-');

  Status_D <= F2IOutStatus_D when F2IOutValid_S = '1' else
              I2FOutStatus_D when I2FOutValid_S = '1' else
              F2FOutStatus_D when F2FOutValid_S = '1' else
              (others => '-');

  Zext_S <= F2IZext_S when F2IOutValid_S = '1' else
            I2FZext_S when I2FOutValid_S = '1' else
            F2FZext_S when F2FOutValid_S = '1' else
            '-';

  TagInt_D   <= Zext_S & Tag_DI;
  OutValid_S <= F2IOutValid_S or I2FOutValid_S or F2FOutValid_S;


  i_fp_pipe : fp_pipe
    generic map (
      WIDTH     => MAXWIDTH(FORMATS, INTFORMATS),
      LATENCY   => LATENCY,
      TAG_WIDTH => TAG_WIDTH+1)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      Result_DI      => Result_D,
      Status_DI      => Status_D,
      Tag_DI         => TagInt_D,
      InValid_SI     => OutValid_S,
      InReady_SO     => OutReady_S,
      Flush_SI       => Flush_SI,
      ResultPiped_DO => Z_DO,
      StatusPiped_DO => Status_DO,
      TagPiped_DO    => TagIntPiped_D,
      OutValid_SO    => OutValid_SO,
      OutReady_SI    => OutReady_SI);

  Zext_SO <= TagIntPiped_D(Tag_DO'high+1);
  Tag_DO  <= TagIntPiped_D(Tag_DO'range);


end architecture parallel_paths;
