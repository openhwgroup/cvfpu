-------------------------------------------------------------------------------
-- Title      : Multiformat Division and Square Root
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_divsqrt_multi.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-04-08
-- Last update: 2018-04-19
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
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

entity fp_divsqrt_multi is

  generic (
    FORMATS : activeFormats_t := (Active   => (FP32 to FP16ALT => true, others => false),
                                  Encoding => DEFAULTENCODING);

    LATENCY   : natural := 0;
    TAG_WIDTH : natural := 0);

  port (
    Clk_CI           : in  std_logic;
    Reset_RBI        : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI, B_DI       : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
    ABox_SI, BBox_SI : in  fmtLogic_t;
    RoundMode_SI     : in  rvRoundingMode_t;
    Op_SI            : in  fpOp_t;
    OpMod_SI         : in  std_logic;
    FpFmt_SI         : in  fpFmt_t;
    Tag_DI           : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI       : in  std_logic;
    InReady_SO       : out std_logic;
    Flush_SI         : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO             : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
    Status_DO        : out rvStatus_t;
    Tag_DO           : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO          : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO      : out std_logic;
    OutReady_SI      : in  std_logic);

end entity fp_divsqrt_multi;


architecture iterative_lei of fp_divsqrt_multi is

  -----------------------------------------------------------------------------
  -- Constant Definitions
  -----------------------------------------------------------------------------
  constant FP_WIDTH : natural := MAXWIDTH(FORMATS);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Unit input side
  signal InDivValid_S, InSqrtValid_S, InReady_S : std_logic;
  signal Fmt_S                                  : std_logic_vector(1 downto 0);
  signal A_D, B_D                               : std_logic_vector(63 downto 0);

  -- Unit output side
  signal OutResult_D            : std_logic_vector(63 downto 0);
  signal OutStatusSlv_D         : std_logic_vector(4 downto 0);
  signal OutStatus_D            : rvStatus_t;
  signal OutTag_DP              : std_logic_vector(TAG_WIDTH-1 downto 0);
  signal OutZext_S              : std_logic;
  signal OutValid_S, OutReady_S : std_logic;

  -- Pre-pipelining
  signal Result_D : std_logic_vector(Z_DO'range);
  signal Status_D : rvStatus_t;

  -----------------------------------------------------------------------------
  -- Component Declarations
  -----------------------------------------------------------------------------
  component div_sqrt_top_mvp is
    port (
      Clk_CI           : in  std_logic;
      Rst_RBI          : in  std_logic;
      Div_start_SI     : in  std_logic;
      Sqrt_start_SI    : in  std_logic;
      Operand_a_DI     : in  std_logic_vector(63 downto 0);
      Operand_b_DI     : in  std_logic_vector(63 downto 0);
      RM_SI            : in  std_logic_vector(2 downto 0);
      Precision_ctl_SI : in  std_logic_vector(5 downto 0);
      Format_sel_SI    : in  std_logic_vector(1 downto 0);
      Kill_SI          : in  std_logic;
      Result_DO        : out std_logic_vector(63 downto 0);
      Fflags_SO        : out std_logic_vector(4 downto 0);
      Ready_SO         : out std_logic;
      Done_SO          : out std_logic);
  end component div_sqrt_top_mvp;

begin  -- architecture iterative_lei

  -----------------------------------------------------------------------------
  -- Input side signals
  -----------------------------------------------------------------------------

  -- Format encoding of unit
  with FpFmt_SI select
    Fmt_S <=
    "00" when FP32,
    "01" when FP64,
    "10" when FP16,
    "11" when FP16ALT,
    "10" when others;                   -- map fp8 to fp16

  -- Map FP8 onto FP16
  A_D <= std_logic_vector(resize(unsigned(A_DI), 64)) when FpFmt_SI /= FP8 else
         std_logic_vector(resize(unsigned(A_DI), 64) sll 8);

  B_D <= std_logic_vector(resize(unsigned(B_DI), 64)) when FpFmt_SI /= FP8 else
         std_logic_vector(resize(unsigned(B_DI), 64) sll 8);

  -- Operation is only started when our extra pipestage in the end is ready for
  -- more data
  InDivValid_S  <= InValid_SI and to_sl(Op_SI = DIV) and OutReady_S;
  InSqrtValid_S <= InValid_SI and to_sl(Op_SI = SQRT) and OutReady_S;

  -- Upstream ready depends on whether the unit and downstream are ready
  InReady_SO <= OutReady_S and InReady_S;

  -----------------------------------------------------------------------------
  -- Store tag until the divider is done
  -----------------------------------------------------------------------------
  p_tagBuffer : process (Clk_CI, Reset_RBI) is
  begin  -- process p_tagBuffer
    if Reset_RBI = '0' then             -- asynchronous reset (active low)
      OutTag_DP <= (others => '0');
    elsif Clk_CI'event and Clk_CI = '1' then  -- rising clock edge
      if (InDivValid_S or InSqrtValid_S) = '1' then
        OutTag_DP <= Tag_DI;
      end if;
    end if;
  end process p_tagBuffer;

  -----------------------------------------------------------------------------
  -- Instance of multifmt div/sqrt unit
  -----------------------------------------------------------------------------

  i_fp_divsqrt : div_sqrt_top_mvp
    port map (
      Clk_CI           => Clk_CI,
      Rst_RBI          => Reset_RBI,
      Div_start_SI     => InDivValid_S,
      Sqrt_start_SI    => InSqrtValid_S,
      Operand_a_DI     => A_D,
      Operand_b_DI     => B_D,
      RM_SI            => to_slv(RoundMode_SI),
      Precision_ctl_SI => (others => '0'),  -- turn off for now
      Format_sel_SI    => Fmt_S,
      Kill_SI          => Flush_SI,              -- TODO ADD KILL
      Result_DO        => OutResult_D,
      Fflags_SO        => OutStatusSlv_D,
      Ready_SO         => InReady_S,
      Done_SO          => OutValid_S);

  OutStatus_D <= to_rvStatus(OutStatusSlv_D);

  -----------------------------------------------------------------------------
  -- Pipeline registers at the outputs of the unit
  -----------------------------------------------------------------------------

  Result_D <= std_logic_vector(resize(unsigned(OutResult_D), Z_DO'length));

  Status_D <= OutStatus_D;

  -- At least one pipleline register is required as the unit does not have a
  -- backpressure path. The register will catch outputs that cannot be processed
  -- downstream
  i_fp_pipe : fp_pipe
    generic map (
      WIDTH     => FP_WIDTH,
      LATENCY   => maximum(1, LATENCY),
      TAG_WIDTH => TAG_WIDTH)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      Result_DI      => Result_D,
      Status_DI      => Status_D,
      Tag_DI         => OutTag_DP,
      InValid_SI     => OutValid_S,
      InReady_SO     => OutReady_S,
      Flush_SI       => Flush_SI,
      ResultPiped_DO => Z_DO,
      StatusPiped_DO => Status_DO,
      TagPiped_DO    => Tag_DO,
      OutValid_SO    => OutValid_SO,
      OutReady_SI    => OutReady_SI);

  Zext_SO <= '0';                       -- always NaN-box


end architecture iterative_lei;
