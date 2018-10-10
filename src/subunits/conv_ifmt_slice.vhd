-------------------------------------------------------------------------------
-- Title      : Floating-Point Format-Specific Slice
-- Project    :
-------------------------------------------------------------------------------
-- File       : conv_fmt_slice.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-24
-- Last update: 2018-10-10
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Parametric slice containing all floating-point operations that
--              operate on a singular format.
--              Supported operations from fpnew_pkg.fpOp:
--              - SGNJ
--              - MINMAX
--              - CMP
--              - CLASS
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

--! @brief Floating-Point Format-Specific Conversion Slice
--! @details Converts from SLICE_FMT to all other enabled formats
entity conv_ifmt_slice is

  generic (
    INTFORMATS : activeIntFormats_t := (Active => (others => true),
                                        Length => INTFMTLENGTHS);

    FPENCODING  : fpFmtEncoding_t := DEFAULTENCODING(FP32);
    LATENCY     : natural         := 0;
    SLICE_WIDTH : natural         := 64;
    GENVECTORS  : boolean         := false;
    TAG_WIDTH   : natural         := 0);

  port (
    Clk_CI         : in  std_logic;
    Reset_RBI      : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI           : in  std_logic_vector(SLICE_WIDTH-1 downto 0);
    ABox_SI        : in  std_logic;
    RoundMode_SI   : in  rvRoundingMode_t;
    Op_SI          : in  fpOp_t;
    OpMod_SI       : in  std_logic;
    IntFmt_SI      : in  intFmt_t;
    VectorialOp_SI : in  std_logic;
    Tag_DI         : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI     : in  std_logic;
    InReady_SO     : out std_logic;
    Flush_SI       : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO           : out std_logic_vector(SLICE_WIDTH-1 downto 0);
    Status_DO      : out rvStatus_t;
    Tag_DO         : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO        : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO    : out std_logic;
    OutReady_SI    : in  std_logic);

end entity conv_ifmt_slice;


architecture rtl of conv_ifmt_slice is

  -----------------------------------------------------------------------------
  -- Constant Definitions
  -----------------------------------------------------------------------------
  -- The width of the FP format
  constant FP_WIDTH : natural := WIDTH(FPENCODING);

  -- The number of parallel lanes the slice can hold
  constant NUMLANES : natural := SLICE_WIDTH/FP_WIDTH;

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

  -- Vectors of results for the lanes
  type laneTags_t is array (0 to NUMLANES-1) of std_logic_vector(TAG_WIDTH downto 0);

  -- Outputs
  type laneResults_t is array (0 to NUMLANES-1) of std_logic_vector(SLICE_WIDTH-1 downto 0);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Operation Selection
  signal VectorialOp_S : std_logic;

  signal ABox_S : std_logic;

  signal F2ILaneOutValid_S, I2FLaneOutValid_S : std_logic_vector(0 to NUMLANES-1);
  signal F2ILaneInReady_S, I2FLaneInReady_S   : std_logic_vector(0 to NUMLANES-1);

  -- Outputs from all dst-format slices
  signal F2ILaneResult_D, I2FLaneResult_D : laneResults_t;
  signal F2ILaneStatus_D, I2FLaneStatus_D : statusArray_t(0 to NUMLANES-1);
  signal F2ILaneZext_S, I2FLaneZext_S     : std_logic_vector(0 to NUMLANES-1);

  -- Output to Pipeline
  signal FinalResult_D : std_logic_vector(Z_DO'range);
  signal FinalStatus_D : rvStatus_t;
  signal FinalZext_S   : std_logic;

  signal TagInt_D, TagPiped_D         : std_logic_vector(TAG_WIDTH downto 0);
  signal PipeInReady_S, PipeInValid_S : std_logic;


begin  -- architecture rtl

  -- Upstream Ready is signalled if first lane can accept instructions
  InReady_SO <= F2ILaneInReady_S(0) and I2FLaneInReady_S(0);

  -- Mask vectorial enable if we don't have vector support
  VectorialOp_S <= VectorialOp_SI and to_sl(GENVECTORS);

  ABox_S <= ABox_SI or VectorialOp_S;

  -----------------------------------------------------------------------------
  -- Generate multiformat slices
  -----------------------------------------------------------------------------
  g_sliceLanes : for i in 0 to NUMLANES-1 generate

    signal F2IDataIn_D : std_logic_vector(FP_WIDTH-1 downto 0);
    signal I2FDataIn_D : std_logic_vector(MAXWIDTH(INTFORMATS)-1 downto 0);

    signal F2IInValid_S, I2FInValid_S : std_logic;

    signal F2IResult_D                  : std_logic_vector(MAXWIDTH(INTFORMATS)-1 downto 0);
    signal I2FResult_D                  : std_logic_vector(FP_WIDTH-1 downto 0);
    signal F2IStatus_D, I2FStatus_D     : rvStatus_t;
    signal F2IOutValid_S, F2IOutReady_S : std_logic;
    signal I2FOutValid_S, I2FOutReady_S : std_logic;

  begin
    -- Generate instances (widest always, others only for vectors)
    g_laneInst : if i = 0 or GENVECTORS generate

      -- Input handshake: first lane always on, others only for vectorial ops
      F2IInValid_S <= InValid_SI and (to_sl(i = 0) or VectorialOp_S) and to_sl(Op_SI = F2I);
      I2FInValid_S <= InValid_SI and (to_sl(i = 0) or VectorialOp_S) and to_sl(Op_SI = I2F);

      -- Input data
      F2IDataIn_D <= A_DI((i+1)*FP_WIDTH-1 downto i*FP_WIDTH);
      I2FDataIn_D <= std_logic_vector(unsigned(A_DI) srl i*FP_WIDTH);

      i_fp_f2icasts_fmt : fp_f2icasts_fmt
        generic map (
          SRCENCODING => FPENCODING,
          INTFORMATS  => INTFORMATS,
          LATENCY     => 0,
          TAG_WIDTH   => 1)
        port map (
          Clk_CI       => Clk_CI,
          Reset_RBI    => Reset_RBI,
          A_DI         => F2IDataIn_D,
          ABox_SI      => ABox_S,
          RoundMode_SI => RoundMode_SI,
          OpMod_SI     => OpMod_SI,
          DstFmt_SI    => IntFmt_SI,
          Tag_DI       => "-",
          InValid_SI   => F2IInValid_S,
          InReady_SO   => F2ILaneInReady_S(i),
          Flush_SI     => Flush_SI,
          Z_DO         => F2IResult_D,
          Status_DO    => F2IStatus_D,
          Tag_DO       => open,
          Zext_SO      => F2ILaneZext_S(i),
          OutValid_SO  => F2IOutValid_S,
          OutReady_SI  => F2IOutReady_S);


      i_fp_i2fcasts_fmt : fp_i2fcasts_fmt
        generic map (
          DSTENCODING => FPENCODING,
          INTFORMATS  => INTFORMATS,
          LATENCY     => 0,
          TAG_WIDTH   => 1)
        port map (
          Clk_CI       => Clk_CI,
          Reset_RBI    => Reset_RBI,
          A_DI         => I2FDataIn_D,
          RoundMode_SI => RoundMode_SI,
          OpMod_SI     => OpMod_SI,
          SrcFmt_SI    => IntFmt_SI,
          Tag_DI       => "-",
          InValid_SI   => I2FInValid_S,
          InReady_SO   => I2FLaneInReady_S(i),
          Flush_SI     => Flush_SI,
          Z_DO         => I2FResult_D,
          Status_DO    => I2FStatus_D,
          Tag_DO       => open,
          Zext_SO      => I2FLaneZext_S(i),
          OutValid_SO  => I2FOutValid_S,
          OutReady_SI  => I2FOutReady_S);


      p_extendRes : process (all) is
        variable F2IResult, I2FResult : std_logic_vector(SLICE_WIDTH-1 downto 0);
      begin  -- process p_extendRes

        F2IResult := (others => not F2ILaneZext_S(i));
        I2FResult := (others => not I2FLaneZext_S(i));

        F2IResult(F2IResult_D'range) := F2IResult_D;
        I2FResult(I2FResult_D'range) := I2FResult_D;

        F2ILaneResult_D(i) <= F2IResult;
        I2FLaneResult_D(i) <= I2FResult;

      end process p_extendRes;


      -- Generate the ready input for this lane based on downstream ready:
      -- First lane follows global ready, other lanes only for vectorial ops
      F2IOutReady_S <= PipeInReady_S and (to_sl(i = 0) or VectorialOp_S);
      I2FOutReady_S <= PipeInReady_S and (to_sl(i = 0) or VectorialOp_S);

      -- Upper lanes are only used when there is a vectorial op
      F2ILaneOutValid_S(i) <= F2IOutValid_S and (to_sl(i = 0) or VectorialOp_S);
      I2FLaneOutValid_S(i) <= I2FOutValid_S and (to_sl(i = 0) or VectorialOp_S);

      -- Silence status when result not used
      F2ILaneStatus_D(i) <= F2IStatus_D when F2ILaneOutValid_S(i) = '1' else
                            (others => '0');
      I2FLaneStatus_D(i) <= I2FStatus_D when I2FLaneOutValid_S(i) = '1' else
                            (others => '0');
    end generate g_laneInst;

    g_laneBypass : if (i /= 0 and not GENVECTORS) generate
      F2ILaneInReady_S(i)  <= '0';
      I2FLaneInReady_S(i)  <= '0';
      F2ILaneResult_D(i)   <= (others => not F2ILaneZext_S(0));
      I2FLaneResult_D(i)   <= (others => not I2FLaneZext_S(0));
      F2ILaneStatus_D(i)   <= (others => '0');
      I2FLaneStatus_D(i)   <= (others => '0');
      F2ILaneOutValid_S(i) <= '0';
      I2FLaneOutValid_S(i) <= '0';
    end generate g_laneBypass;
  end generate g_sliceLanes;


  -----------------------------------------------------------------------------
  -- Result Assembly
  -----------------------------------------------------------------------------
  p_assembleResult : process (all) is

    variable F2IResult, F2IVecResult, I2FResult, I2FVecResult : std_logic_vector(SLICE_WIDTH-1 downto 0);
    variable F2IStatus, I2FStatus                             : rvStatus_t;
  begin  -- process p_assembleResult

    F2IResult    := (others => not F2ILaneZext_S(0));
    F2IVecResult := (others => not F2ILaneZext_S(0));
    I2FResult    := (others => not I2FLaneZext_S(0));
    I2FVecResult := (others => not I2FLaneZext_S(0));
    I2FStatus    := combined_status(I2FLaneStatus_D);

    for i in 0 to NUMLANES-1 loop

      I2FVecResult((i+1)*FP_WIDTH-1 downto i*FP_WIDTH) := I2FLaneResult_D(i)(FP_WIDTH-1 downto 0);

      F2IVecResult((i+1)*FP_WIDTH-1 downto i*FP_WIDTH) := F2ILaneResult_D(i)(FP_WIDTH-1 downto 0);

    end loop;  -- i

    if VectorialOp_S = '1' then
      F2IResult := F2IVecResult;
      F2IStatus := combined_status(F2ILaneStatus_D);
      I2FResult := I2FVecResult;
      I2FStatus := combined_status(I2FLaneStatus_D);
    else
      F2IResult(MAXWIDTH(INTFORMATS)-1 downto 0) := F2ILaneResult_D(0);
      F2IStatus                                  := F2ILaneStatus_D(0);
      I2FResult                                  := I2FLaneResult_D(0);
      I2FStatus                                  := I2FLaneStatus_D(0);
    end if;

    if Op_SI = F2I then
      FinalResult_D <= F2IResult;
      FinalStatus_D <= F2IStatus;
      FinalZext_S   <= F2ILaneZext_S(0);
      PipeInValid_S <= F2ILaneOutValid_S(0);
    else
      FinalResult_D <= I2FResult;
      FinalStatus_D <= I2FStatus;
      FinalZext_S   <= I2FLaneZext_S(0);
      PipeInValid_S <= I2FLaneOutValid_S(0);
    end if;

  end process p_assembleResult;

-----------------------------------------------------------------------------
-- Result Selection
-----------------------------------------------------------------------------

  TagInt_D <= FinalZext_S & Tag_DI;

-----------------------------------------------------------------------------
-- Output Pipeline
-----------------------------------------------------------------------------
  i_fp_pipe : fp_pipe
    generic map (
      WIDTH     => SLICE_WIDTH,
      LATENCY   => LATENCY,
      TAG_WIDTH => TAG_WIDTH+1)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      Result_DI      => FinalResult_D,
      Status_DI      => FinalStatus_D,
      Tag_DI         => TagInt_D,
      InValid_SI     => PipeInValid_S,
      InReady_SO     => PipeInReady_S,
      Flush_SI       => Flush_SI,
      ResultPiped_DO => Z_DO,
      StatusPiped_DO => Status_DO,
      TagPiped_DO    => TagPiped_D,
      OutValid_SO    => OutValid_SO,
      OutReady_SI    => OutReady_SI);


-- Separate the sign-extension information from the tag again
  Zext_SO <= TagPiped_D(TAG_WIDTH);
  Tag_DO  <= TagPiped_D(Tag_DO'range);



end architecture rtl;
