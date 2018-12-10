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
entity conv_fmt_slice is

  generic (
    FORMATS : activeFormats_t := (Active   => (FP32 to FP16ALT => true, others => false),
                                  Encoding => DEFAULTENCODING);

    CPKFORMATS : fmtBooleans_t := (FP64 => true, FP32 => true, others => false);

    SRCFMT      : fpFmt_t := FP32;
    LATENCY     : natural := 0;
    SLICE_WIDTH : natural := 64;
    GENVECTORS  : boolean := false;
    TAG_WIDTH   : natural := 0);

  port (
    Clk_CI                    : in  std_logic;
    Reset_RBI                 : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI, B_DI, C_DI          : in  std_logic_vector(SLICE_WIDTH-1 downto 0);
    ABox_SI, BBox_SI, CBox_SI : in  std_logic;
    RoundMode_SI              : in  rvRoundingMode_t;
    Op_SI                     : in  fpOp_t;
    OpMod_SI                  : in  std_logic;
    FpFmt_SI                  : in  fpFmt_t;  -- the target format
    VectorialOp_SI            : in  std_logic;
    Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI                : in  std_logic;
    InReady_SO                : out std_logic;
    Flush_SI                  : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO                      : out std_logic_vector(SLICE_WIDTH-1 downto 0);
    Status_DO                 : out rvStatus_t;
    Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO                   : out std_logic;
    OutValid_SO               : out std_logic;
    OutReady_SI               : in  std_logic);

end entity conv_fmt_slice;


architecture rtl of conv_fmt_slice is

  -----------------------------------------------------------------------------
  -- Constant Definitions
  -----------------------------------------------------------------------------
  -- The width of the FP format
  constant SRCFMT_WIDTH : natural := WIDTH(SRCFMT, FORMATS);

  -- The number of parallel lanes the slice can hold
  constant NUMSRCENTRIES : natural := SLICE_WIDTH/SRCFMT_WIDTH;
  constant NUMSRCLANES : natural := maximum(NUMSRCENTRIES,
                                            2*to_integer(CPKFORMATS(SRCFMT)));

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

  -- Inputs
  type sourceEntries_t is array (0 to NUMSRCLANES-1) of std_logic_vector(SRCFMT_WIDTH-1 downto 0);


  -- Vectors of results for the lanes

--   type laneTags_t is array (0 to NUMLANES-1) of std_logic_vector(TAGINT_WIDTH-1 downto 0);

  -- Outputs
  type fmtResults_t is array (fpFmt_t) of std_logic_vector(SLICE_WIDTH-1 downto 0);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  signal SrcEntries_D : sourceEntries_t;
  signal FmtInReady_S : fmtLogic_t;

  -- Operation Selection
  signal VectorialOp_S    : std_logic;
  signal IsCPKOp_S        : boolean;
  signal IsUpperVecCast_S : boolean;

  -- Outputs from all dst-format slices
  signal FmtResults_D : fmtResults_t;
  signal FmtStatus_D  : fmtStatus_t;

  -- Output to Pipeline
  signal FinalResult_D : std_logic_vector(Z_DO'range);
  signal FinalStatus_D : rvStatus_t;

  signal TagInt_D, TagPiped_D         : std_logic_vector(TAG_WIDTH downto 0);
  signal PipeInReady_S, PipeInValid_S : std_logic;
  signal FmtOutValid_S                : fmtLogic_t;
  signal FormatZext_S                 : fmtLogic_t;

begin  -- architecture rtl

  -- Upstream Ready is signalled if first lane can accept instructions
  InReady_SO <= FmtInReady_S(FpFmt_SI);

  -- Mask vectorial enable if we don't have vector support
  VectorialOp_S    <= VectorialOp_SI and to_sl(GENVECTORS);
  IsCPKOp_S        <= (Op_SI = CPKAB or Op_SI = CPKCD);
  IsUpperVecCast_S <= VectorialOp_S = '1' and Op_SI = F2F and OpMod_SI = '1';

  -----------------------------------------------------------------------------
  -- Input side signals
  -----------------------------------------------------------------------------
  p_inputEntries : process (all)
  begin  -- process p_inputEntries
    for i in 0 to NUMSRCENTRIES-1 loop
      SrcEntries_D(i) <= A_DI((i+1)*SRCFMT_WIDTH-1 downto i*SRCFMT_WIDTH);
    end loop;  -- i
  end process p_inputEntries;

  -----------------------------------------------------------------------------
  -- Generate conversion units for each destination format
  -----------------------------------------------------------------------------
  g_destFmt : for fmt in fpFmt_t generate

    -- CONSTANTS
    constant DSTFMT_WIDTH  : natural := WIDTH(fmt, FORMATS);
    constant NUMDSTENTRIES : natural := SLICE_WIDTH/DSTFMT_WIDTH;
    constant ISUPCAST      : boolean := SRCFMT_WIDTH < DSTFMT_WIDTH;
    constant ISDOWNCAST    : boolean := SRCFMT_WIDTH > DSTFMT_WIDTH;

    -- Types
    type resultEntries_t is array (natural range <>) of std_logic_vector(DSTFMT_WIDTH-1 downto 0);

    -- Signals
    signal LaneInReady_S : std_logic_vector(0 to NUMSRCLANES-1);

    signal DstResults_D : resultEntries_t(0 to NUMSRCLANES-1);

    signal LaneStatus_D   : statusArray_t(0 to NUMSRCLANES-1);
    signal LaneOutValid_S : std_logic_vector(0 to NUMSRCLANES-1);
    signal LaneZext_S     : std_logic_vector(0 to NUMSRCLANES-1);

  begin

    FmtInReady_S(fmt) <= LaneInReady_S(0);

    g_srcFmtLanes : for i in 0 to NUMSRCLANES-1 generate
      g_laneInst : if (i = 0 or GENVECTORS) and FORMATS.Active(fmt) generate

        signal Input_D         : std_logic_vector(SRCFMT_WIDTH-1 downto 0);
        signal ABox_S, InBox_S : std_logic;
        signal InValid_S       : std_logic;

        signal OpStatus_D             : rvStatus_t;
        signal OutValid_S, OutReady_S : std_logic;

      begin

        -- Handle Inputs for vectors and CPK
        Input_D <= B_DI(Input_D'range) when i = 1 and IsCPKOp_S else
                   SrcEntries_D(i);
        ABox_S  <= ABox_SI or VectorialOp_S;
        InBox_S <= BBox_SI when i = 1 and IsCPKOp_S else
                   ABox_S;

        -- Input handshake: first lane always on, others only for vectorial ops
        InValid_S <= InValid_SI and (to_sl(i = 0) or VectorialOp_S);

        -- Instantiate conversions
        i_convToFmt : fp_f2fcasts_fmt
          generic map (
            SRCENCODING => FORMATS.Encoding(SRCFMT),
            DSTENCODING => FORMATS.Encoding(fmt),
            LATENCY     => 0,
            TAG_WIDTH   => 1)
          port map (
            Clk_CI       => Clk_CI,
            Reset_RBI    => Reset_RBI,
            A_DI         => Input_D,
            ABox_SI      => InBox_S,
            RoundMode_SI => RoundMode_SI,
            Tag_DI       => "-",
            InValid_SI   => InValid_S,
            InReady_SO   => LaneInReady_S(i),
            Flush_SI     => Flush_SI,
            Z_DO         => DstResults_D(i),
            Status_DO    => OpStatus_D,
            Tag_DO       => open,
            Zext_SO      => LaneZext_S(i),
            OutValid_SO  => OutValid_S,
            OutReady_SI  => OutReady_S);

        -- Generate the ready input for this lane based on downstream ready:
        -- First lane follows global ready, other lanes only for vectorial ops
        OutReady_S <= PipeInReady_S and (to_sl(i = 0) or VectorialOp_S);

        -- Upper lanes are only used when there is a vectorial op
        LaneOutValid_S(i) <= OutValid_S and (to_sl(i = 0) or VectorialOp_S);

        -- Silence status when result not used
        LaneStatus_D(i) <= OpStatus_D when LaneOutValid_S(i) = '1' else
                           (others => '0');
      end generate g_laneInst;

      g_laneBypass : if (i /= 0 and not GENVECTORS) or not FORMATS.Active(fmt) generate
        LaneInReady_S(i)  <= '0';
        DstResults_D(i)   <= (others => not LaneZext_S(0));
        LaneStatus_D(i)   <= (others => '0');
        LaneOutValid_S(i) <= '0';
      end generate g_laneBypass;
    end generate g_srcFmtLanes;

    -- Handle Output Assembly
    p_assembleResult : process (all) is
      variable CPKResult, VecResult, Result : resultEntries_t(0 to NUMDSTENTRIES-1);
      variable CPKOffset                    : natural;
      variable CPKStatus, VecStatus, Status : statusArray_t(0 to NUMDSTENTRIES-1);
    begin  -- process p_assembleResult

      -- Default assignments
      Result    := (others => (others => not LaneZext_S(0)));
      CPKResult := (others => (others => not LaneZext_S(0)));
      VecResult := (others => (others => not LaneZext_S(0)));
      CPKOffset := 0;
      Status    := (others => (others => '0'));
      CPKStatus := (others => (others => '0'));
      VecStatus := (others => (others => '0'));

      -- CPK
      if GENVECTORS and NUMDSTENTRIES>1 then
        -- inserts two entries depending on offset: A=0, B=2, C=4, D=6
        if NUMDSTENTRIES > 2 then
          CPKOffset := CPKOffset + 2*to_integer(OpMod_SI);
        end if;
        if NUMDSTENTRIES > 4 then
          CPKOffset := CPKOffset + 4*to_integer(Op_SI = CPKCD);
        end if;

        for i in 0 to NUMDSTENTRIES-1 loop
          -- CPK merges into C_DI
          CPKResult(i) := C_DI((i+1)*DSTFMT_WIDTH-1 downto i*DSTFMT_WIDTH);
        end loop;  -- i

        -- CPK insertions
        CPKResult(CPKOffset) := DstResults_D(0);
        CPKStatus(0)         := LaneStatus_D(0);
        if NUMDSTENTRIES > 1 then
          CPKResult(CPKOffset+1) := DstResults_D(1);
          CPKStatus(1)           := LaneStatus_D(1);
        end if;
      end if;

      -- Vectors
      if GENVECTORS then
        for i in 0 to NUMDSTENTRIES-1 loop
          -- Vectorial casts merge into B_DI
          VecResult(i) := B_DI((i+1)*DSTFMT_WIDTH-1 downto i*DSTFMT_WIDTH);
        end loop;

        -- upcasts have fewer entries in dst than in src
        if ISUPCAST then
          for i in 0 to NUMDSTENTRIES-1 loop
            if IsUpperVecCast_S then
              VecResult(i) := DstResults_D(i+NUMSRCENTRIES/2);
              VecStatus(i) := LaneStatus_D(i+NUMSRCENTRIES/2);
            else
              VecResult(i) := DstResults_D(i);
              VecStatus(i) := LaneStatus_D(i);
            end if;
          end loop;  -- i
        -- downcasts have more entries in dst than in src
        elsif ISDOWNCAST then
          for i in 0 to NUMSRCENTRIES-1 loop
            if IsUpperVecCast_S then
              VecResult(i+NUMDSTENTRIES/2) := DstResults_D(i);
            else
              VecResult(i) := DstResults_D(i);
            end if;
            VecStatus(i) := LaneStatus_D(i);
          end loop;
        -- samecasts don't have upper cast
        else
          for i in 0 to NUMDSTENTRIES-1 loop
            VecResult(i) := DstResults_D(i);
            VecStatus(i) := LaneStatus_D(i);
          end loop;  -- i
        end if;
      end if;

      if IsCPKOp_S then
        Result := CPKResult;
        Status := CPKStatus;
      elsif VectorialOp_S = '1' then
        Result := VecResult;
        Status := VecStatus;
      -- Scalars
      else
        Result(0) := DstResults_D(0);
        Status(0) := LaneStatus_D(0);
      end if;

      -- pack to dest vector
      for i in 0 to NUMDSTENTRIES-1 loop
        FmtResults_D(fmt)((i+1)*DSTFMT_WIDTH-1 downto i*DSTFMT_WIDTH) <= Result(i);
      end loop;  -- i

      FmtStatus_D(fmt) <= combined_status(Status);

    end process p_assembleResult;

    FmtOutValid_S(fmt) <= LaneOutValid_S(0);
    FormatZext_S(fmt)  <= LaneZext_S(0);

  end generate g_destFmt;


  -----------------------------------------------------------------------------
  -- Result Selection
  -----------------------------------------------------------------------------
  FinalResult_D <= FmtResults_D(FpFmt_SI);
  FinalStatus_D <= FmtStatus_D(FpFmt_SI);
  TagInt_D      <= FormatZext_S(FpFmt_SI) & Tag_DI;
  PipeInValid_S <= FmtOutValid_S(FpFmt_SI);

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
