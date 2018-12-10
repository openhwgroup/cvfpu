-------------------------------------------------------------------------------
-- Title      : FPU block containing units for NONCOMP operation group
-- Project    :
-------------------------------------------------------------------------------
-- File       : noncomp_block.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-04-05
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

entity noncomp_block is

  generic (
    FORMATS : activeFormats_t := (Active   => (FP32 to FP16ALT => true, others => false),
                                  Encoding => DEFAULTENCODING);

    UNITTYPES  : fmtUnitTypes_t := (others => PARALLEL);
    LATENCIES  : fmtNaturals_t  := (others => 0);
    GENVECTORS : boolean        := false;
    TAG_WIDTH  : natural        := 0);

  port (
    Clk_CI                    : in  std_logic;
    Reset_RBI                 : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI, B_DI, C_DI          : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
    ABox_SI, BBox_SI, CBox_SI : in  fmtLogic_t;
    RoundMode_SI              : in  rvRoundingMode_t;
    Op_SI                     : in  fpOp_t;
    OpMod_SI                  : in  std_logic;
    VectorialOp_SI            : in  std_logic;
    FpFmt_SI                  : in  fpFmt_t;
    Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI                : in  std_logic;
    InReady_SO                : out std_logic;
    Flush_SI                  : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO                      : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
    Status_DO                 : out rvStatus_t;
    Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO                   : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO               : out std_logic;
    OutReady_SI               : in  std_logic);

end entity noncomp_block;


architecture rtl of noncomp_block is

  -----------------------------------------------------------------------------
  -- Constant Definitions
  -----------------------------------------------------------------------------
  constant WIDTH : natural := MAXWIDTH(FORMATS);

  -- Number of active formats (for optimized output arbitration)
  constant NUMFMTS : natural := numActive(FORMATS);

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

  --! @brief Array of output values for each format
  --! @details Array of STD_LOGIC_VECTOR that hold a value for each \ref
  --! fpFmt_t "FPFMT_T"
  type fmtData_t is array (fpFmt_t) of std_logic_vector(Z_DO'range);
  type fmtTags_t is array (fpFmt_t) of std_logic_vector(Tag_DO'range);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Slice input side outputs for all formats
  signal FmtInReady_S : fmtLogic_t;

  -- Slice outputs for all formats
  signal FmtOutResult_D : fmtData_t;
  signal FmtOutStatus_D : fmtStatus_t;
  signal FmtOutTags_D   : fmtTags_t;
  signal FmtOutZext_S   : fmtLogic_t;
  signal FmtOutValid_S  : fmtLogic_t;

  -- Slice output side inputs for all formats
  signal FmtOutReady_S : fmtLogic_t;

  -- Slice output side as arrays for arbitration, only active formats
  signal FmtOutResult2d_D : fmtSlArray2d_t(fpFmt_t, Z_DO'range);
  signal ArbInResults_D   : slArray2d_t(0 to NUMFMTS-1, Z_DO'range);
  signal FmtOutTags2d_D   : fmtSlArray2d_t(fpFmt_t, TAG_WIDTH downto 0);
  signal ArbInStatus_D    : statusArray_t(0 to NUMFMTS-1);
  signal ArbInTags_D      : slArray2d_t(0 to NUMFMTS-1, TAG_WIDTH downto 0);
  signal ArbInValid_S     : std_logic_vector(0 to NUMFMTS-1);
  signal ArbInReady_S     : std_logic_vector(0 to NUMFMTS-1);

  -- Arbiter Valid output
  signal OutValid_S        : std_logic;
  signal OutputProcessed_S : std_logic;
  signal ArbOutTag_D       : std_logic_vector(TAG_WIDTH downto 0);

  -- Counter for RR arbiter
  signal RoundRobin_SP, RoundRobin_SN : std_logic_vector(clog2(NUMFMTS)-1 downto 0);


begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Block input readiness depends on whether chosen format is ready
  -----------------------------------------------------------------------------
  InReady_SO <= InValid_SI and FmtInReady_S(FpFmt_SI);

  -----------------------------------------------------------------------------
  -- Generate parallel or disabled slices for all active formats
  -----------------------------------------------------------------------------
  g_formatOps : for fmt in fpFmt_t generate
    g_activeOps : if FORMATS.Active(fmt) generate

      -- Enable signals are format-specific
      signal InValid_S : std_logic;

    begin

      -- Generate enable logic for the format: only valid if format selected
      InValid_S <= InValid_SI and to_sl(FpFmt_SI = fmt);

      -------------------------------------------------------------------------
      -- Generate format-specific subunits for float formats (parallel)
      -------------------------------------------------------------------------
      g_parallelOps : if UNITTYPES(fmt) = PARALLEL generate

        i_noncomp_fmt_slice : noncomp_fmt_slice
          generic map (
            EXP_BITS    => FORMATS.Encoding(fmt).ExpBits,
            MAN_BITS    => FORMATS.Encoding(fmt).ManBits,
            LATENCY     => LATENCIES(fmt),
            SLICE_WIDTH => WIDTH,
            GENVECTORS  => GENVECTORS,
            TAG_WIDTH   => TAG_WIDTH)
          port map (
            Clk_CI         => Clk_CI,
            Reset_RBI      => Reset_RBI,
            A_DI           => A_DI,
            B_DI           => B_DI,
            C_DI           => C_DI,
            ABox_SI        => ABox_SI(fmt),
            BBox_SI        => BBox_SI(fmt),
            CBox_SI        => CBox_SI(fmt),
            RoundMode_SI   => RoundMode_SI,
            Op_SI          => Op_SI,
            OpMod_SI       => OpMod_SI,
            VectorialOp_SI => VectorialOp_SI,
            Tag_DI         => Tag_DI,
            InValid_SI     => InValid_S,
            InReady_SO     => FmtInReady_S(fmt),
            Flush_SI       => Flush_SI,
            Z_DO           => FmtOutResult_D(fmt),
            Status_DO      => FmtOutStatus_D(fmt),
            Tag_DO         => FmtOutTags_D(fmt),
            Zext_SO        => FmtOutZext_S(fmt),
            OutValid_SO    => FmtOutValid_S(fmt),
            OutReady_SI    => FmtOutReady_S(fmt));

      end generate g_parallelOps;

      -------------------------------------------------------------------------
      -- Disable unused format outputs of merged ops, all use first format port
      -------------------------------------------------------------------------
      g_mergedOpsUnused : if UNITTYPES(fmt) = MERGED generate


        -- TODO: CURRENTLY NO MERGED NONCOMP OPS

      end generate g_mergedOpsUnused;


      -----------------------------------------------------------------------------
      -- Disable formats when they're set not to generate
      -----------------------------------------------------------------------------
      g_disabledOps : if UNITTYPES(fmt) = NONE generate
        -- Never ready to accept ops
        FmtInReady_S(fmt) <= '0';

        -- Disable output
        FmtOutResult_D(fmt) <= (others => '-');  -- don't care
        FmtOutStatus_D(fmt) <= (others => '-');  -- don't care
        FmtOutTags_D(fmt)   <= (others => '-');  -- dont' care
        FmtOutZext_S(fmt)   <= '-';              -- don't care
        FmtOutValid_S(fmt)  <= '0';              -- disabled

      end generate g_disabledOps;

    end generate g_activeOps;
  end generate g_formatOps;


  -------------------------------------------------------------------------
  -- Generate multiformat subunit for selected float formats (merged)
  -------------------------------------------------------------------------
  g_mergedOps : if anyMergedFormat(UNITTYPES, FORMATS) generate


    -- TODO: CURRENTLY NO MERGED NONCOMP OPS

  end generate g_mergedOps;


  -----------------------------------------------------------------------------
  -- Output Arbitration
  -----------------------------------------------------------------------------

  p_arbInputSide : process (all) is

    variable FmtResult_D : std_logic_vector(Z_DO'range);
    variable FmtTag_D    : std_logic_vector(TAG_WIDTH downto 0);

  begin  -- process p_arbInputSide

    -- change array types to proper 2d arrays - VHDL-93 fluff
    for fmt in fpFmt_t loop
      FmtResult_D := FmtOutResult_D(fmt);
      FmtTag_D    := FmtOutZext_S(fmt) & FmtOutTags_D(fmt);  -- add zext to tag

      set_row(FmtOutResult2d_D, fmt, FmtResult_D);
      set_row(FmtOutTags2d_D, fmt, FmtTag_D);
    end loop;  -- fmt

    -- Bring input data into arbiter format
    extract_active_rows(ArbInResults_D, FmtOutResult2d_D, FORMATS);
    extract_active_statuses(ArbInStatus_D, FmtOutStatus_D, FORMATS);
    extract_active_rows(ArbInTags_D, FmtOutTags2d_D, FORMATS);
    extract_active_logic(ArbInValid_S, FmtOutValid_S, FORMATS);

    -- Input side output comes from used formats. set others don't care
    FmtOutReady_S <= (others => '-');
    inject_active_logic(FmtOutReady_S, ArbInReady_S, FORMATS);

  end process p_arbInputSide;

  -- The arbiter
  i_fp_arbiter : fp_arbiter
    generic map (
      DATA_WIDTH => WIDTH,
      NUM_INPUTS => NUMFMTS,
      TAG_WIDTH  => TAG_WIDTH+1)
    port map (
      InResults_DI  => ArbInResults_D,
      InStatuses_DI => ArbInStatus_D,
      InTags_DI     => ArbInTags_D,
      InValid_SI    => ArbInValid_S,
      InReady_SO    => ArbInReady_S,
      Priorities_SI => RoundRobin_SP,
      OutResult_DO  => Z_DO,
      OutStatus_DO  => Status_DO,
      OutTag_DO     => ArbOutTag_D,
      OutValid_SO   => OutValid_S,
      OutReady_SI   => OutReady_SI,
      OutIdx_SO     => open);

  Zext_SO     <= ArbOutTag_D(ArbOutTag_D'high);
  Tag_DO      <= ArbOutTag_D(Tag_DO'range);
  OutValid_SO <= OutValid_S;

  -- The output will be read at the next clock iff both ready&valid are set
  OutputProcessed_S <= OutValid_S and OutReady_SI;

  -- Round Robin Arbiter Counter
  RoundRobin_SN <= std_logic_vector(unsigned(RoundRobin_SP)+1);

  p_rrCntr : process (Clk_CI, Reset_RBI) is
  begin  -- process p_rrCntr
    if Reset_RBI = '0' then             -- asynchronous reset (active low)
      RoundRobin_SP <= (others => '0');
    elsif Clk_CI'event and Clk_CI = '1' then  -- rising clock edge
      if OutputProcessed_S = '1' then   -- advance when output is read
        RoundRobin_SP <= RoundRobin_SN;
      end if;
    end if;
  end process p_rrCntr;


end architecture rtl;
