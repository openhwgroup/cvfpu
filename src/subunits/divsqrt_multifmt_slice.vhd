-------------------------------------------------------------------------------
-- Title      : Floating-Point Multiformat Conversion Slice
-- Project    :
-------------------------------------------------------------------------------
-- File       : divsqrt_multifmt_slice.vhd
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
--              - F2I
--              - I2F
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

--! @brief Floating-Point Multiformat Conversion Slice
--! @details Parametric slice containing all floating-point operations that
--! operate on a singular format.
--! Supported operations from fpnew_pkg.fpOp:
--! - F2I
--! - I2F
--! - F2F
entity divsqrt_multifmt_slice is

  generic (
    FORMATS : activeFormats_t := (Active   => (FP32 to FP16ALT => true, others => false),
                                  Encoding => DEFAULTENCODING);

    LATENCIES   : fmtNaturals_t := (others => 0);
    SLICE_WIDTH : natural       := 64;
    GENVECTORS  : boolean       := false;
    TAG_WIDTH   : natural       := 0);

  port (
    Clk_CI                    : in  std_logic;
    Reset_RBI                 : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI, B_DI, C_DI          : in  std_logic_vector(SLICE_WIDTH-1 downto 0);
    RoundMode_SI              : in  rvRoundingMode_t;
    ABox_SI, BBox_SI, CBox_SI : in  fmtLogic_t;
    Op_SI                     : in  fpOp_t;
    OpMod_SI                  : in  std_logic;
    FpFmt_SI                  : in  fpFmt_t;
    VectorialOp_SI            : in  std_logic;
    Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    PrecCtl_SI                : in  std_logic_vector(6 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI                : in  std_logic;
    InReady_SO                : out std_logic;
    Flush_SI                  : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO                      : out std_logic_vector(SLICE_WIDTH-1 downto 0);
    Status_DO                 : out rvStatus_t;
    Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO                   : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO               : out std_logic;
    OutReady_SI               : in  std_logic);

end entity divsqrt_multifmt_slice;


architecture parallel_paths of divsqrt_multifmt_slice is

  -----------------------------------------------------------------------------
  -- Constant Definitions
  -----------------------------------------------------------------------------
  -- Latency given by highest latency in active set
  constant LATENCY : natural := largestActiveLatency(LATENCIES, FORMATS);

  -- Check how many bits are needed for the narrowest active float format
  constant MIN_WIDTH : natural := MINWIDTH(FORMATS);

  -- The number of parallel lanes the slice can hold - given by narrowest format
  constant NUMLANES : natural := SLICE_WIDTH/MIN_WIDTH;

  constant FMTBITS      : natural := clog2(fpFmt_t'pos(fpFmt_t'high));
  constant TAGINT_WIDTH : natural := TAG_WIDTH+1+FMTBITS;
  ---------------------------------------------------------------------------
  -- Type Definitions
  ---------------------------------------------------------------------------

  -- Vectors of results for the lanes
  type fmtResults_t is array (fpFmt_t) of std_logic_vector(SLICE_WIDTH-1 downto 0);
  type laneResults_t is array (0 to NUMLANES-1) of std_logic_vector(SLICE_WIDTH-1 downto 0);
  type laneTags_t is array (0 to NUMLANES-1) of std_logic_vector(TAGINT_WIDTH-1 downto 0);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Width of input and output format (for vectors). Wider formats than
  -- SLICE_WIDTH will be ignored in the unit
  signal SrcFmtWidth_S : natural;
  signal DstFmtSlv_S   : std_logic_vector(FMTBITS-1 downto 0);

  -- Internal tag keeps track of vectorial ops and destination width to combine
  -- results properly
  signal TagInt_D : std_logic_vector(TAGINT_WIDTH-1 downto 0);

  -- Internal Vectorial Selection
  signal VectorialOp_S : std_logic;

  -- Output data for each format
  signal FmtOpResults_S : fmtResults_t;

  signal LaneResults_D     : laneResults_t;
  signal ResultVectorial_S : std_logic;
  signal ResultFmt_S       : fpFmt_t;

  -- Valid, Status and Tag outputs from all lanes
  signal LaneStatus_D   : statusArray_t(0 to NUMLANES-1);
  signal LaneOutValid_S : std_logic_vector(0 to NUMLANES-1);
  signal LaneInReady_S  : std_logic_vector(0 to NUMLANES-1);
  signal LaneZext_S     : std_logic_vector(0 to NUMLANES-1);
  signal LaneTags_S     : laneTags_t;

  signal PackedResult_D : std_logic_vector(Z_DO'range);

begin  -- architecture parallel_paths

  -----------------------------------------------------------------------------
  -- Input Side signals
  -----------------------------------------------------------------------------

  -- Figure out the source and destination format width (depends on op)
  SrcFmtWidth_S <= WIDTH(FpFmt_SI, FORMATS);

  DstFmtSlv_S <= std_logic_vector(resize(unsigned(to_slv(FpFmt_SI)), DstFmtSlv_S'length));

  -- Mask vectorial enable if we don't have vector support
  VectorialOp_S <= VectorialOp_SI and to_sl(GENVECTORS);

  -- Upstream Ready is signalled if first lane can accept instructions
  InReady_SO <= LaneInReady_S(0);

  -- Add vectorial tag to the top of the input tag (at position TAG_WIDTH)
  -- Also add the format we're using so we know how to properly unpack the
  -- result
  TagInt_D <= DstFmtSlv_S & VectorialOp_S & Tag_DI;

  -----------------------------------------------------------------------------
  -- Generate multiformat slices
  -----------------------------------------------------------------------------

  g_sliceLanes : for i in 0 to NUMLANES-1 generate

    -- dimensions of lanes differ for formats and position, set active formats
    constant LANEFORMATS : activeFormats_t := getMultiLaneFormats(FORMATS, SLICE_WIDTH, i);
    constant LANE_WIDTH  : natural         := MAXWIDTH(LANEFORMATS);

    -- Lane's input data. Upper input bits of narrow formats are ingnored
    signal AShifted_D : std_logic_vector(A_DI'range);
    signal A_D        : std_logic_vector(LANE_WIDTH-1 downto 0);
    signal BShifted_D : std_logic_vector(B_DI'range);
    signal B_D        : std_logic_vector(LANE_WIDTH-1 downto 0);

    -- Input Operand NaN-boxed checks (only for scalars)
    signal ABox_S, BBox_S, CBox_S : fmtLogic_t;

    -- Enable signal for lanes
    signal InValid_S  : std_logic;
    signal OutValid_S : std_logic;
    signal OutReady_S : std_logic;

    -- Lane-local results
    signal OpResult_D, Result_D : std_logic_vector(LANE_WIDTH-1 downto 0);
    signal OpStatus_D           : rvStatus_t;

  begin

    -- Generate instances (widest always, others only for vectors)
    g_laneInst : if i = 0 or GENVECTORS generate

      -- If inputs are vectorial, we need to bring the element to the LSB side
      AShifted_D <= std_logic_vector(unsigned(A_DI) srl i*SrcFmtWidth_S);
      A_D        <= AShifted_D(LANE_WIDTH-1 downto 0);
      BShifted_D <= std_logic_vector(unsigned(B_DI) srl i*SrcFmtWidth_S);
      B_D        <= BShifted_D(LANE_WIDTH-1 downto 0);

      p_inNanBoxing : process (all) is
      begin  -- process p_inNanBoxing

        for fmt in fpFmt_t loop
          -- Boxing check is overriden for vectorial ops
          ABox_S(fmt) <= ABox_SI(fmt) or VectorialOp_S or to_sl(i /= 0);
          BBox_S(fmt) <= BBox_SI(fmt) or VectorialOp_S or to_sl(i /= 0);
          CBox_S(fmt) <= CBox_SI(fmt) or VectorialOp_S or to_sl(i /= 0);
        end loop;  -- fmt

      end process p_inNanBoxing;

      -- Generate input valid logic for this lane based on input valid:
      -- first lane always on, others only for vectorial ops
      InValid_S <= InValid_SI and (to_sl(i = 0) or VectorialOp_S);

      i_fp_divsqrt_multi : fp_divsqrt_multi
        generic map (
          FORMATS   => LANEFORMATS,
          LATENCY   => LATENCY,
          TAG_WIDTH => TAGINT_WIDTH)
        port map (
          Clk_CI       => Clk_CI,
          Reset_RBI    => Reset_RBI,
          A_DI         => A_D,
          ABox_SI      => ABox_S,
          B_DI         => B_D,
          BBox_SI      => BBox_S,
          RoundMode_SI => RoundMode_SI,
          Op_SI        => Op_SI,
          OpMod_SI     => OpMod_SI,
          FpFmt_SI     => FpFmt_SI,
          Tag_DI       => TagInt_D,
          PrecCtl_SI   => PrecCtl_SI,
          InValid_SI   => InValid_S,
          InReady_SO   => LaneInReady_S(i),
          Flush_SI     => Flush_SI,
          Z_DO         => OpResult_D,
          Status_DO    => OpStatus_D,
          Tag_DO       => LaneTags_S(i),
          Zext_SO      => LaneZext_S(i),
          OutValid_SO  => OutValid_S,
          OutReady_SI  => OutReady_S);

      -- Generate the ready input for this lane based on downstream ready:
      -- First lane follows global ready, other lanes only for vectorial ops
      OutReady_S <= OutReady_SI and (to_sl(i = 0) or ResultVectorial_S);

      -- Upper lanes are only used when there is a vectorial op
      LaneOutValid_S(i) <= OutValid_S and (to_sl(i = 0) or ResultVectorial_S);

      -- Zero-Extend the result when requested, else NaN-Box unused results
      Result_D <= OpResult_D when LaneOutValid_S(i) = '1' else
                  (others => '0') when LaneZext_S(0) = '1' else
                  (others => '1');

      -- Silence status when result not used
      LaneStatus_D(i) <= OpStatus_D when LaneOutValid_S(i) = '1' else
                         (others => '0');

    end generate g_laneInst;

    -- Otherwise generate all ones/zeroes for NaN-boxing / silencing
    g_laneBypass : if (i /= 0 and not GENVECTORS) generate

      Result_D <= (others => '0') when LaneZext_S(0) = '1' else
                  (others => '1');
      LaneStatus_D(i)   <= (others => '0');
      LaneOutValid_S(i) <= '0';
      LaneInReady_S(i)  <= '0';

    end generate g_laneBypass;

    -- Add lane result into global lanes result, shifted for vectors
    --   LaneResults_D(i)(LANE_WIDTH-1 downto 0) <= Result_D;

    g_fmtResults : for fmt in fpFmt_t generate
      g_activeFmts : if LANEFORMATS.Active(fmt) generate
        FmtOpResults_S(fmt)((i+1)*WIDTH(fmt, LANEFORMATS)-1 downto i*WIDTH(fmt, LANEFORMATS))
          <= Result_D(WIDTH(fmt, LANEFORMATS)-1 downto 0);
      end generate g_activeFmts;
    end generate g_fmtResults;

  end generate g_sliceLanes;

  -- Output of slice is vectorial if the output vectorial tag is set (lane 0)
  ResultVectorial_S <= LaneTags_S(0)(TAG_WIDTH);

  -- Restore the destination format width
  ResultFmt_S <= to_fpFmt(LaneTags_S(0)(FMTBITS+TAG_WIDTH downto TAG_WIDTH+1));

  -----------------------------------------------------------------------------
  -- Result selection
  -----------------------------------------------------------------------------

  Z_DO <= FmtOpResults_S(ResultFmt_S);

  -- Separate the sign-extension information from the tag again
  Tag_DO  <= LaneTags_S(0)(Tag_DO'range);
  Zext_SO <= LaneTags_S(0)(TAG_WIDTH);

  -- Combine slice status (logic ORing)
  Status_DO <= combined_status(LaneStatus_D);

  -- First lane dictates the flow of operations
  OutValid_SO <= LaneOutValid_S(0);


end architecture parallel_paths;
