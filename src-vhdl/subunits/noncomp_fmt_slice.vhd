-------------------------------------------------------------------------------
-- Title      : Floating-Point Format-Specific Slice
-- Project    :
-------------------------------------------------------------------------------
-- File       : noncomp_fmt_slice.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-24
-- Last update: 2018-04-18
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
use work.fpnew_comps_pkg.all;

--! @brief Floating-Point Format-Specific Slice
--! @details Parametric slice containing all floating-point operations that
--! operate on a singular format.
--! Supported operations from fpnew_pkg.fpOp:
--! - FMADD
--! - FMSUB
--! - ADD
--! - MUL
entity noncomp_fmt_slice is

  generic (
    EXP_BITS    : natural := 5;
    MAN_BITS    : natural := 10;
    LATENCY     : natural := 0;
    SLICE_WIDTH : natural := 16;
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
    ---------------------------------------------------------------------------
    OutValid_SO               : out std_logic;
    OutReady_SI               : in  std_logic);

end entity noncomp_fmt_slice;


architecture rtl of noncomp_fmt_slice is

  -----------------------------------------------------------------------------
  -- Constant Definitions
  -----------------------------------------------------------------------------
  -- The width of the FP format
  constant FMT_WIDTH : natural := EXP_BITS+MAN_BITS+1;

  -- The number of parallel lanes the slice can hold
  constant NUMLANES : natural := SLICE_WIDTH/FMT_WIDTH;

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

  -- Vectors of results for the lanes
  --type laneResults_t is array (NUMLANES-1 downto 0) of std_logic_vector(FMTWIDTH-1 downto 0);
  type laneTags_t is array (0 to NUMLANES-1) of std_logic_vector(TAG_WIDTH downto 0);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Internal Vectorial Selection
  signal VectorialOp_S : std_logic;

  -- Internal tag keeps track of vectorial ops to combine results properly
  signal TagInt_D : std_logic_vector(TAG_WIDTH downto 0);

  -- Signals holding slice results
  signal SliceResult_D     : std_logic_vector(NUMLANES*FMT_WIDTH-1 downto 0);
  signal ResultVectorial_S : std_logic;

  -- Valid, Status and Tag outputs from all lanes, also noncomp-specific info
  signal LaneStatus_D   : statusArray_t(0 to NUMLANES-1);
  signal LaneOutValid_S : std_logic_vector(0 to NUMLANES-1);
  signal LaneInReady_S  : std_logic_vector(0 to NUMLANES-1);
  signal LaneUnpack_S   : std_logic_vector(0 to NUMLANES-1);
  signal LaneZext_S     : std_logic_vector(0 to NUMLANES-1);
  signal LaneTags_S     : laneTags_t;

  signal ResultPreUnpack_D : std_logic_vector(Z_DO'range);

begin  -- architecture rtl

  -- Upstream Ready is signalled if first lane can accept instructions
  InReady_SO <= LaneInReady_S(0);

  -- Mask vectorial enable if we don't have vector support
  VectorialOp_S <= VectorialOp_SI and to_sl(GENVECTORS);

  -- Add vectorial tag to the top of the input tag (at position TAG_WIDTH)
  TagInt_D <= VectorialOp_S & Tag_DI;

  -----------------------------------------------------------------------------
  -- Generate duplicated lanes for vectorial ops (parallel)
  -----------------------------------------------------------------------------

  -- Generate Lanes
  g_fmtOpLane : for i in 0 to NUMLANES-1 generate

    -- Input Operand NaN-boxed checks (only for scalars)
    signal ABox_S, BBox_S, CBox_S : std_logic;

    -- Enable signal differs across lanes for scalar ops
    signal InValid_S  : std_logic;
    signal OutValid_S : std_logic;
    signal OutReady_S : std_logic;

    -- Lane-local results
    signal OpResult_D, LaneResult_D : std_logic_vector(FMT_WIDTH-1 downto 0);
    signal OpStatus_D               : rvStatus_t;

  begin

    -- Generate instances in lane only if needed (one lane at least)
    g_laneInst : if (i = 0 or GENVECTORS) generate

      -- Boxing check is overriden for vectorial ops
      ABox_S <= ABox_SI or VectorialOp_S or to_sl(i /= 0);
      BBox_S <= BBox_SI or VectorialOp_S or to_sl(i /= 0);
      CBox_S <= CBox_SI or VectorialOp_S or to_sl(i /= 0);

      -- Generate input valid logic for this lane based on input valid:
      -- first lane always on, others only for vectorial ops
      InValid_S <= InValid_SI and (to_sl(i = 0) or VectorialOp_S);

      i_fp_noncomp : fp_noncomp
        generic map (
          EXP_BITS  => EXP_BITS,
          MAN_BITS  => MAN_BITS,
          LATENCY   => LATENCY,
          TAG_WIDTH => TAG_WIDTH+1)
        port map (
          Clk_CI         => Clk_CI,
          Reset_RBI      => Reset_RBI,
          A_DI           => A_DI((i+1)*FMT_WIDTH-1 downto i*FMT_WIDTH),
          B_DI           => B_DI((i+1)*FMT_WIDTH-1 downto i*FMT_WIDTH),
          ABox_SI        => ABox_S,
          BBox_SI        => BBox_S,
          RoundMode_SI   => RoundMode_SI,
          Op_SI          => Op_SI,
          OpMod_SI       => OpMod_SI,
          VectorialOp_SI => VectorialOp_S,
          Tag_DI         => TagInt_D,
          InValid_SI     => InValid_S,
          InReady_SO     => LaneInReady_S(i),
          Flush_SI       => Flush_SI,
          Z_DO           => OpResult_D,
          Status_DO      => OpStatus_D,
          Tag_DO         => LaneTags_S(i),
          UnpackClass_SO => LaneUnpack_S(i),
          Zext_SO        => LaneZext_S(i),
          OutValid_SO    => OutValid_S,
          OutReady_SI    => OutReady_S);

      -- Generate the ready input for this lane based on downstream ready:
      -- First lane follows global ready, other lanes only for vectorial ops
      OutReady_S <= OutReady_SI and (to_sl(i = 0) or ResultVectorial_S);

      -- Upper lanes are only used when there is a vectorial op
      LaneOutValid_S(i) <= OutValid_S and (to_sl(i = 0) or ResultVectorial_S);

      -- Zero-Extend the result when requested, else NaN-Box unused results
      LaneResult_D <= OpResult_D when LaneOutValid_S(i) = '1' else
                      (others => '0') when LaneZext_S(0) = '1' else
                      (others => '1');

      -- Silence status when result not used
      LaneStatus_D(i) <= OpStatus_D when LaneOutValid_S(i) = '1' else
                         (others => '0');

    -- else generate is only valid in VHDL-2008
--      else generate
    end generate g_laneInst;
    -- Otherwise generate all ones/zeroes for NaN-boxing / silencing
    g_laneBypass : if (i /= 0 and not GENVECTORS) generate

      LaneResult_D <= (others => '0') when LaneZext_S(0) = '1' else
                      (others => '1');
      LaneStatus_D(i)   <= (others => '0');
      LaneOutValid_S(i) <= '0';
      LaneInReady_S(i)  <= '0';

    end generate g_laneBypass;

    -- Insert lane result into slice result
    SliceResult_D((i+1)*FMT_WIDTH-1 downto i*FMT_WIDTH) <= LaneResult_D;

  end generate g_fmtOpLane;

  -- Output of slice is vectorial if the output vectorial tag is set (lane 0)
  ResultVectorial_S <= LaneTags_S(0)(TAG_WIDTH);  -- VectorialOp

  -- Extend result to fit in slice result width (NaN-boxing or zero-extension)
  -- --> could happen if the slice width is not a multiple of the fp format
  ResultPreUnpack_D(SliceResult_D'range)                   <= SliceResult_D;
  ResultPreUnpack_D(Z_DO'high downto SliceResult_D'high+1) <= (others => '0') when LaneZext_S(0) = '1' else
                                                              (others => '1');
  -----------------------------------------------------------------------------
  -- Result Selection
  -----------------------------------------------------------------------------
  -- Unpack the scalar classification block if needed
  Z_DO <= std_logic_vector(resize(unsigned(to_slv_unpack(ResultPreUnpack_D(3 downto 0))), Z_DO'length)) when LaneUnpack_S(0) = '1' else
          ResultPreUnpack_D;

  -- Separate the sign-extension information from the tag again
  Tag_DO  <= LaneTags_S(0)(Tag_DO'range);
  Zext_SO <= LaneTags_S(0)(TAG_WIDTH);

  -- Combine slice status (logic ORing)
  Status_DO <= combined_status(LaneStatus_D);

  -- First lane dictates the flow of operations
  OutValid_SO <= LaneOutValid_S(0);

end architecture rtl;
