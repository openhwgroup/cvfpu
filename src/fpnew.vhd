-------------------------------------------------------------------------------
-- Title      : TransPrecision Floating-Point Unit
-- Project    :
-------------------------------------------------------------------------------
-- File       : fpnew.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-24
-- Last update: 2018-10-10
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

--! @brief TransPrecision Floating-Point Unit
entity fpnew is

  generic (
    FORMATS : activeFormats_t := (Active   => (FP32 to FP16ALT => true, others => false),
                                  Encoding => DEFAULTENCODING);

    INTFORMATS : activeIntFormats_t := (Active => (others => true),
                                        Length => INTFMTLENGTHS);

    UNITTYPES : opGroupFmtUnitTypes_t := (ADDMUL  => (others => PARALLEL),
                                          DIVSQRT => (others => MERGED),
                                          NONCOMP => (others => PARALLEL),
                                          CONV    => (others => PARALLEL));

    LATENCIES  : opGroupFmtNaturals_t := (others => (others => 0));
    GENVECTORS : boolean              := true;
    TAG_WIDTH  : natural              := 0;
    IN_NANBOX  : boolean              := true);

  port (
    Clk_CI           : in  std_logic;
    Reset_RBI        : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI, B_DI, C_DI : in  std_logic_vector(MAXWIDTH(FORMATS, INTFORMATS)-1 downto 0);
    RoundMode_SI     : in  rvRoundingMode_t;
    Op_SI            : in  fpOp_t;
    OpMod_SI         : in  std_logic;
    VectorialOp_SI   : in  std_logic;
    FpFmt_SI         : in  fpFmt_t;
    FpFmt2_SI        : in  fpFmt_t;
    IntFmt_SI        : in  intFmt_t;
    Tag_DI           : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    PrecCtl_SI       : in  std_logic_vector(6 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI       : in  std_logic;
    InReady_SO       : out std_logic;
    Flush_SI         : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO             : out std_logic_vector(MAXWIDTH(FORMATS, INTFORMATS)-1 downto 0);
    Status_DO        : out rvStatus_t;
    Tag_DO           : out std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    OutValid_SO      : out std_logic;
    OutReady_SI      : in  std_logic);

end entity fpnew;


architecture rtl of fpnew is

  -----------------------------------------------------------------------------
  -- Constant Declarations
  -----------------------------------------------------------------------------

  -- Width of the FPnew (maximum width of both fp and int formats)
  constant DATA_WIDTH : natural := MAXWIDTH(FORMATS, INTFORMATS);

  -- Width of the widest active FP format (for fp-fp or noncomp ops)
  constant FLEN : natural := MAXWIDTH(FORMATS);

  -- Width of the widest active integer format (for fp-int ops)
  constant ILEN : natural := MAXWIDTH(INTFORMATS);

  -- Number of Operation Groups
  constant NUM_OPGROUPS : natural := fpOpGroup_t'pos(fpOpGroup_t'high) + 1;


  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

  --! @brief Array of output values for each format
  --! @details Array of STD_LOGIC_VECTOR that hold a value for each \ref
  --! fpOpGroup_t "FPOPGROUP_T"
  type opGroupSlResults_t is array (fpOpGroup_t) of std_logic_vector(Z_DO'range);
  type opGroupSlTags_t is array (fpOpGroup_t) of std_logic_vector(Tag_DO'range);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Input side
  signal OpGrpInValid_S : opGroupLogic_t;
  signal OpGrpInReady_S : opGroupLogic_t;

  -- Input NaN-Boxing detection
  signal ABox_S, BBox_S, CBox_S : fmtLogic_t;

  -- OpGroup-specific output side (fp ops could be narrower than unit)
  signal AddMulResult_D, DivSqrtResult_D : std_logic_vector(FLEN-1 downto 0);
  signal NonCompResult_D                 : std_logic_vector(FLEN-1 downto 0);
  signal ConvResult_D                    : std_logic_vector(DATA_WIDTH-1 downto 0);

  -- Output side
  signal OpGrpOutResults_D  : opGroupSlResults_t;
  signal OpGrpOutStatuses_D : opGroupStatus_t;
  signal OpGrpOutTags_D     : opGroupSlTags_t;
  signal OpGrpOutZext_S     : opGroupLogic_t;
  signal OpGrpOutValid_S    : opGroupLogic_t;
  signal OpGrpOutReady_S    : opGroupLogic_t;

  -- Subunit output side as arrays for arbitration - VHDL-93 silliness
  signal ArbInResults_D  : slArray2d_t(0 to NUM_OPGROUPS-1, Z_DO'range);
  signal ArbInStatuses_D : statusArray_t(0 to NUM_OPGROUPS-1);
  signal ArbInTags_D     : slArray2d_t(0 to NUM_OPGROUPS-1, Tag_DO'range);
  signal ArbInValid_S    : std_logic_vector(0 to NUM_OPGROUPS-1);
  signal ArbInReady_S    : std_logic_vector(0 to NUM_OPGROUPS-1);

  -- Arbiter Valid output
  signal OutValid_S        : std_logic;
  signal OutputProcessed_S : std_logic;

  -- Counter for RR arbiter
  signal RoundRobin_SP, RoundRobin_SN : std_logic_vector(clog2(NUM_OPGROUPS)-1 downto 0);


begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- FPnew input readiness depends on whether the chosen subunit is ready
  -----------------------------------------------------------------------------
  InReady_SO <= InValid_SI and OpGrpInReady_S(getFpOpGroup(Op_SI));

  -----------------------------------------------------------------------------
  -- Input side signals for each operation group subunit and format
  -----------------------------------------------------------------------------

  p_inputSideSignals : process (all) is
  begin  -- process p_inputNanBoxing

    -- Subunit is activated if requested operation matches its opgroup
    for opgrp in fpOpGroup_t loop
      OpGrpInValid_S(opgrp) <= InValid_SI and to_sl(getFpOpGroup(Op_SI) = opgrp);
    end loop;  -- opgrp in fpOpGroup_t generate

    -- Input NaN-boxing is detected for each FP format
    for fmt in fpFmt_t loop
      if IN_NANBOX and WIDTH(fmt, FORMATS) < FLEN then
        ABox_S(fmt) <= to_sl(unsigned(not A_DI(A_DI'high downto WIDTH(fmt, FORMATS))) = 0);
        BBox_S(fmt) <= to_sl(unsigned(not B_DI(B_DI'high downto WIDTH(fmt, FORMATS))) = 0);
        CBox_S(fmt) <= to_sl(unsigned(not C_DI(C_DI'high downto WIDTH(fmt, FORMATS))) = 0);
      else
        ABox_S(fmt) <= '1';
        BBox_S(fmt) <= '1';
        CBox_S(fmt) <= '1';
      end if;
    end loop;  -- fmt

  end process p_inputSideSignals;

  -----------------------------------------------------------------------------
  -- Operation Group Subunits
  -----------------------------------------------------------------------------

  -- ADDMUL
  i_addmul_block : addmul_block
    generic map (
      FORMATS    => FORMATS,
      UNITTYPES  => UNITTYPES(ADDMUL),
      LATENCIES  => LATENCIES(ADDMUL),
      GENVECTORS => GENVECTORS,
      TAG_WIDTH  => TAG_WIDTH)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      A_DI           => A_DI(FLEN-1 downto 0),
      B_DI           => B_DI(FLEN-1 downto 0),
      C_DI           => C_DI(FLEN-1 downto 0),
      ABox_SI        => ABox_S,
      BBox_SI        => BBox_S,
      CBox_SI        => CBox_S,
      RoundMode_SI   => RoundMode_SI,
      Op_SI          => Op_SI,
      OpMod_SI       => OpMod_SI,
      FpFmt_SI       => FpFmt_SI,
      VectorialOp_SI => VectorialOp_SI,
      Tag_DI         => Tag_DI,
      InValid_SI     => OpGrpInValid_S(ADDMUL),
      InReady_SO     => OpGrpInReady_S(ADDMUL),
      Flush_SI       => Flush_SI,
      Z_DO           => AddMulResult_D,
      Status_DO      => OpGrpOutStatuses_D(ADDMUL),
      Tag_DO         => OpGrpOutTags_D(ADDMUL),
      Zext_SO        => OpGrpOutZext_S(ADDMUL),
      OutValid_SO    => OpGrpOutValid_S(ADDMUL),
      OutReady_SI    => OpGrpOutReady_S(ADDMUL));

  -- Perform the proper extension (in case integers are wider than floats)
  OpGrpOutResults_D(ADDMUL)(AddMulResult_D'range) <= AddMulResult_D;
  OpGrpOutResults_D(ADDMUL)(Z_DO'high downto AddMulResult_D'high+1)
    <= (others => '0') when OpGrpOutZext_S(ADDMUL) = '1' else
    (others    => '1');


  -- DIVSQRT
  i_divsqrt_block : divsqrt_block
    generic map (
      FORMATS    => FORMATS,
      UNITTYPES  => UNITTYPES(DIVSQRT),
      LATENCIES  => LATENCIES(DIVSQRT),
      GENVECTORS => GENVECTORS,
      TAG_WIDTH  => TAG_WIDTH)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      A_DI           => A_DI(FLEN-1 downto 0),
      B_DI           => B_DI(FLEN-1 downto 0),
      C_DI           => (others => '-'),
      ABox_SI        => ABox_S,
      BBox_SI        => BBox_S,
      CBox_SI        => CBox_S,
      RoundMode_SI   => RoundMode_SI,
      Op_SI          => Op_SI,
      OpMod_SI       => OpMod_SI,
      FpFmt_SI       => FpFmt_SI,
      VectorialOp_SI => VectorialOp_SI,
      Tag_DI         => Tag_DI,
      PrecCtl_SI     => PrecCtl_SI,
      InValid_SI     => OpGrpInValid_S(DIVSQRT),
      InReady_SO     => OpGrpInReady_S(DIVSQRT),
      Flush_SI       => Flush_SI,
      Z_DO           => DivSqrtResult_D,
      Status_DO      => OpGrpOutStatuses_D(DIVSQRT),
      Tag_DO         => OpGrpOutTags_D(DIVSQRT),
      Zext_SO        => OpGrpOutZext_S(DIVSQRT),
      OutValid_SO    => OpGrpOutValid_S(DIVSQRT),
      OutReady_SI    => OpGrpOutReady_S(DIVSQRT));

  -- Perform the proper extension (in case integers are wider than floats)
  OpGrpOutResults_D(DIVSQRT)(DivSqrtResult_D'range) <= DivSqrtResult_D;
  OpGrpOutResults_D(DIVSQRT)(Z_DO'high downto DivSqrtResult_D'high+1)
    <= (others => '0') when OpGrpOutZext_S(DIVSQRT) = '1' else
    (others    => '1');


  -- NONCOMP
  i_noncomp_block : noncomp_block
    generic map (
      FORMATS    => FORMATS,
      UNITTYPES  => UNITTYPES(NONCOMP),
      LATENCIES  => LATENCIES(NONCOMP),
      GENVECTORS => GENVECTORS,
      TAG_WIDTH  => TAG_WIDTH)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      A_DI           => A_DI(FLEN-1 downto 0),
      B_DI           => B_DI(FLEN-1 downto 0),
      C_DI           => (others => '-'),
      ABox_SI        => ABox_S,
      BBox_SI        => BBox_S,
      CBox_SI        => CBox_S,
      RoundMode_SI   => RoundMode_SI,
      Op_SI          => Op_SI,
      OpMod_SI       => OpMod_SI,
      FpFmt_SI       => FpFmt_SI,
      VectorialOp_SI => VectorialOp_SI,
      Tag_DI         => Tag_DI,
      InValid_SI     => OpGrpInValid_S(NONCOMP),
      InReady_SO     => OpGrpInReady_S(NONCOMP),
      Flush_SI       => Flush_SI,
      Z_DO           => NonCompResult_D,
      Status_DO      => OpGrpOutStatuses_D(NONCOMP),
      Tag_DO         => OpGrpOutTags_D(NONCOMP),
      Zext_SO        => OpGrpOutZext_S(NONCOMP),
      OutValid_SO    => OpGrpOutValid_S(NONCOMP),
      OutReady_SI    => OpGrpOutReady_S(NONCOMP));

  -- Perform the proper extension (in case integers are wider than floats)
  OpGrpOutResults_D(NONCOMP)(NonCompResult_D'range) <= NonCompResult_D;
  OpGrpOutResults_D(NONCOMP)(Z_DO'high downto NonCompResult_D'high+1)
    <= (others => '0') when OpGrpOutZext_S(NONCOMP) = '1' else
    (others    => '1');


  -- CONV
  i_conv_block : conv_block
    generic map (
      FORMATS    => FORMATS,
      INTFORMATS => INTFORMATS,
      UNITTYPES  => UNITTYPES(CONV),
      LATENCIES  => LATENCIES(CONV),
      GENVECTORS => GENVECTORS,
      TAG_WIDTH  => TAG_WIDTH)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      A_DI           => A_DI,
      B_DI           => B_DI,
      C_DI           => C_DI,
      ABox_SI        => ABox_S,
      BBox_SI        => BBox_S,
      CBox_SI        => CBox_S,
      RoundMode_SI   => RoundMode_SI,
      Op_SI          => Op_SI,
      OpMod_SI       => OpMod_SI,
      FpFmt_SI       => FpFmt_SI,
      FpFmt2_SI      => FpFmt2_SI,
      IntFmt_SI      => IntFmt_SI,
      VectorialOp_SI => VectorialOp_SI,
      Tag_DI         => Tag_DI,
      InValid_SI     => OpGrpInValid_S(CONV),
      InReady_SO     => OpGrpInReady_S(CONV),
      Flush_SI       => Flush_SI,
      Z_DO           => OpGrpOutResults_D(CONV),
      Status_DO      => OpGrpOutStatuses_D(CONV),
      Tag_DO         => OpGrpOutTags_D(CONV),
      Zext_SO        => OpGrpOutZext_S(CONV),
      OutValid_SO    => OpGrpOutValid_S(CONV),
      OutReady_SI    => OpGrpOutReady_S(CONV));


  -----------------------------------------------------------------------------
  -- Output Arbitration
  -----------------------------------------------------------------------------

  -- Convert arbitration inputs (index by naturals instead of custom types)
  p_arbInputSide : process (all) is

    variable OpGrpResult_D : std_logic_vector(Z_DO'range);
    variable OpGrpTag_D    : std_logic_vector(Tag_DO'range);

  begin  -- process p_arbInputSide

    -- change array types to proper 2d arrays - VHDL-93 fluff
    for i in 0 to NUM_OPGROUPS-1 loop

      OpGrpResult_D := OpGrpOutResults_D(fpOpGroup_t'val(i));
      OpGrpTag_D    := OpGrpOutTags_D(fpOpGroup_t'val(i));

      set_row_var(ArbInResults_D, i, OpGrpResult_D);
      ArbInStatuses_D(i) <= OpGrpOutStatuses_D(fpOpGroup_t'val(i));
      set_row_var(ArbInTags_D, i, OpGrpTag_D);

    end loop;  -- OpGrp

    ArbInValid_S    <= to_slv(OpGrpOutValid_S);
    OpGrpOutReady_S <= to_opGroupLogic(ArbInReady_S);

  end process p_arbInputSide;

  -- The arbiter
  i_fp_arbiter : fp_arbiter
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      NUM_INPUTS => NUM_OPGROUPS,
      TAG_WIDTH  => TAG_WIDTH)
    port map (
      InResults_DI  => ArbInResults_D,
      InStatuses_DI => ArbInStatuses_D,
      InTags_DI     => ArbInTags_D,
      InValid_SI    => ArbInValid_S,
      InReady_SO    => ArbInReady_S,
      Priorities_SI => RoundRobin_SP,
      OutResult_DO  => Z_DO,
      OutStatus_DO  => Status_DO,
      OutTag_DO     => Tag_DO,
      OutValid_SO   => OutValid_S,
      OutReady_SI   => OutReady_SI,
      OutIdx_SO     => open);

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
