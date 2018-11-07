-------------------------------------------------------------------------------
-- Title      : Top Entity of TransPrecision Floating-Point Unit
-- Project    :
-------------------------------------------------------------------------------
-- File       : fpnew_top.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-24
-- Last update: 2018-10-10
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Converts to package-specific formats to standard logic types
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

entity fpnew_top is

  generic (
    WIDTH     : natural := 64;          -- Narrower width will mask out fmts
    TAG_WIDTH : natural := 0;           -- Tag is sent along with operation

    RV64    : boolean := true;          -- Enables 64-bit integer formats
    RVF     : boolean := true;          -- Enables FP32 format
    RVD     : boolean := true;          -- Enables FP64 format
    Xf16    : boolean := true;          -- Enables FP16 format
    Xf16alt : boolean := true;          -- Enables FP16alt format
    Xf8     : boolean := true;          -- Enables FP8 format
    Xfvec   : boolean := true;          -- Generates vector for enabled formats

    -- Unit types for operation groups
    TYPE_ADDMUL  : natural := unitType_t'pos(PARALLEL);
    TYPE_DIVSQRT : natural := unitType_t'pos(MERGED);
    TYPE_NONCOMP : natural := unitType_t'pos(PARALLEL);
    TYPE_CONV    : natural := unitType_t'pos(MERGED);

    LATENCY_COMP_F       : natural := 0;  -- Latency of FP32 comp. ops
    LATENCY_COMP_D       : natural := 0;  -- Latency of FP64 comp. ops
    LATENCY_COMP_Xf16    : natural := 0;  -- Latency of FP16 comp. ops
    LATENCY_COMP_Xf16alt : natural := 0;  -- Latency of FP16alt comp. ops
    LATENCY_COMP_Xf8     : natural := 0;  -- Latency of FP8 comp. ops
    LATENCY_DIVSQRT      : natural := 0;  -- Latency of div/sqrt. postprocessing
    LATENCY_NONCOMP      : natural := 0;  -- Latency of non-comp. ops
    LATENCY_CONV         : natural := 0;  -- Latency of conversion ops

    ENFORCE_INPUT_NANBOX : boolean := true);  -- Enforce input NaN-boxing

  port (
    Clk_CI           : in  std_logic;
    Reset_RBI        : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI, B_DI, C_DI : in  std_logic_vector(WIDTH-1 downto 0);
    RoundMode_SI     : in  std_logic_vector(2 downto 0);
    Op_SI            : in  std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OpMod_SI         : in  std_logic;
    VectorialOp_SI   : in  std_logic;
    FpFmt_SI         : in  std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    FpFmt2_SI        : in  std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    IntFmt_SI        : in  std_logic_vector(clog2(intFmt_t'pos(intFmt_t'high))-1 downto 0);
    Tag_DI           : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    PrecCtl_SI       : in  std_logic_vector(6 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI       : in  std_logic;
    InReady_SO       : out std_logic;
    Flush_SI         : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO             : out std_logic_vector(WIDTH-1 downto 0);
    Status_DO        : out std_logic_vector(4 downto 0);
    Tag_DO           : out std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    OutValid_SO      : out std_logic;
    OutReady_SI      : in  std_logic);

end entity fpnew_top;


architecture rtl of fpnew_top is

  -----------------------------------------------------------------------------
  -- Static Translation of Generic Inputs
  -----------------------------------------------------------------------------

  constant FORMATS : activeFormats_t := (Active             => (FP32 => RVF,
                                                    FP64    => RVD,
                                                    FP16    => Xf16,
                                                    FP8     => Xf8,
                                                    FP16ALT => Xf16alt,
                                                    others  => false),
                                         Encoding => DEFAULTENCODING);


  constant INTFORMATS : activeIntFormats_t := (Active            => (INT32 => true,
                                                          INT64      => RV64,
                                                          others => Xfvec),
                                               Length => INTFMTLENGTHS);

  constant LATENCIES : opGroupFmtNaturals_t := (ADDMUL             => (FP32 => LATENCY_COMP_F,
                                                           FP64    => LATENCY_COMP_D,
                                                           FP16    => LATENCY_COMP_Xf16,
                                                           FP8     => LATENCY_COMP_Xf8,
                                                           FP16ALT => LATENCY_COMP_Xf16alt,
                                                           others  => 0),
                                                DIVSQRT => (others => LATENCY_DIVSQRT),
                                                NONCOMP => (others => LATENCY_NONCOMP),
                                                CONV    => (others => LATENCY_CONV),
                                                others  => (others => 0));

  constant UNITTYPES : opGroupFmtUnitTypes_t := (ADDMUL  => (others => unitType_t'val(TYPE_ADDMUL)),
                                                 DIVSQRT => (others => unitType_t'val(TYPE_DIVSQRT)),
                                                 NONCOMP => (others => unitType_t'val(TYPE_NONCOMP)),
                                                 CONV    => (others => unitType_t'val(TYPE_CONV)));

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  signal Status_D : rvStatus_t;

begin  -- architecture rtl

  i_fpnew : fpnew
    generic map (
      FORMATS    => FORMATS,
      INTFORMATS => INTFORMATS,
      UNITTYPES  => UNITTYPES,
      LATENCIES  => LATENCIES,
      GENVECTORS => Xfvec,
      TAG_WIDTH  => TAG_WIDTH,
      IN_NANBOX  => ENFORCE_INPUT_NANBOX)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      A_DI           => A_DI,
      B_DI           => B_DI,
      C_DI           => C_DI,
      RoundMode_SI   => to_rvRoundMode(RoundMode_SI),
      Op_SI          => to_fpOp(Op_SI),
      OpMod_SI       => OpMod_SI,
      VectorialOp_SI => VectorialOp_SI,
      FpFmt_SI       => to_fpFmt(FpFmt_SI),
      FpFmt2_SI      => to_fpFmt(FpFmt2_SI),
      IntFmt_SI      => to_intFmt(IntFmt_SI),
      Tag_DI         => Tag_DI,
      PrecCtl_SI     => PrecCtl_SI,
      InValid_SI     => InValid_SI,
      InReady_SO     => InReady_SO,
      Flush_SI       => Flush_SI,
      Z_DO           => Z_DO,
      Status_DO      => Status_D,
      Tag_DO         => Tag_DO,
      OutValid_SO    => OutValid_SO,
      OutReady_SI    => OutReady_SI);

  Status_DO <= to_slv(Status_D);


end architecture rtl;


--configuration cfg_fpnew_RV64FDXf16_Xf16alt_Xf8 of fpnew_top is

--  for rtl
--    for all : fpnew
--      use entity work.fpnew
--        generic map (
--          FORMATS => (Active   => (FP32 to FP16ALT  => true,
--                                   others  => false),
--                      Encoding => DEFAULTENCODING),
--          INTFORMATS => (Active => (W      => true,
--                                    D      => true,
--                                    others => false),
--                         Length => INTFMTLENGTHS));
--    end for;
--  end for;

--end configuration cfg_fpnew_RV64FDXf16_Xf16alt_Xf8;

--configuration cfg_fpnew_RV32FDXf16_Xf16alt_Xf8 of fpnew_top is

--  for rtl
--    for all : fpnew
--      use entity work.fpnew
--        generic map (
--          FORMATS => (Active   => (FP32 to FP16ALT  => true,
--                                   others  => false),
--                      Encoding => DEFAULTENCODING),
--          INTFORMATS => (Active => (W      => true,
--                                    D      => false,
--                                    others => false),
--                         Length => INTFMTLENGTHS));
--    end for;
--  end for;

--end configuration cfg_fpnew_RV32FDXf16_Xf16alt_Xf8;

--configuration cfg_fpnew_RV32FXf16_Xf16alt_Xf8 of fpnew_top is

--  for rtl
--    for all : fpnew
--      use entity work.fpnew
--        generic map (
--          FORMATS => (Active   => (FP32             => true,
--                                   FP64             => false,
--                                   FP16 to FP16ALT  => true,
--                                   others           => false),
--                      Encoding => DEFAULTENCODING),
--          INTFORMATS => (Active => (W      => true,
--                                    D      => false,
--                                    others => false),
--                         Length => INTFMTLENGTHS));
--    end for;
--  end for;

--end configuration cfg_fpnew_RV32FXf16_Xf16alt_Xf8;
