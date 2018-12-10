-------------------------------------------------------------------------------
-- Title      : FP Unit Components Package
-- Project    :
-------------------------------------------------------------------------------
-- File       : fpnew_comps_pkg.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-20
-- Last update: 2018-10-10
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Component package for use with the 'FPnew' design and its
--              subcomponents.
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


--! @brief Component Package for TransPrecision Floating-Point Units
--!
--! Provides component declaration used in the transprecision floating-point
--! unit effort.
package fpnew_comps_pkg is

  -----------------------------------------------------------------------------
  -- FPnew
  -----------------------------------------------------------------------------

  component fpnew is
    generic (
      FORMATS    : activeFormats_t;
      INTFORMATS : activeIntFormats_t;
      UNITTYPES  : opGroupFmtUnitTypes_t;
      LATENCIES  : opGroupFmtNaturals_t;
      GENVECTORS : boolean;
      TAG_WIDTH  : natural;
      IN_NANBOX  : boolean);
    port (
      Clk_CI           : in  std_logic;
      Reset_RBI        : in  std_logic;
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
      InValid_SI       : in  std_logic;
      InReady_SO       : out std_logic;
      Flush_SI         : in  std_logic;
      Z_DO             : out std_logic_vector(MAXWIDTH(FORMATS, INTFORMATS)-1 downto 0);
      Status_DO        : out rvStatus_t;
      Tag_DO           : out std_logic_vector(TAG_WIDTH-1 downto 0);
      OutValid_SO      : out std_logic;
      OutReady_SI      : in  std_logic);
  end component fpnew;

  -----------------------------------------------------------------------------
  -- Unit Sub-Blocks
  -----------------------------------------------------------------------------

  component addmul_block is
    generic (
      FORMATS    : activeFormats_t;
      UNITTYPES  : fmtUnitTypes_t;
      LATENCIES  : fmtNaturals_t;
      GENVECTORS : boolean;
      TAG_WIDTH  : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      ABox_SI, BBox_SI, CBox_SI : in  fmtLogic_t;
      RoundMode_SI              : in  rvRoundingMode_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      VectorialOp_SI            : in  std_logic;
      FpFmt_SI                  : in  fpFmt_t;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO                   : out std_logic;
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component addmul_block;

  component divsqrt_block is
    generic (
      FORMATS    : activeFormats_t;
      UNITTYPES  : fmtUnitTypes_t;
      LATENCIES  : fmtNaturals_t;
      GENVECTORS : boolean;
      TAG_WIDTH  : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      ABox_SI, BBox_SI, CBox_SI : in  fmtLogic_t;
      RoundMode_SI              : in  rvRoundingMode_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      FpFmt_SI                  : in  fpFmt_t;
      VectorialOp_SI            : in  std_logic;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      PrecCtl_SI                : in  std_logic_vector(6 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO                   : out std_logic;
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component divsqrt_block;

  component noncomp_block is
    generic (
      FORMATS    : activeFormats_t;
      UNITTYPES  : fmtUnitTypes_t;
      LATENCIES  : fmtNaturals_t;
      GENVECTORS : boolean;
      TAG_WIDTH  : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      ABox_SI, BBox_SI, CBox_SI : in  fmtLogic_t;
      RoundMode_SI              : in  rvRoundingMode_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      VectorialOp_SI            : in  std_logic;
      FpFmt_SI                  : in  fpFmt_t;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO                   : out std_logic;
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component noncomp_block;

  component conv_block is
    generic (
      FORMATS    : activeFormats_t;
      INTFORMATS : activeIntFormats_t;
      UNITTYPES  : fmtUnitTypes_t;
      LATENCIES  : fmtNaturals_t;
      GENVECTORS : boolean;
      TAG_WIDTH  : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(MAXWIDTH(FORMATS, INTFORMATS)-1 downto 0);
      ABox_SI, BBox_SI, CBox_SI : in  fmtLogic_t;
      RoundMode_SI              : in  rvRoundingMode_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      FpFmt_SI                  : in  fpFmt_t;
      FpFmt2_SI                 : in  fpFmt_t;
      IntFmt_SI                 : in  intFmt_t;
      VectorialOp_SI            : in  std_logic;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(MAXWIDTH(FORMATS, INTFORMATS)-1 downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO                   : out std_logic;
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component conv_block;

  component addmul_fmt_slice is
    generic (
      EXP_BITS    : natural;
      MAN_BITS    : natural;
      LATENCY     : natural;
      SLICE_WIDTH : natural;
      GENVECTORS  : boolean;
      TAG_WIDTH   : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(SLICE_WIDTH-1 downto 0);
      ABox_SI, BBox_SI, CBox_SI : in  std_logic;
      RoundMode_SI              : in  rvRoundingMode_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      VectorialOp_SI            : in  std_logic;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(SLICE_WIDTH-1 downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component addmul_fmt_slice;

  component divsqrt_multifmt_slice is
    generic (
      FORMATS     : activeFormats_t;
      LATENCIES   : fmtNaturals_t;
      SLICE_WIDTH : natural;
      GENVECTORS  : boolean;
      TAG_WIDTH   : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(SLICE_WIDTH-1 downto 0);
      RoundMode_SI              : in  rvRoundingMode_t;
      ABox_SI, BBox_SI, CBox_SI : in  fmtLogic_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      FpFmt_SI                  : in  fpFmt_t;
      VectorialOp_SI            : in  std_logic;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      PrecCtl_SI                : in  std_logic_vector(6 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(SLICE_WIDTH-1 downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO                   : out std_logic;
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component divsqrt_multifmt_slice;

  component noncomp_fmt_slice is
    generic (
      EXP_BITS    : natural;
      MAN_BITS    : natural;
      LATENCY     : natural;
      SLICE_WIDTH : natural;
      GENVECTORS  : boolean;
      TAG_WIDTH   : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(SLICE_WIDTH-1 downto 0);
      ABox_SI, BBox_SI, CBox_SI : in  std_logic;
      RoundMode_SI              : in  rvRoundingMode_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      VectorialOp_SI            : in  std_logic;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(SLICE_WIDTH-1 downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO                   : out std_logic;
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component noncomp_fmt_slice;

  component conv_fmt_slice is
    generic (
      FORMATS     : activeFormats_t;
      CPKFORMATS  : fmtBooleans_t;
      SRCFMT      : fpFmt_t;
      LATENCY     : natural;
      SLICE_WIDTH : natural;
      GENVECTORS  : boolean;
      TAG_WIDTH   : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(SLICE_WIDTH-1 downto 0);
      ABox_SI, BBox_SI, CBox_SI : in  std_logic;
      RoundMode_SI              : in  rvRoundingMode_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      FpFmt_SI                  : in  fpFmt_t;
      VectorialOp_SI            : in  std_logic;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(SLICE_WIDTH-1 downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO                   : out std_logic;
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component conv_fmt_slice;

  component conv_ifmt_slice is
    generic (
      INTFORMATS  : activeIntFormats_t;
      FPENCODING  : fpFmtEncoding_t;
      LATENCY     : natural;
      SLICE_WIDTH : natural;
      GENVECTORS  : boolean;
      TAG_WIDTH   : natural);
    port (
      Clk_CI         : in  std_logic;
      Reset_RBI      : in  std_logic;
      A_DI           : in  std_logic_vector(SLICE_WIDTH-1 downto 0);
      ABox_SI        : in  std_logic;
      RoundMode_SI   : in  rvRoundingMode_t;
      Op_SI          : in  fpOp_t;
      OpMod_SI       : in  std_logic;
      IntFmt_SI      : in  intFmt_t;
      VectorialOp_SI : in  std_logic;
      Tag_DI         : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI     : in  std_logic;
      InReady_SO     : out std_logic;
      Flush_SI       : in  std_logic;
      Z_DO           : out std_logic_vector(SLICE_WIDTH-1 downto 0);
      Status_DO      : out rvStatus_t;
      Tag_DO         : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO        : out std_logic;
      OutValid_SO    : out std_logic;
      OutReady_SI    : in  std_logic);
  end component conv_ifmt_slice;

  component conv_multifmt_slice is
    generic (
      FORMATS     : activeFormats_t;
      INTFORMATS  : activeIntFormats_t;
      CPKFORMATS : fmtBooleans_t;
      LATENCIES   : fmtNaturals_t;
      SLICE_WIDTH : natural;
      GENVECTORS  : boolean;
      TAG_WIDTH   : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(SLICE_WIDTH-1 downto 0);
      ABox_SI, BBox_SI, CBox_SI : in  fmtLogic_t;
      RoundMode_SI              : in  rvRoundingMode_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      FpFmt_SI                  : in  fpFmt_t;
      FpFmt2_SI                 : in  fpFmt_t;
      IntFmt_SI                 : in  intFmt_t;
      VectorialOp_SI            : in  std_logic;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(SLICE_WIDTH-1 downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO                   : out std_logic;
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component conv_multifmt_slice;

  -----------------------------------------------------------------------------
  -- Regular Floating-Point Operations
  -----------------------------------------------------------------------------

  -- Fused Multiply-Add Unit
  --! \copydoc work.fp_fma
  component fp_fma is
    generic (
      EXP_BITS  : natural;
      MAN_BITS  : natural;
      LATENCY   : natural;
      TAG_WIDTH : natural);
    port (
      Clk_CI                    : in  std_logic;
      Reset_RBI                 : in  std_logic;
      A_DI, B_DI, C_DI          : in  std_logic_vector(EXP_BITS+MAN_BITS downto 0);
      ABox_SI, BBox_SI, CBox_SI : in  std_logic;
      RoundMode_SI              : in  rvRoundingMode_t;
      Op_SI                     : in  fpOp_t;
      OpMod_SI                  : in  std_logic;
      Tag_DI                    : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI                : in  std_logic;
      InReady_SO                : out std_logic;
      Flush_SI                  : in  std_logic;
      Z_DO                      : out std_logic_vector(EXP_BITS+MAN_BITS downto 0);
      Status_DO                 : out rvStatus_t;
      Tag_DO                    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      OutValid_SO               : out std_logic;
      OutReady_SI               : in  std_logic);
  end component fp_fma;

  component fp_divsqrt_multi is
    generic (
      FORMATS   : activeFormats_t;
      LATENCY   : natural;
      TAG_WIDTH : natural);
    port (
      Clk_CI           : in  std_logic;
      Reset_RBI        : in  std_logic;
      A_DI, B_DI       : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      ABox_SI, BBox_SI : in  fmtLogic_t;
      RoundMode_SI     : in  rvRoundingMode_t;
      Op_SI            : in  fpOp_t;
      OpMod_SI         : in  std_logic;
      FpFmt_SI         : in  fpFmt_t;
      Tag_DI           : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      PrecCtl_SI       : in  std_logic_vector(6 downto 0);
      InValid_SI       : in  std_logic;
      InReady_SO       : out std_logic;
      Flush_SI         : in  std_logic;
      Z_DO             : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      Status_DO        : out rvStatus_t;
      Tag_DO           : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO          : out std_logic;
      OutValid_SO      : out std_logic;
      OutReady_SI      : in  std_logic);
  end component fp_divsqrt_multi;

  -- Non-Computational FP Operations (Comparisons, Classifications etc.)
  --! \copydoc work.fp_noncomp
  component fp_noncomp is
    generic (
      EXP_BITS  : natural;
      MAN_BITS  : natural;
      LATENCY   : natural;
      TAG_WIDTH : natural);
    port (
      Clk_CI           : in  std_logic;
      Reset_RBI        : in  std_logic;
      A_DI, B_DI       : in  std_logic_vector(EXP_BITS+MAN_BITS downto 0);
      ABox_SI, BBox_SI : in  std_logic;
      RoundMode_SI     : in  rvRoundingMode_t;
      Op_SI            : in  fpOp_t;
      OpMod_SI         : in  std_logic;
      VectorialOp_SI   : in  std_logic;
      Tag_DI           : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI       : in  std_logic;
      InReady_SO       : out std_logic;
      Flush_SI         : in  std_logic;
      Z_DO             : out std_logic_vector(EXP_BITS+MAN_BITS downto 0);
      Status_DO        : out rvStatus_t;
      Tag_DO           : out std_logic_vector(TAG_WIDTH-1 downto 0);
      UnpackClass_SO   : out std_logic;
      Zext_SO          : out std_logic;
      OutValid_SO      : out std_logic;
      OutReady_SI      : in  std_logic);
  end component fp_noncomp;

  -- Conversions between formats
  --! \copydoc work.fp_conv_multi
  component fp_conv_multi is
    generic (
      FORMATS    : activeFormats_t;
      INTFORMATS : activeIntFormats_t;
      LATENCY    : natural;
      TAG_WIDTH  : natural);
    port (
      Clk_CI       : in  std_logic;
      Reset_RBI    : in  std_logic;
      A_DI         : in  std_logic_vector(MAXWIDTH(FORMATS, INTFORMATS)-1 downto 0);
      ABox_SI      : in  fmtLogic_t;
      RoundMode_SI : in  rvRoundingMode_t;
      Op_SI        : in  fpOp_t;
      OpMod_SI     : in  std_logic;
      FpFmt_SI     : in  fpFmt_t;
      FpFmt2_SI    : in  fpFmt_t;
      IntFmt_SI    : in  intFmt_t;
      Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI   : in  std_logic;
      InReady_SO   : out std_logic;
      Flush_SI     : in  std_logic;
      Z_DO         : out std_logic_vector(MAXWIDTH(FORMATS, INTFORMATS)-1 downto 0);
      Status_DO    : out rvStatus_t;
      Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO      : out std_logic;
      OutValid_SO  : out std_logic;
      OutReady_SI  : in  std_logic);
  end component fp_conv_multi;

  -- Float to Integer casts
  --! \copydoc work.fp_f2icasts
  component fp_f2icasts is
    generic (
      FORMATS    : activeFormats_t;
      INTFORMATS : activeIntFormats_t;
      LATENCY    : natural;
      TAG_WIDTH  : natural);
    port (
      Clk_CI       : in  std_logic;
      Reset_RBI    : in  std_logic;
      A_DI         : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      ABox_SI      : in  fmtLogic_t;
      RoundMode_SI : in  rvRoundingMode_t;
      OpMod_SI     : in  std_logic;
      SrcFmt_SI    : in  fpFmt_t;
      DstFmt_SI    : in  intFmt_t;
      Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI   : in  std_logic;
      InReady_SO   : out std_logic;
      Flush_SI     : in  std_logic;
      Z_DO         : out std_logic_vector(MAXWIDTH(INTFORMATS)-1 downto 0);
      Status_DO    : out rvStatus_t;
      Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO      : out std_logic;
      OutValid_SO  : out std_logic;
      OutReady_SI  : in  std_logic);
  end component fp_f2icasts;

  component fp_f2icasts_fmt is
    generic (
      SRCENCODING : fpFmtEncoding_t;
      INTFORMATS  : activeIntFormats_t;
      LATENCY     : natural;
      TAG_WIDTH   : natural);
    port (
      Clk_CI       : in  std_logic;
      Reset_RBI    : in  std_logic;
      A_DI         : in  std_logic_vector(WIDTH(SRCENCODING)-1 downto 0);
      ABox_SI      : in  std_logic;
      RoundMode_SI : in  rvRoundingMode_t;
      OpMod_SI     : in  std_logic;
      DstFmt_SI    : in  intFmt_t;
      Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI   : in  std_logic;
      InReady_SO   : out std_logic;
      Flush_SI     : in  std_logic;
      Z_DO         : out std_logic_vector(MAXWIDTH(INTFORMATS)-1 downto 0);
      Status_DO    : out rvStatus_t;
      Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO      : out std_logic;
      OutValid_SO  : out std_logic;
      OutReady_SI  : in  std_logic);
  end component fp_f2icasts_fmt;

  -- Integer to Float casts
  --! \copydoc work.fp_i2fcasts
  component fp_i2fcasts is
    generic (
      FORMATS    : activeFormats_t;
      INTFORMATS : activeIntFormats_t;
      LATENCY    : natural;
      TAG_WIDTH  : natural);
    port (
      Clk_CI       : in  std_logic;
      Reset_RBI    : in  std_logic;
      A_DI         : in  std_logic_vector(MAXWIDTH(INTFORMATS)-1 downto 0);
      RoundMode_SI : in  rvRoundingMode_t;
      OpMod_SI     : in  std_logic;
      SrcFmt_SI    : in  intFmt_t;
      DstFmt_SI    : in  fpFmt_t;
      Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI   : in  std_logic;
      InReady_SO   : out std_logic;
      Flush_SI     : in  std_logic;
      Z_DO         : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      Status_DO    : out rvStatus_t;
      Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO      : out std_logic;
      OutValid_SO  : out std_logic;
      OutReady_SI  : in  std_logic);
  end component fp_i2fcasts;

  component fp_i2fcasts_fmt is
    generic (
      DSTENCODING : fpFmtEncoding_t;
      INTFORMATS  : activeIntFormats_t;
      LATENCY     : natural;
      TAG_WIDTH   : natural);
    port (
      Clk_CI       : in  std_logic;
      Reset_RBI    : in  std_logic;
      A_DI         : in  std_logic_vector(MAXWIDTH(INTFORMATS)-1 downto 0);
      RoundMode_SI : in  rvRoundingMode_t;
      OpMod_SI     : in  std_logic;
      SrcFmt_SI    : in  intFmt_t;
      Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI   : in  std_logic;
      InReady_SO   : out std_logic;
      Flush_SI     : in  std_logic;
      Z_DO         : out std_logic_vector(WIDTH(DSTENCODING)-1 downto 0);
      Status_DO    : out rvStatus_t;
      Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO      : out std_logic;
      OutValid_SO  : out std_logic;
      OutReady_SI  : in  std_logic);
  end component fp_i2fcasts_fmt;

  -- Float to Float casts
  --! \copydoc work.fp_f2fcasts
  component fp_f2fcasts is
    generic (
      FORMATS   : activeFormats_t;
      LATENCY   : natural;
      TAG_WIDTH : natural);
    port (
      Clk_CI       : in  std_logic;
      Reset_RBI    : in  std_logic;
      A_DI         : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      ABox_SI      : in  fmtLogic_t;
      RoundMode_SI : in  rvRoundingMode_t;
      SrcFmt_SI    : in  fpFmt_t;
      DstFmt_SI    : in  fpFmt_t;
      Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI   : in  std_logic;
      InReady_SO   : out std_logic;
      Flush_SI     : in  std_logic;
      Z_DO         : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
      Status_DO    : out rvStatus_t;
      Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO      : out std_logic;
      OutValid_SO  : out std_logic;
      OutReady_SI  : in  std_logic);
  end component fp_f2fcasts;

  component fp_f2fcasts_fmt is
    generic (
      SRCENCODING : fpFmtEncoding_t;
      DSTENCODING : fpFmtEncoding_t;
      LATENCY     : natural;
      TAG_WIDTH   : natural);
    port (
      Clk_CI       : in  std_logic;
      Reset_RBI    : in  std_logic;
      A_DI         : in  std_logic_vector(WIDTH(SRCENCODING)-1 downto 0);
      ABox_SI      : in  std_logic;
      RoundMode_SI : in  rvRoundingMode_t;
      Tag_DI       : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI   : in  std_logic;
      InReady_SO   : out std_logic;
      Flush_SI     : in  std_logic;
      Z_DO         : out std_logic_vector(WIDTH(DSTENCODING)-1 downto 0);
      Status_DO    : out rvStatus_t;
      Tag_DO       : out std_logic_vector(TAG_WIDTH-1 downto 0);
      Zext_SO      : out std_logic;
      OutValid_SO  : out std_logic;
      OutReady_SI  : in  std_logic);
  end component fp_f2fcasts_fmt;

  -----------------------------------------------------------------------------
  -- Helper Modules
  -----------------------------------------------------------------------------

  -- FMA computational core (non Inf/NaN inputs, output needs rounding)
  --! \copydoc work.fma_core
  component fma_core is
    generic (
      EXP_BITS : natural;
      MAN_BITS : natural);
    port (
      SignA_DI, SignB_DI, SignC_DI             : in  std_logic;
      ExpA_DI, ExpB_DI, ExpC_DI                : in  unsigned(EXP_BITS-1 downto 0);
      MantA_DI, MantB_DI, MantC_DI             : in  std_logic_vector(MAN_BITS-1 downto 0);
      IsNormalA_DI, IsNormalB_DI, IsNormalC_DI : in  std_logic;
      IsZeroA_SI, IsZeroB_SI                   : in  boolean;
      ResSign_DO                               : out std_logic;
      ResExp_DO                                : out unsigned(FMAEXPWIDTH(EXP_BITS, MAN_BITS)-1 downto 0);
      ResMant_DO                               : out std_logic_vector(MAN_BITS + 2 downto 0));
  end component fma_core;

  -- Rounding Module
  --! \copydoc work.fp_rounding
  component fp_rounding is
    generic (
      EXP_BITS : positive;
      MAN_BITS : natural);
    port (
      ResultAbs_DI     : in  std_logic_vector(EXP_BITS+MAN_BITS-1 downto 0);
      ResultSign_DI    : in  std_logic;
      RoundSticky_SI   : in  std_logic_vector(1 downto 0);
      RoundMode_SI     : in  rvRoundingMode_t;
      OFBeforeRnd_SI   : in  boolean;
      ResZero_SI       : in  boolean;
      EffSub_SI        : in  boolean;
      RoundedResult_DO : out std_logic_vector(EXP_BITS+MAN_BITS downto 0));
  end component fp_rounding;

  -- Pipeline Stages (put at outputs inside operations)
  --! \copydoc work.fp_pipe
  component fp_pipe is
    generic (
      WIDTH     : natural;
      LATENCY   : natural;
      TAG_WIDTH : natural);
    port (
      Clk_CI         : in  std_logic;
      Reset_RBI      : in  std_logic;
      Result_DI      : in  std_logic_vector(WIDTH-1 downto 0);
      Status_DI      : in  rvStatus_t;
      Tag_DI         : in  std_logic_vector(TAG_WIDTH-1 downto 0);
      InValid_SI     : in  std_logic;
      InReady_SO     : out std_logic;
      Flush_SI       : in  std_logic;
      ResultPiped_DO : out std_logic_vector(WIDTH-1 downto 0);
      StatusPiped_DO : out rvStatus_t;
      TagPiped_DO    : out std_logic_vector(TAG_WIDTH-1 downto 0);
      OutValid_SO    : out std_logic;
      OutReady_SI    : in  std_logic);
  end component fp_pipe;

  -- Output Arbitration
  --! \copydoc work.fp_arbiter
  component fp_arbiter is
    generic (
      DATA_WIDTH : natural;
      NUM_INPUTS : natural;
      TAG_WIDTH  : natural);
    port (
      InResults_DI  : in  slArray2d_t(0 to NUM_INPUTS-1, DATA_WIDTH-1 downto 0);
      InStatuses_DI : in  statusArray_t(0 to NUM_INPUTS-1);
      InTags_DI     : in  slArray2d_t(0 to NUM_INPUTS-1, TAG_WIDTH-1 downto 0);
      InValid_SI    : in  std_logic_vector(0 to NUM_INPUTS-1);
      InReady_SO    : out std_logic_vector(0 to NUM_INPUTS-1);
      Priorities_SI : in  std_logic_vector(clog2(NUM_INPUTS)-1 downto 0);
      OutResult_DO  : out std_logic_vector(DATA_WIDTH-1 downto 0);
      OutStatus_DO  : out rvStatus_t;
      OutTag_DO     : out std_logic_vector(TAG_WIDTH-1 downto 0);
      OutValid_SO   : out std_logic;
      OutReady_SI   : in  std_logic;
      OutIdx_SO     : out std_logic_vector(clog2(NUM_INPUTS)-1 downto 0));
  end component fp_arbiter;



  -----------------------------------------------------------------------------
  -- Leading Zero Counter, found in common_cells dependency
  -----------------------------------------------------------------------------

  component find_first_one is
    generic (
      WIDTH : natural;
      FLIP  : natural);
    port (
      in_i        : in  std_logic_vector(WIDTH-1 downto 0);
      first_one_o : out unsigned(clog2(WIDTH)-1 downto 0);
      no_ones_o   : out boolean);
  end component find_first_one;


end package fpnew_comps_pkg;
