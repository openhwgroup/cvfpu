-------------------------------------------------------------------------------
-- Title      : Constants from fpnew_pkg for mixed-language interfaces
-- Project    :
-------------------------------------------------------------------------------
-- File       : fpnew_pkg_constants.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-04-10
-- Last update: 2018-04-11
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Exposes constants from the fpnew_pkg package as ports
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


entity fpnew_pkg_constants is

  port (
    --OPERATIONS--------------------------------------------------------------
    OP_NUMBITS : out integer;
    OP_FMADD   : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_FNMSUB  : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_ADD     : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_MUL     : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_DIV     : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_SQRT    : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_SGNJ    : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_MINMAX  : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_CMP     : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_CLASS   : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_F2I     : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_I2F     : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_F2F     : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_CPKAB   : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    OP_CPKCD   : out std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0);
    --FP FORMATS--------------------------------------------------------------
    FMT_NUMBITS : out integer;
    FMT_FP32    : out std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    FMT_FP64    : out std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    FMT_FP16    : out std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    FMT_FP8     : out std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    FMT_FP16ALT : out std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    FMT_CUST1   : out std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    FMT_CUST2   : out std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    FMT_CUST3   : out std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0);
    --INT FORMATS-------------------------------------------------------------
    IFMT_NUMBITS : out integer;
    IFMT_INT8    : out std_logic_vector(clog2(intFmt_t'pos(intFmt_t'high))-1 downto 0);
    IFMT_INT16   : out std_logic_vector(clog2(intFmt_t'pos(intFmt_t'high))-1 downto 0);
    IFMT_INT32   : out std_logic_vector(clog2(intFmt_t'pos(intFmt_t'high))-1 downto 0);
    IFMT_INT64   : out std_logic_vector(clog2(intFmt_t'pos(intFmt_t'high))-1 downto 0));

end entity fpnew_pkg_constants;


architecture arch of fpnew_pkg_constants is

  -----------------------------------------------------------------------------
  -- Operation Encoding
  -----------------------------------------------------------------------------
  constant C_OP_NUMBITS : integer := clog2(fpOp_t'pos(fpOp_t'high));
  constant C_OP_FMADD   : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(FMADD), C_OP_NUMBITS));
  constant C_OP_FNMSUB  : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(FNMSUB), C_OP_NUMBITS));
  constant C_OP_ADD     : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(ADD), C_OP_NUMBITS));
  constant C_OP_MUL     : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(MUL), C_OP_NUMBITS));
  constant C_OP_DIV     : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(DIV), C_OP_NUMBITS));
  constant C_OP_SQRT    : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(SQRT), C_OP_NUMBITS));
  constant C_OP_SGNJ    : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(SGNJ), C_OP_NUMBITS));
  constant C_OP_MINMAX  : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(MINMAX), C_OP_NUMBITS));
  constant C_OP_CMP     : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(CMP), C_OP_NUMBITS));
  constant C_OP_CLASS   : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(CLASS), C_OP_NUMBITS));
  constant C_OP_F2I     : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(F2I), C_OP_NUMBITS));
  constant C_OP_I2F     : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(I2F), C_OP_NUMBITS));
  constant C_OP_F2F     : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(F2F), C_OP_NUMBITS));
  constant C_OP_CPKAB   : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(CPKAB), C_OP_NUMBITS));
  constant C_OP_CPKCD   : std_logic_vector(C_OP_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpOp_t'pos(CPKCD), C_OP_NUMBITS));

  -----------------------------------------------------------------------------
  -- FP Format Encoding
  -----------------------------------------------------------------------------
  constant C_FMT_NUMBITS : integer := clog2(fpFmt_t'pos(fpFmt_t'high));
  constant C_FMT_FP32    : std_logic_vector(C_FMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpFmt_t'pos(FP32), C_FMT_NUMBITS));
  constant C_FMT_FP64    : std_logic_vector(C_FMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpFmt_t'pos(FP64), C_FMT_NUMBITS));
  constant C_FMT_FP16    : std_logic_vector(C_FMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpFmt_t'pos(FP16), C_FMT_NUMBITS));
  constant C_FMT_FP8     : std_logic_vector(C_FMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpFmt_t'pos(FP8), C_FMT_NUMBITS));
  constant C_FMT_FP16ALT : std_logic_vector(C_FMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpFmt_t'pos(FP16ALT), C_FMT_NUMBITS));
  constant C_FMT_CUST1   : std_logic_vector(C_FMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpFmt_t'pos(CUST1), C_FMT_NUMBITS));
  constant C_FMT_CUST2   : std_logic_vector(C_FMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpFmt_t'pos(CUST2), C_FMT_NUMBITS));
  constant C_FMT_CUST3   : std_logic_vector(C_FMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(fpFmt_t'pos(CUST3), C_FMT_NUMBITS));

 -----------------------------------------------------------------------------
  -- FP Format Encoding
  -----------------------------------------------------------------------------
  constant C_IFMT_NUMBITS : integer := clog2(intFmt_t'pos(intFmt_t'high));
  constant C_IFMT_INT8    : std_logic_vector(C_IFMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(intFmt_t'pos(INT8), C_IFMT_NUMBITS));
  constant C_IFMT_INT16   : std_logic_vector(C_IFMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(intFmt_t'pos(INT16), C_IFMT_NUMBITS));
  constant C_IFMT_INT32   : std_logic_vector(C_IFMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(intFmt_t'pos(INT32), C_IFMT_NUMBITS));
  constant C_IFMT_INT64   : std_logic_vector(C_IFMT_NUMBITS-1 downto 0) := std_logic_vector(to_unsigned(intFmt_t'pos(INT64), C_IFMT_NUMBITS));

begin -- architecture arch

  OP_NUMBITS <= C_OP_NUMBITS;
  OP_FMADD   <= C_OP_FMADD;
  OP_FNMSUB  <= C_OP_FNMSUB;
  OP_ADD     <= C_OP_ADD;
  OP_MUL     <= C_OP_MUL;
  OP_DIV     <= C_OP_DIV;
  OP_SQRT    <= C_OP_SQRT;
  OP_SGNJ    <= C_OP_SGNJ;
  OP_MINMAX  <= C_OP_MINMAX;
  OP_CMP     <= C_OP_CMP;
  OP_CLASS   <= C_OP_CLASS;
  OP_F2I     <= C_OP_F2I;
  OP_I2F     <= C_OP_I2F;
  OP_F2F     <= C_OP_F2F;
  OP_CPKAB   <= C_OP_CPKAB;
  OP_CPKCD   <= C_OP_CPKCD;

  FMT_NUMBITS <= C_FMT_NUMBITS;
  FMT_FP32    <= C_FMT_FP32;
  FMT_FP64    <= C_FMT_FP64;
  FMT_FP16    <= C_FMT_FP16;
  FMT_FP8     <= C_FMT_FP8;
  FMT_FP16ALT <= C_FMT_FP16ALT;
  FMT_CUST1   <= C_FMT_CUST1;
  FMT_CUST2   <= C_FMT_CUST2;
  FMT_CUST3   <= C_FMT_CUST3;

  -----------------------------------------------------------------------------
  -- FP Format Encoding
  -----------------------------------------------------------------------------
  IFMT_NUMBITS <= C_IFMT_NUMBITS;
  IFMT_INT8    <= C_IFMT_INT8;
  IFMT_INT16   <= C_IFMT_INT16;
  IFMT_INT32   <= C_IFMT_INT32;
  IFMT_INT64   <= C_IFMT_INT64;

end architecture arch;
