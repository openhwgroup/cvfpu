-------------------------------------------------------------------------------
-- Title      : TransPrecision Floating-Point Unit Package
-- Project    :
-------------------------------------------------------------------------------
-- File       : fpnew_pkg.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-08
-- Last update: 2018-06-20
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

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

--! @brief TransPrecision Floating-Point Unit Package
--!
--! Provides types, functions and constants used in the transprecision floating
--! point unit effort.
package fpnew_pkg is

  --===========================================================================
  -- General Utility Type Definitions and Procedures
  --===========================================================================

  --! @brief 2d std_logic array to use as array of std_logic_vector
  --! @details Serves as a workaround of shortcomings of VHDL-93 where array
  --! types cannot remain unconstrained in type declarations or port lists.
  type slArray2d_t is array (natural range <>, natural range <>) of std_logic;

  --! @brief Set array row at index idx to std_logic_vector signal slv
  procedure set_row (signal arr : out slArray2d_t; constant idx : in natural;
                     signal slv : in  std_logic_vector);

  --! @brief Set array row at index idx to std_logic_vector variable slv
  procedure set_row_var (signal arr : out slArray2d_t; constant idx : in natural;
                     variable slv : in  std_logic_vector);

  --! @brief Bind std_logic_vector signal slv to array row at index idx
  procedure get_row (signal slv : out std_logic_vector;
                     signal arr : in  slArray2d_t; constant idx : in natural);


  --! @brief Bind row of one array to a row in another array
  procedure copy_row (signal to_arr     : out slArray2d_t;
                      constant to_idx   : in  natural;
                      signal from_arr   : in  slArray2d_t;
                      constant from_idx : in  natural);

  --===========================================================================
  -- RISC-V-Specific Type Definitions
  --===========================================================================

  --! @brief RISC-V FP classification block flags
  --! @details The ten RISC-V FP classification flags are:
  --! | Enumerator | Description
  --! |:----------:|:-----------
  --! | NEGINF     | -&infin;
  --! | NEGNORM    | negative normal number
  --! | NEGSUBNORM  | negative denormal number
  --! | NEGZERO    | -0
  --! | POSZERO    | +0
  --! | POSSUBNORM  | positive denormal number
  --! | POSNORM    | positive normal number
  --! | POSINF     | +&infin;
  --! | SNAN       | signaling NaN
  --! | QNAN       | quiet NaN
  --! Layout of RISC-V FP classification block:
  --! <pre>
  --! ---------------------------------------------------------------------------------------------------
  --! [ QNAN | SNAN | POSINF | POSNORM | POSSUBNORM | POSZERO | NEGZERO | NEGSUBNORM | NEGNORM | NEGINF ]
  --! ---9------8-------7---------6----------5-----------4---------3----------2-----------1--------0-----
  --! </pre>
  --! @note Call the to_slv(rvClassBit_t) function to convert an enumerator into its
  --! corresponding classification block representation
  type rvClassBit_t is (NEGINF, NEGNORM, NEGSUBNORM, NEGZERO,
                        POSZERO, POSSUBNORM, POSNORM, POSINF,
                        SNAN, QNAN);


  --! @brief RISC-V rounding modes
  --! @details The five floating-point rounding modes in RISC-V are:
  --! | Enumerator | Meaning
  --! |:----------:|:-------
  --! |   RNE      | Round to Nearest, ties to Even
  --! |   RTZ      | Round towards Zero
  --! |   RDN      | Round Down (towards -&infin;)
  --! |   RUP      | Round Up (towards &infin;)
  --! |   RMM      | Round to Nearest, ties to Max Magnitude
  --! | *others*   | *invalid*
  --! \note Enumerated literals are encoded in order!
  type rvRoundingMode_t is (RNE, RTZ, RDN, RUP, RMM);  -- RISC-V rounding modes


  --! @brief RISC-V FP exception flags
  --! @details The five RISC-V FP exception flags are:
  --! | Flag Name | Enumerator | Description
  --! |:---------:|:----------:|:-----------
  --! | NX        | NX         | Inexact
  --! | UF        | UF         | Underflow
  --! | OF        | OvF        | Overflow ('OF' is a reserved VHDL keyword
  --! | DZ        | DZ         | Division by Zero
  --! | NV        | NV         | Invalid Operation
  type rvStatusBit_t is (NX, UF, OvF, DZ, NV);

  --! @brief Type holding the five fflags bits in order
  --! @details Addressed by the five enumerators from rvStatusBit_t.
  --! Layout of RISC-V FP exception flags (fflags) in fcsr:
  --! <pre>
  --! --------------------------
  --! [ NV | DZ | OF | UF | NX ] fcsr
  --! ---4----3----2----1----0--
  --! </pre>
  --! \note rvStatus_t is closely related to std_logic_vector in VHDL-2008,
  --! so you can directly cast in between the two types.
  --! Synopsys DC doesn't have VHDL-2008 support and  will complain about
  --! direct casts while modelsim will not. For simulation using direct casts
  --! to/from slv works and the to_slv(rvStatus_t) as well as
  --! to_rvStatus() functions are not needed.
  type rvStatus_t is array (rvStatusBit_t) of std_logic;

  --! @brief Array of std_logic_vector values
  --! @details Array of STD_LOGIC_VECTOR to be used when pipelining operations
  --! \note Type must be constrained upon declaration in a signal
  --! \warning VHDL-2008 needed for arrays of unconstrained types. Only
  --! uncomment when your tools support VHDL-2008
--  type slvPipe_t is array (natural range <>) of std_logic_vector;

  --! @brief Array of status flags
  --! @details Array of status flags to be used when pipelining operations
  type statusArray_t is array (natural range <>) of rvStatus_t;

  --===========================================================================
  -- Custom ISA-Extension-Specific Type Definitions
  --===========================================================================

  --! @brief Vectorial FP classification block mask bits
  --! @details The six vectorial FP classification block mask bits are:
  --! | Enumerator | Description
  --! |:----------:|:-----------
  --! | INF        | Infinity
  --! | NORM       | Normal Number
  --! | SUBNORM    | Subnmormal Number
  --! | ZERO       | Zero
  --! | SNAN       | Signalling NaN
  --! | QNAN       | Quiet NaN
  --! Layout of the mask bits in the classification block:
  --! <pre>
  --! ---------------------------------------------
  --! [ QNAN | SNAN | ZERO | SUBNORM | NORM | INF ] vecClassBlock[5:0]
  --! ---5------4------3--------2-------1------0---
  --! </pre>
  type vecClassBit_t is (INF, NORM, SUBNORM, ZERO, SNAN, QNAN);

  --! @brief Internal encoding of vectorial classification block
  --! @details Holds the classification information for vectorial values.
  type vecClassBlock_t is record
    Sign  : std_logic;
    Class : vecClassBit_t;
  end record vecClassBlock_t;

  --===========================================================================
  -- FP Operation-Dependent Type Definitions
  --===========================================================================

  --! @brief Floating-point operation groups
  --! @details Floating-point operations are divided into logical groups that
  --! are used for the structural generation of the FPnew. Generally, a
  --! subunit inside the FPnew can take care of all operations within an
  --! operation group.
  --! The groups are:
  --! | Enumerator | Group
  --! |:----------:|-------
  --! | ADDMUL     | Computational operations involving addition and multiplication
  --! | DIVSQRT    | Computational operations involving division and square root
  --! | NONCOMP    | Non-computational operations
  --! | CONV       | Conversions between formats
  --! \warning Enumerated literals are encoded in order! Only add entries at
  --! <em> THE END </em> of the list or you will break backwards compatibility
  --! to fixed upstream circuits.
  type fpOpGroup_t is (ADDMUL, DIVSQRT, NONCOMP, CONV);

  --! @brief Array of booleans for each operation group
  --! @details Array of BOOLEAN that hold a value for each \ref fpOpGroup_t
  --! "FPOPGROUP_T"
  type opGroupBooleans_t is array (fpOpGroup_t) of boolean;

  --! @brief Array of std_logic for each operation group
  --! @details Array of STD_LOGIC that hold a value for each \ref fpOpGroup_t
  --! "FPOPGROUP_T"
  type opGroupLogic_t is array (fpOpGroup_t) of std_logic;

  --! @brief Array of naturals for each operation group
  --! @details Array of NATURAL that hold a value for each \ref fpOpGroup_t
  --! "FPOPGROUP_T"
  type opGroupNaturals_t is array (fpOpGroup_t) of natural;

  --! @brief Array of std_logic_vector values for each operation group
  --! @details Array of STD_LOGIC_VECTOR that hold a value for each \ref
  --! fpOpGroup_t "FPOPGROUP_T"
  --! \note Type must be constrained upon declaration in a signal
  --! \warning VHDL-2008 needed for arrays of unconstrained types. Only
  --! uncomment when your tools support VHDL-2008
--  type opGroupSlVectors_t is array (fpOpGroup_t) of std_logic_vector;
  type opGroupSlArray2d_t is array (fpOpGroup_t range <>, natural range <>) of std_logic;

  --! @brief Array of status flags for each operation group
  --! @details Array of RVSTATUS_T that hold a value for each \ref
  --! fpOpGroup_t "FPOPGROUP_T"
  type opGroupStatus_t is array (fpOpGroup_t) of rvStatus_t;

  --! @brief Encoding of floating-point operations
  --! @details These options make up the set of floating-point operations
  --! available in the unit. Some operations can be further modified by using
  --! the \c OpMod_SI signal of applicable instances.
  --! | Enumerator | Operation | Alternate Operation (\c OpMod_SI='1') | Operation Group
  --! |:----------:|-----------|---------------------------------------|----------------
  --! | FMADD      | Fused Multiply-Addition | Fused Multiply-Subraction | \c ADDMUL
  --! | FNMSUB     | Negated Fused Multiply-Subraction | Negated Fused Multiply-Addition | \c ADDMUL
  --! | ADD        | Addition | Subtraction | \c ADDMUL
  --! | MUL        | Multiplication | \e n/a | \c ADDMUL
  --! | DIV        | Division | \e n/a | *n/a*
  --! | SQRT       | Square Root | \e n/a | *n/a*
  --! | SGNJ       | Sign Injection, Op encoded in \c RoundMode_SI | \e n/a | \c NONCOMP
  --! | MINMAX     | Minimum/Maximum, Op encoded in \c RoundMode_SI | \e n/a | \c NONCOMP
  --! | CMP        | Comparisons | \e n/a | \c NONCOMP
  --! | CLASS      | Classify | \e n/a | \c NONCOMP
  --! | F2I        | Cast: Float to Signed Integer | Cast: Float to Unsigned Integer | \c CONV
  --! | I2F        | Cast: Signed Integer to Float | Cast: Unsigned Integer to Float | \c CONV
  --! | F2F        | Cast: Float to Float | \e n/a | \c CONV
  --! | CPKAB      | Cast and Pack to Entries 0,1  | Cast and Pack to Entries 2,3 | \c CONV
  --! | CPKCD      | Cast and Pack to Entries 4,5  | Cast and Pack to Entries 6,7 | \c CONV
  --! \warning Enumerated literals are encoded in order! Only add entries at
  --! <em> THE END </em> of the list or you will break backwards compatibility
  --! to fixed upstream circuits.
  type fpOp_t is (FMADD, FNMSUB, ADD, MUL, DIV, SQRT,
                  SGNJ, MINMAX, CMP, CLASS,
                  F2I, I2F, F2F, CPKAB, CPKCD);

  --! @brief Array of booleans for each operation
  --! @details Array of BOOLEAN that hold a value for each \ref fpOp_t
  --! "FPOP_T"
  type opBooleans_t is array (fpOp_t) of boolean;

  --===========================================================================
  -- Status Type Helper Functions
  --===========================================================================

  --! @brief Combine status words by logic-OR
  --! @returns The combined status of the input stati from STAUSARRAY_T arr
  --! @retval RVSTATUS_T
  function combined_status (constant arr : statusArray_t)
    return rvStatus_t;

  --===========================================================================
  -- Generic FP-Format-Dependent Helper Functions Returning Constants
  --===========================================================================

  --! @brief Bias for generic FP format
  --! @returns The bias for a symmetrically biased IEEE 754-conformant FP
  --! format with EXP_BITS exponent bits
  --! @retval NATURAL
  function BIAS (constant EXP_BITS : natural)
    return natural;

  --! @brief Maximum encoded exponent value for generic FP format
  --! @returns The largest encoded exponent for the FP format, corresponding to
  --! &infin; or NaN
  --! @retval UNSIGNED(EXP_BITS-1 downto 0)
  function MAXEXP (constant EXP_BITS : natural)
    return unsigned;

  --! @brief Infinity bit-pattern for FP format
  --! @returns Positive infinity for the FP format
  --! @retval STD_LOGIC_VECTOR(EXP_BITS+MAN_BITS downto 0)
  function INF (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector;

  --! @brief Canonical NaN bit-pattern for FP format
  --! @returns The canonical quiet NaN for the FP format
  --! @retval STD_LOGIC_VECTOR(EXP_BITS+MAN_BITS downto 0)
  function NAN (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector;

  --! @brief Bit-pattern for +1.0 for FP format
  --! @returns Positive one (+1.0) encoded in the FP format
  --! @retval STD_LOGIC_VECTOR(EXP_BITS+MAN_BITS downto 0)
  function ONE (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector;

  --! @brief Bit-pattern for -0.0 for FP format
  --! @returns Negative zero (-0.0) encoded in the FP format
  --! @retval STD_LOGIC_VECTOR(EXP_BITS+MAN_BITS downto 0)
  function NEGZERO (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector;

  --! @brief Bit-pattern for largest normal absolute number in FP format
  --! @returns The largest normal number absolute for the FP format
  --! @retval STD_LOGIC_VECTOR(EXP_BITS+MAN_BITS-1 downto 0)
  function MAXNORMAL (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector;

  --! @brief Internal exponent widths for FP format
  --! @returns width of internal SIGNED exponents for FMA
  --! @retval NATURAL
  function FMAEXPWIDTH (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return natural;

  --===========================================================================
  -- FP-Operation Type Dependent Helper Functions
  --===========================================================================

  --! @brief Get operation group for operation
  --! @returns The opgroup associated with FPOP_T op
  --! @retval FPOPGROUP_T
  function getFpOpGroup (constant op : fpOp_t)
    return fpOpGroup_t;

  --! @brief Check for set operation group entry
  --! @returns TRUE when any bit in OPGROUPLOGIC_T val is set, FALSE otherwise
  --! @retval BOOLEAN
  --! /note This function is only needed when no VHDL-2008 support is present,
  --! as OPGROUPLOGIC_T is closely related to std_logic_vector in VHDL-2008 and
  --! can be directly cast and fed into or_reduce()
  function anySet (constant val : opGroupLogic_t)
    return boolean;

  --! @brief First set operation group entry
  --! @returns The first operation group that is set in OPGROUPLOGIC_T val
  --! @retval FPOPGROUP_T
  function findFirstSet (constant val : opGroupLogic_t)
    return fpOpGroup_t;

  --! @brief Set array row at format index fmt to std_logic_vector variable slv
  procedure set_row_var (signal arr   : out opGroupSlArray2d_t; constant opgrp : in fpOpGroup_t;
                         variable slv   : in  std_logic_vector);


  --===========================================================================
  -- Arithmetic Operators
  --===========================================================================

  --! @brief Ceiling of base-2 logarithm
  --! @returns base-2 logarithm of NATURAL arg rounded up to next integer
  --! @retval NATURAL
  function clog2 (constant arg : natural)
    return natural;

  --TODO ADD FLOG2

  --===========================================================================
  -- Conversion Functions
  --===========================================================================

  --! @brief fpOp_t from std_logic_vector
  --! @details Converts the floating-point operation encoding in
  --! STD_LOGIC_VECTOR slvOp to the internal enumerated type \ref fpOp_t "FPOP_T"
  --! @returns The floating-point operation as an \ref fpOp_t "FPOP_T"
  --! @retval FPOP_T
  function to_fpOp (slvOp : std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0))
    return fpOp_t;

  --! @brief 0/1 from boolean
  --! @returns 1 when BOOLEAN bool is true, 0 otherwise
  --! @retval NATURAL
  function to_integer (bool : boolean)
    return natural;

  --! @brief 0/1 from std_logic
  --! @returns 1 when STD_LOGIC sl is '1', 0 otherwise
  --! @retval NATURAL
  function to_integer (sl : std_logic)
    return natural;

  --! @brief rvRoundingMode_t from std_logic_vector
  --! @details Converts the RISC-V rnd field in STD_LOGIC_VECTOR slvRound to
  --! the internal enumerated type \ref rvRoundingMode_t "RVROUNDINGMODE_T"
  --! @returns The RISC-V rnd field as an \ref rvRoundingMode_t "RVROUNDINGMODE_T"
  --! @retval RVROUNDINGMODE_T
  function to_rvRoundMode (slvRound : std_logic_vector(2 downto 0))
    return rvRoundingMode_t;

  --! @brief rvStatus_t from std_logic_vector
  --! @details Converts the RISC-V fflags in STD_LOGIC_VECTOR rvStat to the
  --! internal enumerated vector type \ref rvStatus_t "RVSTATUS_T"
  --! @returns The RISC-V fflags as an \ref rvStatus_t "RVSTATUS_T"
  --! @retval RVSTATUS_T
  function to_rvStatus(slvStat : std_logic_vector(4 downto 0))
    return rvStatus_t;

  --! @brief opGroupLogic_t from std_logic_vector
  --! @returns The opgroup-indexed vector
  --! @retval OPGROUPLOGIC_T
  function to_opGroupLogic ( slv : std_logic_vector(0 to fpOpGroup_t'pos(fpOpGroup_t'high)))
    return opGroupLogic_t;

  --! @brief std_logic from boolean
  --! @returns STD_LOGIC '1' when true, '0' when false
  --! @retval STD_LOGIC
  function to_sl (bool : boolean)
    return std_logic;

  --! @brief std_logic_vector from rvClassBit_t
  --! @returns The enumerated classification in RVCLASSBIT_T class as a
  --! STD_LOGIC_VECTOR
  --! @retval STD_LOGIC_VECTOR(3 downto 0)
  function to_slv (class : rvClassBit_t)
    return std_logic_vector;

  --! @brief std_logic_vector classification block from rvClassBit_t
  --! @returns The RISC-V classification block as a STD_LOGIC_VECTOR (one-hot)
  --! @retval STD_LOGIC_VECTOR(9 downto 0)
  function to_slv_unpack (class : rvClassBit_t)
    return std_logic_vector;

  --! @brief std_logic_vector classification block from packed std_logic_vector
  --! @returns The RISC-V classification block as a STD_LOGIC_VECTOR (one-hot)
  --! @retval STD_LOGIC_VECTOR(9 downto 0)
  function to_slv_unpack (class : std_logic_vector(3 downto 0))
    return std_logic_vector;

  --! @brief std_logic_vector from rvRoundingMode_t
  --! @returns The RISC-V rnd field as a STD_LOGIC_VECTOR
  --! @retval STD_LOGIC_VECTOR(2 downto 0)
  function to_slv (rvRound : rvRoundingMode_t)
    return std_logic_vector;

  --! @brief std_logic_vector from rvStatus_t
  --! @returns The RISC-V fflags as a STD_LOGIC_VECTOR
  --! @retval STD_LOGIC_VECTOR(4 downto 0)
  function to_slv (rvStat : rvStatus_t)
    return std_logic_vector;

  --! @brief std_logic_vector from opGroupLogic_t
  --! @returns The opgroup-indexed vector as a STD_LOGIC_VECTOR
  --! @retval STD_LOGIC_VECTOR(0 to fpOpGroup_t'pos(fpOpGroup_t'high)
  function to_slv (opgrpvec : opGroupLogic_t)
    return std_logic_vector;

  --! @brief std_logic_vector vectorial classification block from vecClassBlock_t
  --! @returns The vectorial classification block as a STD_LOGIC_VECTOR
  --! @retval STD_LOGIC_VECTOR(7 downto 0)
  function to_slv (constant cb : vecClassBlock_t)
    return std_logic_vector;

  --===========================================================================
  -- Relational Operators
  --===========================================================================

  --! @brief Minimum
  --! @returns The smaller operand of l and r.
  --! @retval INTEGER
  function minimum (l : integer; r : integer)
    return integer;

  --! @brief Maximum
  --! @returns The larger operand of l and r.
  --! @retval INTEGER
  function maximum (l : integer; r : integer)
    return integer;


  --===========================================================================
  -- Logical Operators
  --===========================================================================

  --! @brief OR reduction
  --! @returns logic OR of all bits in STD_LOGIC_VECTOR arg
  --! @retval STD_LOGIC
  function or_reduce (arg : std_logic_vector)
    return std_logic;

  --! @brief AND reduction
  --! @returns logic AND of all bits in STD_LOGIC_VECTOR arg
  --! @retval STD_LOGIC
  function and_reduce (arg : std_logic_vector)
    return std_logic;

  --! @brief OR reduction
  --! @returns logic OR of all bits in UNSIGNED vector arg
  --! @retval STD_LOGIC
  function or_reduce (arg : unsigned)
    return std_logic;

  --! @brief AND reduction
  --! @returns logic AND of all bits in UNSIGNED vector arg
  --! @retval STD_LOGIC
  function and_reduce (arg : unsigned)
    return std_logic;

  --! @brief OR reduction
  --! @returns logic OR of all bits in SIGNED vector arg
  --! @retval STD_LOGIC
  function or_reduce (arg : signed)
    return std_logic;

  --! @brief AND reduction
  --! @returns logic AND of all bits in SIGNED vector arg
  --! @retval STD_LOGIC
  function and_reduce (arg : signed)
    return std_logic;

end package fpnew_pkg;


--=============================================================================
--=======================    Package    Body    ===============================
--=============================================================================

package body fpnew_pkg is

--===========================Local Functions===================================


--=========================Exported Functions==================================

  procedure set_row (signal arr : out slArray2d_t; constant idx : in natural;
                     signal slv : in  std_logic_vector) is
  begin  -- procedure set_row
    for i in slv'range loop
      arr(idx, i) <= slv(i);
    end loop;  -- i
  end procedure set_row;

  -----------------------------------------------------------------------------

  procedure set_row_var (signal arr : out slArray2d_t; constant idx : in natural;
                         variable slv : in  std_logic_vector) is
  begin  -- procedure set_row_var
    for i in slv'range loop
      arr(idx, i) <= slv(i);
    end loop;  -- i
  end procedure set_row_var;

  -----------------------------------------------------------------------------

  procedure get_row (signal slv : out std_logic_vector;
                     signal arr : in  slArray2d_t; constant idx : in natural) is
  begin  -- procedure get_row
    for i in slv'range loop
      slv(i) <= arr(idx, i);
    end loop;  -- i
  end procedure get_row;

  -----------------------------------------------------------------------------

  procedure copy_row (signal to_arr   : out slArray2d_t; constant to_idx : in natural;
                      signal from_arr : in  slArray2d_t; constant from_idx : in natural) is
  begin  -- procedure copy_row
    for i in from_arr'range(2) loop
      to_arr(to_idx, i) <= from_arr(from_idx, i);
    end loop;  -- i
  end procedure copy_row;

  -----------------------------------------------------------------------------

  function combined_status (constant arr : statusArray_t)
    return rvStatus_t is
    variable stat : rvStatus_t := (others => '0');
  begin  -- function combined_status
    for i in arr'range loop
      stat := to_rvStatus(to_slv(stat) or to_slv(arr(i)));
    end loop;  -- i
    return stat;
  end function combined_status;

  -----------------------------------------------------------------------------

  function BIAS (constant EXP_BITS : natural)
    return natural is
  begin  -- function BIAS
    return 2**(EXP_BITS-1)-1;
  end function BIAS;

  -----------------------------------------------------------------------------

  function MAXEXP (constant EXP_BITS : natural)
    return unsigned is
    variable res : unsigned(EXP_BITS-1 downto 0);
  begin  -- function MAXEXP
    res := (others => '1');
    return res;
  end function MAXEXP;

  -----------------------------------------------------------------------------

  function INF (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector is
    variable res : std_logic_vector(EXP_BITS+MAN_BITS downto 0);
  begin  -- function INF
    res                                      := (others => '0');
    res(EXP_BITS+MAN_BITS-1 downto MAN_BITS) := (others => '1');
    return res;
  end function INF;

  -----------------------------------------------------------------------------

  function NAN (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector is
    variable res : std_logic_vector(EXP_BITS+MAN_BITS downto 0);
  begin  -- function NAN
    res                                      := (others => '0');
    res(EXP_BITS+MAN_BITS-1 downto MAN_BITS) := (others => '1');
    res(MAN_BITS-1)                          := '1';
    return res;
  end function NAN;

  -----------------------------------------------------------------------------

  function ONE (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector is
    variable res : std_logic_vector(EXP_BITS+MAN_BITS downto 0);
  begin  -- function ONE
    res := (others => '0');
    res(EXP_BITS+MAN_BITS-1 downto MAN_BITS)
      := std_logic_vector(to_unsigned(BIAS(EXP_BITS), EXP_BITS));
    return res;
  end function ONE;

  -----------------------------------------------------------------------------

  function NEGZERO (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector is
    variable res : std_logic_vector(EXP_BITS+MAN_BITS downto 0);
  begin  -- function ONE
    res := (others => '0');
    res(EXP_BITS+MAN_BITS) := '1';
    return res;
  end function NEGZERO;

  -----------------------------------------------------------------------------

  function MAXNORMAL (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return std_logic_vector is
    variable res : std_logic_vector(EXP_BITS+MAN_BITS-1 downto 0);
  begin  -- function MAXNORMAL
    -- Largest normal has an exponent of MAXEXP-1 and all ones mantissa
    res(EXP_BITS+MAN_BITS-1 downto MAN_BITS)
      := std_logic_vector(resize(MAXEXP(EXP_BITS) - 1, EXP_BITS));
    res(MAN_BITS-1 downto 0) := (others => '1');
    return res;
  end function MAXNORMAL;

  -----------------------------------------------------------------------------

  function FMAEXPWIDTH (constant EXP_BITS : natural; constant MAN_BITS : natural)
    return natural is
  begin  -- function FMAEXPWIDTH
    return maximum(EXP_BITS+2, clog2(2*(MAN_BITS+1)+3));
  end function FMAEXPWIDTH;

  -----------------------------------------------------------------------------

  function getFpOpGroup (constant op : fpOp_t)
    return fpOpGroup_t is
  begin  -- function getFpOpGroup
    case op is
      when FMADD | FNMSUB | ADD | MUL => return ADDMUL;
      when DIV | SQRT => return DIVSQRT;
      when SGNJ | MINMAX | CMP | CLASS => return NONCOMP;
      when F2I | I2F | F2F | CPKAB | CPKCD => return CONV;
      when others =>
        -- pragma synthesis_off
        report "Operation '" & fpOp_t'image(op) & "' is not bound to an OpGroup!" severity failure;
        -- pragma synthesis_on
        return fpOpGroup_t'left;
    end case;
  end function getFpOpGroup;

  -----------------------------------------------------------------------------

  function anySet (constant val : opGroupLogic_t)
    return boolean is
  begin  -- function anyValid
    for grp in fpOpGroup_t loop
      if val(grp) = '1' then
        return true;
      end if;
    end loop;  -- grp
    return false;                       -- default return
  end function anySet;

  -----------------------------------------------------------------------------

  function findFirstSet (constant val : opGroupLogic_t)
    return fpOpGroup_t is
  begin  -- function findFirstSet
    for grp in fpOpGroup_t loop
      if val(grp) = '1' then
        return grp;
      end if;
    end loop;  -- grp
    return fpOpGroup_t'left;            -- default return
  end function findFirstSet;

  -----------------------------------------------------------------------------

   procedure set_row_var (signal arr   : out opGroupSlArray2d_t;  constant opgrp : in fpOpGroup_t;
                      variable slv : in  std_logic_vector) is
  begin  -- procedure set_row_var
    for i in slv'range loop
      arr(opgrp,i) <= slv(i);
    end loop;  -- i
  end procedure set_row_var;

  -----------------------------------------------------------------------------

  function clog2 (constant arg : natural) return natural is
    variable res   : natural := 0;
    variable a_int : natural := arg-1;
  begin  -- function clog2
    --if (arg = 0) then
    --  return 0;
    --end if;
    while a_int /= 0 loop
      a_int := a_int / 2;
      res   := res +1;
    end loop;
    return res;
  end function clog2;

  -----------------------------------------------------------------------------

  function to_integer (bool : boolean) return natural is
  begin  -- function to_integer
    if bool then
      return 1;
    else
      return 0;
    end if;
  end function to_integer;

  -----------------------------------------------------------------------------

  function to_integer (sl : std_logic) return natural is
  begin  -- function to_integer
    if sl = '1' then
      return 1;
    else
      return 0;
    end if;
  end function to_integer;

  -----------------------------------------------------------------------------

  function to_fpOp (slvOp : std_logic_vector(clog2(fpOp_t'pos(fpOp_t'high))-1 downto 0))
    return fpOp_t is
  begin  -- function to_fpOp
    if unsigned(slvOp) > fpOp_t'pos(fpOp_t'high) then
      return fpOp_t'low;
    else
      return fpOp_t'val(to_integer(unsigned(slvOp)));
    end if;

  end function to_fpOp;

  -----------------------------------------------------------------------------

  function to_rvRoundMode (slvRound : std_logic_vector(2 downto 0))
    return rvRoundingMode_t is
  begin  -- function to_rvRoundMode
    if unsigned(slvRound) > rvRoundingMode_t'pos(rvRoundingMode_t'high) then
      return rvRoundingMode_t'low;
    else
      return rvRoundingMode_t'val(to_integer(unsigned(slvRound)));
    end if;

  end function to_rvRoundMode;

  -----------------------------------------------------------------------------

  function to_rvStatus (slvStat : std_logic_vector(4 downto 0))
    return rvStatus_t is
    variable RVSTAT : rvStatus_t;
  begin  -- function to_rvStatus
    for i in rvStatusBit_t loop
      RVSTAT(i) := slvStat(rvStatusBit_t'pos(i));
    end loop;  -- i
    return RVSTAT;
  end function to_rvStatus;

  -----------------------------------------------------------------------------

  function to_opGroupLogic ( slv : std_logic_vector(0 to fpOpGroup_t'pos(fpOpGroup_t'high)))
    return opGroupLogic_t is
    variable res : opGroupLogic_t;
  begin  -- function to_opGroupLogic
    for i in 0 to fpOpGroup_t'pos(fpOpGroup_t'high) loop
      res(fpOpGroup_t'val(i)) := slv(i);
    end loop;
    return res;
  end function to_opGroupLogic;

  -----------------------------------------------------------------------------

  function to_sl (bool : boolean)
    return std_logic is
  begin  -- function to_sl
    if bool then
      return '1';
    else
      return '0';
    end if;
  end function to_sl;

  -----------------------------------------------------------------------------

  function to_slv (class : rvClassBit_t) return std_logic_vector is
  begin  -- function to_slv
    return std_logic_vector(to_unsigned(rvClassBit_t'pos(class), 4));
  end function to_slv;

  -----------------------------------------------------------------------------

  function to_slv_unpack (class : rvClassBit_t) return std_logic_vector is
  begin  -- function to_slv
    return std_logic_vector(unsigned'("0000000001") sll rvClassBit_t'pos(class));
  end function to_slv_unpack;

  -----------------------------------------------------------------------------

  function to_slv_unpack (class : std_logic_vector(3 downto 0))
    return std_logic_vector is
  begin  -- function to_slv
    return std_logic_vector(unsigned'("0000000001") sll to_integer(unsigned(class)));
  end function to_slv_unpack;

  -----------------------------------------------------------------------------

  function to_slv (rvRound : rvRoundingMode_t) return std_logic_vector is
  begin  -- function to_slv
    return std_logic_vector(to_unsigned(rvRoundingMode_t'pos(rvRound), 3));
  end function to_slv;

  -----------------------------------------------------------------------------

  function to_slv (rvStat : rvStatus_t)
    return std_logic_vector is
    variable slvstat : std_logic_vector(4 downto 0);
  begin  -- function to_slv
    for i in rvStatusBit_t loop
      slvstat(rvStatusBit_t'pos(i)) := rvStat(i);
    end loop;  -- i
    return slvstat;
  end function to_slv;

  -----------------------------------------------------------------------------

  function to_slv ( opgrpvec : opGroupLogic_t )
    return std_logic_vector is
    variable res : std_logic_vector(0 to fpOpGroup_t'pos(fpOpGroup_t'high));
  begin  -- function to_slv
    for i in res'range loop
      res(i) := opgrpvec(fpOpGroup_t'val(i));
    end loop;
    return res;
  end function to_slv;

  -----------------------------------------------------------------------------

  function to_slv (constant cb : vecClassBlock_t)
    return std_logic_vector is
    variable res : std_logic_vector(7 downto 0);
  begin  -- function to_slv
    return cb.Sign & not cb.Sign & std_logic_vector(unsigned'("000001") sll vecClassBit_t'pos(cb.Class));
  end function to_slv;

  -----------------------------------------------------------------------------

  function minimum (l : integer; r : integer) return integer is
  begin
    if r < l then
      return r;
    end if;
    return l;
  end function minimum;

  -----------------------------------------------------------------------------

  function maximum (l : integer; r : integer) return integer is
  begin
    if r > l then
      return r;
    end if;
    return l;
  end function maximum;

  -----------------------------------------------------------------------------

  function or_reduce (arg : std_logic_vector) return std_logic is
    variable res : std_logic;
  begin
    for i in arg'range loop
      if i = arg'left then
        res := arg(i);
      else
        res := arg(i) or res;
      end if;
      exit when res = '1';              -- faster sim
    end loop;
    return res;
  end function;

  -----------------------------------------------------------------------------

  function and_reduce (arg : std_logic_vector) return std_logic is
    variable res : std_logic;
  begin
    for i in arg'range loop
      if i = arg'left then
        res := arg(i);
      else
        res := arg(i) and res;
      end if;
      exit when res = '0';              -- faster sim
    end loop;
    return res;
  end function;

  -----------------------------------------------------------------------------

  function or_reduce (arg : unsigned) return std_logic is
  begin
    return or_reduce(std_logic_vector(arg));
  end function or_reduce;

  -----------------------------------------------------------------------------

  function and_reduce (arg : unsigned) return std_logic is
  begin
    return and_reduce(std_logic_vector(arg));
  end function and_reduce;

  -----------------------------------------------------------------------------

  function or_reduce (arg : signed) return std_logic is
  begin
    return or_reduce(std_logic_vector(arg));
  end function or_reduce;

  -----------------------------------------------------------------------------

  function and_reduce (arg : signed) return std_logic is
  begin
    return and_reduce(std_logic_vector(arg));
  end function and_reduce;

  -----------------------------------------------------------------------------

end package body fpnew_pkg;


