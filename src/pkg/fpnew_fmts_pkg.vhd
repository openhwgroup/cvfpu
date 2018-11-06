-------------------------------------------------------------------------------
-- Title      : Format Package for TransPrecision Floating-Point Unit
-- Project    :
-------------------------------------------------------------------------------
-- File       : fpnew_fmts_pkg.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-24
-- Last update: 2018-10-02
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: This package provides types and functions for predefined
--              floating-point and integer types for the use with the
--              TransPrecision floating-point unit.
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

--! @brief TransPrecision Floating-Point Unit Package
--!
--! This package provides types and functions for predefined floating-point and
--! integer types for the use with the TransPrecision floating-point unit.
package fpnew_fmts_pkg is

  --===========================================================================
  -- FP Format-Dependent Type Definitions
  --===========================================================================

  --! @brief Encoding of floating-point formats
  --! @details These options make up the set of pre-defined floating-point
  --! formats available in the unit. Additional enumerators are provided for
  --! additional custom floating-point formats.
  --! \sa DEFAULTENCODING
  --! | Enumerator | Format           | Width  | EXP_BITS | MAN_BITS
  --! |:----------:|------------------|-------:|:--------:|:--------:
  --! | FP32       | IEEE binary32    | 32 bit | 8        | 23
  --! | FP64       | IEEE binary64    | 64 bit | 11       | 52
  --! | FP16       | IEEE binary16    | 16 bit | 5        | 10
  --! | FP8        | binary8          |  8 bit | 5        | 2
  --! | FP16ALT    | binary16alt      | 16 bit | 8        | 7
  --! | CUST{1-3}  | *custom formats* | *any*  | 0\*      | 0\*
  --! \note \* Custom formats must be set-up in the implementation and
  --! configured to the components through the *FORMATS* generic port
  type fpFmt_t is (FP32, FP64, FP16, FP8, FP16ALT,
                   CUST1, CUST2, CUST3);

  --! @brief Array of naturals for each format
  --! @details Array of NATURAL that holds a value for each format from
  --! \ref fpFmt_t "FPFMT_T"
  type fmtNaturals_t is array (fpFmt_t) of natural;

  --! @brief Array of std_logic for each format
  --! @details Array of STD_LOGIC that holds a value for each format from
  --! \ref fpFmt_t "FPFMT_T"
  type fmtLogic_t is array (fpFmt_t) of std_logic;

  --! @brief Array of booleans for each format
  --! @details Array of BOOLEAN that holds a value for each format from
  --! \ref fpFmt_t "FPFMT_T"
  type fmtBooleans_t is array (fpFmt_t) of boolean;

  --! @brief Array of status flags for each format
  --! @details Array of RVSTATUS_T that holds a value for each format from
  --! \ref fpFmt_t "FPFMT_T"
  type fmtStatus_t is array (fpFmt_t) of rvStatus_t;

  --! @brief Array of classifications for each format
  --! @details Array of RVCLASSBIT_T that holds a value for each format from
  --! \ref fpFmt_t "FPFMT_T"
  type fmtClass_t is array (fpFmt_t) of rvClassBit_t;

  --! @brief Array of std_logic_vector values for each format
  --! @details Array of STD_LOGIC_VECTOR that hold a value for each \ref
  --! fpFmt_t "FPFMT_T"
  --! \note Type must be constrained upon declaration in a signal
  --! \warning VHDL-2008 needed for arrays of unconstrained types. Only
  --! uncomment when your tools support VHDL-2008
--  type fmtSlVectors_t is array (fpFmt_t) of std_logic_vector;
  type fmtSlArray2d_t is array (fpFmt_t range <>, natural range <>) of std_logic;

  --! @brief Encoding of a floating-point format
  --! @details Contains the number of exponent bits and number of mantissa bits
  --! that define a floating-point format
  type fpFmtEncoding_t is record
    ExpBits : natural;
    ManBits : natural;
  end record fpFmtEncoding_t;

  --! @brief Array of floating-point format encodings for each format
  --! @details Array of FPFMTENCODING_T that holds an encoding for each format from
  --! \ref fpFmt_t "FPFMT_T"
  type fmtEncodings_t is array (fpFmt_t) of fpFmtEncoding_t;

  --! @brief List of active formats and their encodings
  --! @details Contains the configuration of enabled formats as bitmasks as
  --! well as the format specific bitwidths to be used.
  type activeFormats_t is record
    Active   : fmtBooleans_t;
    Encoding : fmtEncodings_t;
  end record activeFormats_t;

  --! @brief Unit latency configuration
  --! @details Contains the configuraition of subunit latencies for all formats.
  type opGroupFmtNaturals_t is array (fpOpGroup_t) of fmtNaturals_t;

  --! @brief Subunit type
  --! @details Contains the possible ways of handling multiple formats in
  --! operation group-specific subunits.
  --! The types are:
  --! | Enumerator | Type     | Explanation
  --! |:----------:|----------|------------
  --! | NONE       | *n/a*    | Don't instantiate operational unit for this fp format
  --! | PARALLEL   | Parallel | Separate unit for this fp format
  --! | MERGED     | Merged   | This format is inside a shared unit with other formats
  type unitType_t is (NONE, PARALLEL, MERGED);

  --! @brief Array of subunit types for each format
  --! @details Array of UNITTYPE_T that holds a subunit type for each format
  --! from \ref fpFmt_t "FPFMT_T"
  type fmtUnitTypes_t is array (fpFmt_t) of unitType_t;

  --! @brief Array of subunit type arrays for each operation group
  --! @details Array of FMTUNITTYPES_T that holds a subunit type array for each
  --! operation group from \ref fpOpGroup_t "FPOPGROUP_T"
  type opGroupFmtUnitTypes_t is array (fpOpGroup_t) of fmtUnitTypes_t;


  --===========================================================================
  -- Integer Format-Dependent Type Definitions
  --===========================================================================

  --! @brief Encoding of integer formats
  --! @details These options make up the set of pre-defined integer formats
  --! available in the unit.
  --! | Enumerator | Alias | Format      | Width
  --! |:----------:|:-----:|-------------|-------:
  --! | INT8       |  B    | Byte        |  8 bit
  --! | INT16      |  H    | Half-Word   | 16 bit
  --! | INT32      |  W    | Word        | 32 bit
  --! | INT64      |  D    | Double-Word | 64 bit
  type intFmt_t is (INT8, INT16, INT32, INT64);
--  alias B is INT8[return intFmt_t];
--  alias H is INT16[return intFmt_t];
--  alias W is INT32[return intFmt_t];
--  alias D is INT64[return intFmt_t];

  --! @brief Array of naturals for each integer format
  --! @details Array of NATURAL that holds a value for each integer format from
  --! \ref intFmt_t "INTFMT_T"
  type intFmtNaturals_t is array (intFmt_t) of natural;

  --! @brief Array of booleans for each integer format
  --! @details Array of BOOLEAN that holds a value for each integer format from
  --! \ref intFmt_t "INTFMT_T"
  type intFmtBooleans_t is array (intFmt_t) of boolean;

  --! @brief List of active integer formats and their widths
  --! @details Contains the configuration of enabled integer formats as
  --! bitmasks as well as the format specific width to be used.
  type activeIntFormats_t is record
    Active : intFmtBooleans_t;
    Length : intFmtNaturals_t;
  end record activeIntFormats_t;

  --===========================================================================
  -- Predefined FP Format-Dependent Constants
  --===========================================================================

  --! @brief Predefined FP format encodings
  --! @details Holds the bit encodings of each predefined entry of fpFmt_t.
  constant DEFAULTENCODING : fmtEncodings_t := (FP32    => (8, 23),
                                                FP64    => (11, 52),
                                                FP16    => (5, 10),
                                                FP8     => (5, 2),
                                                FP16ALT => (8, 7),
                                                others  => (0, 0));

  --===========================================================================
  -- Predefined Integer Format-Dependent Constants
  --===========================================================================

  --! @brief Predefined integer format lengths
  --! @details Holds the lengths of each predefined entry of intFmt_t
  constant INTFMTLENGTHS : intFmtNaturals_t := (INT8 => 8, INT16 => 16,
                                                INT32 => 32, INT64 => 64);

  --===========================================================================
  -- FP Format-Dependent Helper Functions
  --===========================================================================

  --! @brief FP format width from encoding
  --! @returns The width of the format given in encoding FPFMTENCODING_T fmtbits
  --! @retval NATURAL
  function WIDTH (constant encoding : fpFmtEncoding_t)
    return natural;

  --! @brief FP format width from configuration
  --! @returns The width of the format given in FPFMT_T fmt under configuration
  --! ACTIVEFORMATS_T config
  --! @retval NATURAL
  function WIDTH (constant fmt : fpFmt_t; constant config : activeFormats_t)
    return natural;

  --! @brief Maximum encoded exponent value for FP format from encoding
  --! @returns The largest encoded exponent for the format encoded in
  --! FPFMTENCODING_T encoding, corresponding to &infin; or NaN
  --! @retval UNSIGNED(encoding.ExpBits-1 downto 0)
  function MAXEXP (constant encoding : fpFmtEncoding_t)
    return unsigned;

  --! @brief Maximum encoded exponent value for FP format from configuration
  --! @returns The largest encoded exponent for the format in FPFMT_T fmt under
  --! configuration ACTIVEFORMATS_T config, corresponding to &infin; or NaN
  --! @retval UNSIGNED(config.Encoding(fmt).ExpBits-1 downto 0)
  function MAXEXP (constant fmt : fpFmt_t; constant config : activeFormats_t)
    return unsigned;

  --! @brief Smallest FP format width of active formats from configuration
  --! @returns The width of the narrowest active format given in record
  --! ACTIVEFORMATS_T conf
  --! @retval NATURAL
  function MINWIDTH (constant conf : in activeFormats_t)
    return natural;

  --! @brief Largest FP format width of active formats from configuration
  --! @returns The width of the largest active format given in record
  --! ACTIVEFORMATS_T conf
  --! @retval NATURAL
  function MAXWIDTH (constant conf : in activeFormats_t)
    return natural;

  --! @brief Encoding of FP superformat for all active formats in configuration
  --! @returns The format encoding of the smallest format that can precisely
  --! hold all active formats in ACTIVEFORMATS_T config
  --! @retval fpFmtEncoding_t
  function SUPERFORMAT (constant config : in activeFormats_t)
    return fpFmtEncoding_t;

  --! @brief Bias for encoded FP format from encoding
  --! @returns The bias for the format encoded in FPFMTENCODING_T fmtenc
  --! @retval NATURAL
  function BIAS (constant fmtenc : fpFmtEncoding_t)
    return natural;

  --! @brief Bias for encoded FP format from configuration
  --! @returns The bias for the format encoded in FPFMT_T fmt under
  --! configuration ACTIVEFORMATS_T fmts
  --! @retval NATURAL
  function BIAS (constant fmt : fpFmt_t; constant config : activeFormats_t)
    return natural;

  --! @brief Canonical NaN bit-pattern for FP format from encoding
  --! @returns The canonical quiet NaN for the format encoded in
  --! FPFMTENCODING_T fmtenc
  --! @retval STD_LOGIC_VECTOR(fmtenc.ExpBits+fmtenc.ManBits downto 0)
  function NAN (constant fmtenc : fpFmtEncoding_t)
    return std_logic_vector;

  --! @brief Canonical NaN bit-pattern for FP format from configuration
  --! @returns The canonical quiet NaN for the format in FPFMT_T fmt under
  --! configuration ACTIVEFORMATS_T config
  --! @retval STD_LOGIC_VECTOR(fmtenc.ExpBits+fmtenc.ManBits downto 0)
  function NAN (constant fmt : fpFmt_t; constant config : activeFormats_t)
    return std_logic_vector;

  --! @brief Largest active format in configuration
  --! @returns The largest format marked active in ACTIVEFORMATS_T conf
  --! @retval FPFMT_T
  function largestActive (constant conf : activeFormats_t)
    return fpFmt_t;

  --! @brief Smallest active format in configuration
  --! @returns The smallest format marked active in ACTIVEFORMATS_T conf
  --! @retval FPFMT_T
  function smallestActive (constant conf : activeFormats_t)
    return fpFmt_t;

  --! @brief First active format in configuration
  --! @returns The first format marked active in ACTIVEFORMATS_T conf
  --! @retval FPFMT_T
  function findFirstActive (constant conf : activeFormats_t)
    return fpFmt_t;

  --! @brief Number of active formats in configuration
  --! @returns The number of formats marked active in ACTIVEFORMATS_T conf
  --! @retval NATURAL
  function numActive (constant conf : activeFormats_t)
    return natural;

  --! @brief Check for set format entry
  --! @returns TRUE when any bit in FMTLOGIC_T val is set, FALSE otherwise
  --! @retval BOOLEAN
  --! /note This function is only needed when no VHDL-2008 support is present,
  --! as FMTLOGIC_T is closely related to std_logic_vector in VHDL-2008 and
  --! can be directly cast and fed into or_reduce()
  function anySet (constant val : fmtLogic_t)
    return boolean;

  --! @brief Check for set format entry
  --! @returns TRUE when any bit in FMTBOOLEANS_t val is set, FALSE otherwise
  --! @retval BOOLEAN
  --! /note This function is only needed when no VHDL-2008 support is present,
  --! as FMTLOGIC_T is closely related to std_logic_vector in VHDL-2008 and
  --! can be directly cast and fed into or_reduce()
  function anySet (constant val : fmtBooleans_t)
    return boolean;

  --! @brief First set format entry
  --! @returns The first format that is set in FMTLOGIC_T val
  --! @retval FMTLOGIC_T
  function findFirstSet (constant val : fmtLogic_t)
    return fpFmt_t;

  --! @brief Largest active latency
  --! @returns The largest active format-dependent latency from latency and
  --! format config
  function largestActiveLatency (constant lat  : fmtNaturals_t;
                                 constant conf : activeFormats_t)
    return natural;

  --! @brief Largest active latency
  --! @returns The largest active group- and format-dependent latency from
  --! latency and format config
  function largestActiveLatency (constant lat  : opGroupFmtNaturals_t;
                                 constant grp  : fpOpGroup_t;
                                 constant conf : activeFormats_t)
    return natural;

  --! @brief Check for active merged unit
  --! @returns TRUE when an active format wants to live in a merged unit
  function anyMergedFormat (constant types : fmtUnitTypes_t;
                            constant conf  : activeFormats_t)
    return boolean;

  --! @brief Get first active merged format
  --! @returns The first format that wants to live in a merged unit
  function firstMergedFormat (constant types : fmtUnitTypes_t;
                              constant conf  : activeFormats_t)
    return fpFmt_t;

  --! @brief Get active formats configuration for merged formats
  function getMergedFormats (constant types : fmtUnitTypes_t;
                             constant conf  : activeFormats_t)
    return activeFormats_t;

  --! @brief Active formats for a multiformat unit lane
  --! @returns The configuration of formats that are active in lane number
  --! NATURAL lane_no
  --! @retval ACTIVEFORMATS_T
  function getMultiLaneFormats (constant conf        : activeFormats_t;
                                constant slice_width : natural;
                                constant lane_no     : natural)
    return activeFormats_t;

  --! @brief Active formats for a multiformat conversion unit lane
  --! @returns The configuration of formats that are active in lane number
  --! NATURAL lane_no
  --! @retval ACTIVEFORMATS_T
  --! /note Conversions have other requirements as cast-and-pack operations
  --! incur irregular lane layouts
  function getMultiLaneFormats (constant conf        : activeFormats_t;
                                constant slice_width : natural;
                                constant lane_no     : natural;
                                constant cpk_fmts    : fmtBooleans_t)
    return activeFormats_t;

  --! @brief Set array row at format index fmt to std_logic_vector signal slv
  procedure set_row (signal arr   : out fmtSlArray2d_t; constant fmt : in fpFmt_t;
                     variable slv : in  std_logic_vector);


  --! @brief Binds valid rows in format-indexed 2d array to 2d logic array
  procedure extract_active_rows (signal arr_out : out slArray2d_t;
                                 signal arr_in  : in  fmtSlArray2d_t;
                                 constant conf  : in  activeFormats_t);

  --! @brief Binds valid statuses in format-indexed array to status array
  procedure extract_active_statuses (signal arr_out : out statusArray_t;
                                     signal arr_in  : in  fmtStatus_t;
                                     constant conf  : in  activeFormats_t);

  --! @brief Binds valid logic in format-indexed array to logic vector
  procedure extract_active_logic (signal slv    : out std_logic_vector;
                                  signal arr_in : in  fmtLogic_t;
                                  constant conf : in  activeFormats_t);

  --! @brief Binds valid logic in logic vector to format-indexed array entries
  procedure inject_active_logic (signal arr_out : out fmtLogic_t;
                                 signal slv     : in  std_logic_vector;
                                 constant conf  : in  activeFormats_t);



  --===========================================================================
  -- Integer Format-Dependent Helper Functions
  --===========================================================================

  --! @brief Largest integer format length of active formats
  --! @returns The lenght of the largest active integer format given in record
  --! ACTIVEINTFORMATS_T conf
  --! @retval NATURAL
  function MAXWIDTH (constant conf : in activeIntFormats_t)
    return natural;

  --! @brief First active integer format in configuration
  --! @returns The first integer format marked active in ACTIVEINTFORMATS_T
  --! conf
  --! @retval INTFMT_T
  function findFirstActive (constant conf : activeIntFormats_t)
    return intFmt_t;

  --! @brief Check for set format entry
  --! @returns TRUE when any bit in INTFMTLOGIC_T val is set, FALSE otherwise
  --! @retval BOOLEAN
  --! /note This function is only needed when no VHDL-2008 support is present,
  --! as FMTLOGIC_T is closely related to std_logic_vector in VHDL-2008 and
  --! can be directly cast and fed into or_reduce()
  function anySet (constant val : intFmtBooleans_t)
    return boolean;

  --! @brief Active integer formats for a multiformat unit lane
  --! @returns The configuration of integer formats that are active in lane
  --! number NATURAL lane_no
  --! @retval ACTIVEINTFORMATS_T
  function getMultiLaneFormats (constant conf        : activeIntFormats_t;
                                constant slice_width : natural;
                                constant lane_no     : natural)
    return activeIntFormats_t;

  --===========================================================================
  -- Float & Integer Format-Dependent Helper Functions
  --===========================================================================

  --! @brief Width encompassing both active int and float formats from
  --! configuration
  --! @returns The maximum width of the largest active formats given in records
  --! ACTIVEFORMATS_T fpconf and ACTIVEINTFORMATS_T intconf
  --! @retval NATURAL
  function MAXWIDTH (constant fpconf : activeFormats_t; constant intconf : activeIntFormats_t)
    return natural;


  --===========================================================================
  -- Conversion Functions
  --===========================================================================

  --! @brief fpFmt_t from std_logic_vector
  --! @details Converts the floating-point format encoding in STD_LOGIC_VECTOR
  --! slvFmt to the internal enumerated type \ref fpFmt_t "FPFMT_T"
  --! @returns The floating-point format as an \ref fpFmt_t "FPFMT_T"
  --! @retval FPFMT_T
  function to_fpFmt (slvFmt : std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0))
    return fpFmt_t;

  --! @brief intFmt_t from std_logic_vector
  --! @details Converts the integer format encoding in STD_LOGIC_VECTOR
  --! slvFmt to the internal enumerated type \ref intFmt_t "INTFMT_T"
  --! @returns The floating-point format as an \ref intFmt_t "INTFMT_T"
  --! @retval FPFMT_T
  function to_intFmt (slvFmt : std_logic_vector(clog2(intFmt_t'pos(intFmt_t'high))-1 downto 0))
    return intFmt_t;

  --! @brief std_logic_vector from fpFmt_t
  --! @returns The floating-point format as a STD_LOGIC_VECTOR
  --! @retval STD_LOGIC_VECTOR(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0)
  function to_slv (fmt : fpFmt_t)
    return std_logic_vector;

  --! @brief std_logic_vector from unitType_t
  --! @returns The operation group in UNITTYPE_T as a STD_LOGIC_VECTOR
  --! @retval STD_LOGIC_VECTOR(clog2(unitType_t'pos(unitType_t'high))-1 downto 0)
  function to_slv (utype : unitType_t)
    return std_logic_vector;

  --! @brief std_logic_vector from intFmt_t
  --! @returns The integer format as a STD_LOGIC_VECTOR
  --! @retval STD_LOGIC_VECTOR(clog2(intFmt_t'pos(intFmt_t'high))-1 downto 0))
  function to_slv (ifmt : intFmt_t)
    return std_logic_vector;


end package fpnew_fmts_pkg;


--=============================================================================
--=======================    Package    Body    ===============================
--=============================================================================

package body fpnew_fmts_pkg is

--=========================Exported Functions==================================

  function WIDTH (constant encoding : fpFmtEncoding_t)
    return natural is
  begin  -- function FMTWIDTH
    return encoding.ExpBits + encoding.ManBits + 1;
  end function WIDTH;

  -----------------------------------------------------------------------------

  function WIDTH (constant fmt : fpFmt_t; constant config : activeFormats_t)
    return natural is
  begin  -- function FMTWIDTH
    return WIDTH(config.Encoding(fmt));
  end function WIDTH;

  -----------------------------------------------------------------------------

  function MAXEXP (constant encoding : fpFmtEncoding_t)
    return unsigned is
  begin  -- function MAXEXP
    return MAXEXP(encoding.ExpBits);
  end function MAXEXP;

  -----------------------------------------------------------------------------

  function MAXEXP (constant fmt : fpFmt_t; constant config : activeFormats_t)
    return unsigned is
  begin  -- function MAXEXP
    return MAXEXP(config.Encoding(fmt));
  end function MAXEXP;

  -----------------------------------------------------------------------------

  function MINWIDTH (constant conf : activeFormats_t)
    return natural is
    variable res : natural := integer'high;
  begin  -- function MINWIDTH
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        res := minimum(res, WIDTH(conf.Encoding(fmt)));
      end if;
    end loop;  -- fmt
    return res;
  end function MINWIDTH;

  -----------------------------------------------------------------------------

  function MAXWIDTH (constant conf : activeFormats_t)
    return natural is
    variable res : natural := 0;
  begin  -- function MAXWIDTH
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        res := maximum(res, WIDTH(conf.Encoding(fmt)));
      end if;
    end loop;  -- fmt
    return res;
  end function MAXWIDTH;

  -----------------------------------------------------------------------------

  function SUPERFORMAT (constant config : in activeFormats_t)
    return fpFmtEncoding_t is
    variable res : fpFmtEncoding_t := (others => 0);
  begin  -- function SUPERFORMAT
    for fmt in fpFmt_t loop
      if config.Active(fmt) then
        res.ExpBits := maximum(res.ExpBits, config.Encoding(fmt).ExpBits);
        res.ManBits := maximum(res.ManBits, config.Encoding(fmt).ManBits);
      end if;
    end loop;  -- fmt
    return res;
  end function SUPERFORMAT;

  -----------------------------------------------------------------------------

  function BIAS (constant fmtenc : fpFmtEncoding_t)
    return natural is
  begin  -- function BIAS
    return BIAS(fmtenc.ExpBits);
  end function BIAS;

  -----------------------------------------------------------------------------

  function BIAS (constant fmt : fpFmt_t; constant config : activeFormats_t)
    return natural is
  begin  -- function BIAS
    return BIAS(config.Encoding(fmt));
  end function BIAS;

  -----------------------------------------------------------------------------

  function NAN (constant fmtenc : fpFmtEncoding_t)
    return std_logic_vector is
  begin  -- function NAN
    return NAN(fmtenc.ExpBits, fmtenc.ManBits);
  end function NAN;

  -----------------------------------------------------------------------------

  function NAN (constant fmt : fpFmt_t; constant config : activeFormats_t)
    return std_logic_vector is
  begin  -- function NAN
    return NAN(config.Encoding(fmt));
  end function NAN;

  -----------------------------------------------------------------------------

  function largestActive (constant conf : activeFormats_t)
    return fpFmt_t is
  begin
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        if WIDTH(conf.Encoding(fmt)) = MAXWIDTH(conf) then
          return fmt;
        end if;
      end if;
    end loop;  -- fmt
    return fpFmt_t'right;               -- TODO error on no active
  end function largestActive;

  -----------------------------------------------------------------------------

  function smallestActive (constant conf : activeFormats_t)
    return fpFmt_t is
  begin
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        if WIDTH(conf.Encoding(fmt)) = MINWIDTH(conf) then
          return fmt;
        end if;
      end if;
    end loop;  -- fmt
    return fpFmt_t'right;               -- TODO error on no active
  end function smallestActive;

  -----------------------------------------------------------------------------

  function findFirstActive (constant conf : activeFormats_t)
    return fpFmt_t is
  begin
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        return fmt;
      end if;
    end loop;  -- fmt
    return fpFmt_t'left;                -- TODO error on no active
  end function findFirstActive;

  -----------------------------------------------------------------------------

  function numActive (constant conf : activeFormats_t)
    return natural is
    variable res : natural := 0;
  begin
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        res := res+1;
      end if;
    end loop;  -- fmt
    return res;                         -- TODO error on no active
  end function numActive;

  -----------------------------------------------------------------------------

  function anySet (constant val : fmtLogic_t)
    return boolean is
  begin  -- function anyValid
    for fmt in fpFmt_t loop
      if val(fmt) = '1' then
        return true;
      end if;
    end loop;  -- fmt
    return false;                       -- default return
  end function anySet;

  -----------------------------------------------------------------------------

  function anySet (constant val : fmtBooleans_t)
    return boolean is
  begin  -- function anyValid
    for fmt in fpFmt_t loop
      if val(fmt) then
        return true;
      end if;
    end loop;  -- fmt
    return false;                       -- default return
  end function anySet;

  -----------------------------------------------------------------------------

  function findFirstSet (constant val : fmtLogic_t)
    return fpFmt_t is
  begin  -- function findFirstSet
    for fmt in fpFmt_t loop
      if val(fmt) = '1' then
        return fmt;
      end if;
    end loop;  -- fmt
    return fpFmt_t'left;                -- default return
  end function findFirstSet;

  -----------------------------------------------------------------------------

  function largestActiveLatency (constant lat  : fmtNaturals_t;
                                 constant conf : activeFormats_t)
    return natural is
    variable res : natural := 0;
  begin  -- function largestActiveLatency
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        res := maximum(res, lat(fmt));
      end if;
    end loop;  -- fmt
    return res;
  end function largestActiveLatency;

  -----------------------------------------------------------------------------

  function largestActiveLatency (constant lat  : opGroupFmtNaturals_t;
                                 constant grp  : fpOpGroup_t;
                                 constant conf : activeFormats_t)
    return natural is
    variable res : natural := 0;
  begin  -- function largestActiveLatency
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        res := maximum(res, lat(grp)(fmt));
      end if;
    end loop;  -- fmt
    return res;
  end function largestActiveLatency;

  -----------------------------------------------------------------------------

  function anyMergedFormat (constant types : fmtUnitTypes_t;
                            constant conf  : activeFormats_t)
    return boolean is
  begin
    for fmt in fpFmt_t loop
      if conf.Active(fmt) and types(fmt) = MERGED then
        return true;
      end if;
    end loop;
    return false;
  end function anyMergedFormat;

  -----------------------------------------------------------------------------

  function firstMergedFormat (constant types : fmtUnitTypes_t;
                              constant conf  : activeFormats_t)
    return fpFmt_t is
  begin
    for fmt in fpFmt_t loop
      if conf.Active(fmt) and types(fmt) = MERGED then
        return fmt;
      end if;
    end loop;
    return fpFmt_t'right;
  end function firstMergedFormat;

  -----------------------------------------------------------------------------

  --! @brief Get active formats configuration for merged formats
  function getMergedFormats (constant types : fmtUnitTypes_t;
                             constant conf  : activeFormats_t)
    return activeFormats_t is
    variable res : activeFormats_t;
  begin
    for fmt in fpFmt_t loop
      res.Active(fmt) := conf.Active(fmt) and types(fmt) = MERGED;
    end loop;  -- fmt
    res.Encoding := conf.Encoding;
    return res;
  end function getMergedFormats;

  -----------------------------------------------------------------------------

  function getMultiLaneFormats (constant conf        : activeFormats_t;
                                constant slice_width : natural;
                                constant lane_no     : natural)
    return activeFormats_t is
    variable res : activeFormats_t;
  begin
    for fmt in fpFmt_t loop
      res.Active(fmt) := conf.Active(fmt)
                         and slice_width/WIDTH(conf.Encoding(fmt)) > lane_no;
    end loop;  -- fmt
    res.Encoding := conf.Encoding;
    return res;
  end function getMultiLaneFormats;

  -----------------------------------------------------------------------------

  function getMultiLaneFormats (constant conf        : activeFormats_t;
                                constant slice_width : natural;
                                constant lane_no     : natural;
                                constant cpk_fmts    : fmtBooleans_t)
    return activeFormats_t is
    variable res : activeFormats_t;
  begin
    for fmt in fpFmt_t loop
      -- Put in CPK formats at least twice
      res.Active(fmt) := conf.Active(fmt)
                         and (slice_width/WIDTH(conf.Encoding(fmt)) > lane_no
                              or (cpk_fmts(fmt) and lane_no < 2));
    end loop;  -- fmt
    res.Encoding := conf.Encoding;
    return res;
  end function getMultiLaneFormats;

  -----------------------------------------------------------------------------

  procedure set_row (signal arr   : out fmtSlArray2d_t; constant fmt : in fpFmt_t;
                     variable slv : in  std_logic_vector) is
  begin  -- procedure set_row
    for i in slv'range loop
      arr(fmt, i) <= slv(i);
    end loop;  -- i
  end procedure set_row;

  -----------------------------------------------------------------------------

  procedure extract_active_rows (signal arr_out : out slArray2d_t;
                                 signal arr_in  : in  fmtSlArray2d_t;
                                 constant conf  : in  activeFormats_t) is
    variable idx : natural := 0;
  begin  -- procedure extract_active_rows
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        for i in arr_out'range(2) loop
          arr_out(idx, i) <= arr_in(fmt, i);
        end loop;  -- i
        idx := idx+1;
      end if;
    end loop;  -- fmt
  end procedure extract_active_rows;

  -----------------------------------------------------------------------------

  procedure extract_active_statuses (signal arr_out : out statusArray_t;
                                     signal arr_in  : in  fmtStatus_t;
                                     constant conf  : in  activeFormats_t) is
    variable idx : natural := 0;
  begin  -- procedure extract_active_statuses
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        arr_out(idx) <= arr_in(fmt);
        idx          := idx+1;
      end if;
    end loop;  -- fmt
  end procedure extract_active_statuses;

  -----------------------------------------------------------------------------

  procedure extract_active_logic (signal slv    : out std_logic_vector;
                                  signal arr_in : in  fmtLogic_t;
                                  constant conf : in  activeFormats_t) is
    variable idx : natural := 0;
  begin  -- procedure extract_active_statuses
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        slv(idx) <= arr_in(fmt);
        idx      := idx+1;
      end if;
    end loop;  -- fmt
  end procedure extract_active_logic;

  -----------------------------------------------------------------------------

  procedure inject_active_logic (signal arr_out : out fmtLogic_t;
                                 signal slv     : in  std_logic_vector;
                                 constant conf  : in  activeFormats_t) is
    variable idx : natural := 0;
  begin  -- procedure inject_active_logic
    for fmt in fpFmt_t loop
      if conf.Active(fmt) then
        arr_out(fmt) <= slv(idx);
        idx          := idx+1;
      end if;
    end loop;  -- fmt
  end procedure inject_active_logic;

  -----------------------------------------------------------------------------

  function MAXWIDTH (constant conf : activeIntFormats_t)
    return natural is
    variable res : natural := 0;
  begin  -- function MAXWIDTH
    for fmt in intFmt_t loop
      if conf.Active(fmt) then
        res := maximum(res, conf.Length(fmt));
      end if;
    end loop;  -- fmt
    return res;
  end function MAXWIDTH;

  -----------------------------------------------------------------------------

  function findFirstActive (constant conf : activeIntFormats_t)
    return intFmt_t is
  begin
    for fmt in intFmt_t loop
      if conf.Active(fmt) then
        return fmt;
      end if;
    end loop;  -- fmt
    return intFmt_t'left;               -- TODO error on no active
  end function findFirstActive;

  -----------------------------------------------------------------------------

  function anySet (constant val : intFmtBooleans_t)
    return boolean is
  begin  -- function anyValid
    for ifmt in intFmt_t loop
      if val(ifmt) then
        return true;
      end if;
    end loop;  -- ifmt
    return false;                       -- default return
  end function anySet;

  -----------------------------------------------------------------------------

  function getMultiLaneFormats (constant conf        : activeIntFormats_t;
                                constant slice_width : natural;
                                constant lane_no     : natural)
    return activeIntFormats_t is
    variable res : activeIntFormats_t;
  begin
    for ifmt in intFmt_t loop
      res.Active(ifmt) := conf.Active(ifmt)
                          and slice_width/conf.Length(ifmt) > lane_no;
    end loop;  -- ifmt
    res.Length := conf.Length;
    return res;
  end function getMultiLaneFormats;

  -----------------------------------------------------------------------------

  function MAXWIDTH (constant fpconf : activeFormats_t; constant intconf : activeIntFormats_t)
    return natural is
  begin  -- function MAXWIDTH
    return maximum(MAXWIDTH(fpconf), MAXWIDTH(intconf));
  end function MAXWIDTH;

  -----------------------------------------------------------------------------

  function to_fpFmt (slvFmt : std_logic_vector(clog2(fpFmt_t'pos(fpFmt_t'high))-1 downto 0))
    return fpFmt_t is
  begin  -- function to_fpFmt
    if unsigned(slvFmt) > fpFmt_t'pos(fpFmt_t'high) then
      return fpFmt_t'low;
    else
      return fpFmt_t'val(to_integer(unsigned(slvFmt)));
    end if;

  end function to_fpFmt;

  -----------------------------------------------------------------------------

  function to_intFmt (slvFmt : std_logic_vector(clog2(intFmt_t'pos(intFmt_t'high))-1 downto 0))
    return intFmt_t is
  begin  -- function to_intFmt
    if unsigned(slvFmt) > intFmt_t'pos(intFmt_t'high) then
      return intFmt_t'low;
    else
      return intFmt_t'val(to_integer(unsigned(slvFmt)));
    end if;

  end function to_intFmt;

  -----------------------------------------------------------------------------

  function to_slv (fmt : fpFmt_t)
    return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned(fpFmt_t'pos(fmt), clog2(fpFmt_t'pos(fpFmt_t'high))));
  end function to_slv;

  -----------------------------------------------------------------------------

  function to_slv (utype : unitType_t) return std_logic_vector is
  begin  -- function to_slv
    return std_logic_vector(to_unsigned(unitType_t'pos(utype), clog2(unitType_t'pos(unitType_t'high))));
  end function to_slv;

  -----------------------------------------------------------------------------

  function to_slv (ifmt : intFmt_t)
    return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned(intFmt_t'pos(ifmt), clog2(intFmt_t'pos(intFmt_t'high))));
  end function to_slv;

  -----------------------------------------------------------------------------

end package body fpnew_fmts_pkg;
