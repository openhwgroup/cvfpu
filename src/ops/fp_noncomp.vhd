-------------------------------------------------------------------------------
-- Title      : Floating-Point Non-Computational Operations Unit
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_noncomp.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-02-14
-- Last update: 2018-04-18
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Parametric floating-point comparison unit.
--              Supported operations from fpnew_pkg.fpOp_t:
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


entity fp_noncomp is

  generic (
    EXP_BITS  : natural := 5;
    MAN_BITS  : natural := 10;
    LATENCY   : natural := 0;
    TAG_WIDTH : natural := 0);

  port (
    Clk_CI           : in  std_logic;
    Reset_RBI        : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI, B_DI       : in  std_logic_vector(EXP_BITS+MAN_BITS downto 0);
    ABox_SI, BBox_SI : in  std_logic;
    RoundMode_SI     : in  rvRoundingMode_t;
    Op_SI            : in  fpOp_t;
    OpMod_SI         : in  std_logic;
    VectorialOp_SI   : in  std_logic;
    Tag_DI           : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI       : in  std_logic;
    InReady_SO       : out std_logic;
    Flush_SI                  : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO             : out std_logic_vector(EXP_BITS+MAN_BITS downto 0);
    Status_DO        : out rvStatus_t;
    Tag_DO           : out std_logic_vector(TAG_WIDTH-1 downto 0);
    UnpackClass_SO   : out std_logic;
    Zext_SO          : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO      : out std_logic;
    OutReady_SI      : in  std_logic);

end entity fp_noncomp;



architecture rtl of fp_noncomp is

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------
  constant MAXEXP : unsigned(EXP_BITS-1 downto 0) := MAXEXP(EXP_BITS);

  -- The quiet bit index is the topmost bit of the mantissa of a NaN value
  constant QUIETBIT : natural := MAN_BITS-1;

  -- Bit-Patterns of special values, only read from these!
  signal INFEXP  : std_logic_vector(EXP_BITS-1 downto 0);
  signal INFMANT : std_logic_vector(MAN_BITS-1 downto 0);

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

  --! @brief Non-computational operation groups
  --! @details Enumerators for indexing arrays that hold values depending on
  --! the operation group currently executed.
  type nonCompOpGroup_t is (SGNJ, MINMAX, CMP, CLASS);

  --! @brief Array of output words indexed by operation
  --! @details An output word for each operation in this unit. Addressable by
  --! enumerators from nonCompOpGroup_t corresponding to the operation classes.
  type resultArray_t is array (nonCompOpGroup_t) of std_logic_vector(Z_DO'range);
  --! @brief Array of status flags indexed by operation
  --! @details An fflags word for each operation in this unit. Addressable by
  --! enumerators from nonCompOpGroup_t corresponding to the operation classes.
  type statusArray_t is array (nonCompOpGroup_t) of rvStatus_t;
  --! @brief Array of std_logic indexed by operation
  --! @details A bit for each operation in this unit. Addressable by
  --! enumerators from nonCompOpGroup_t corresponding to the operation classes.
  type logicArray_t is array (nonCompOpGroup_t) of std_logic;

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Provide aliased signal names for parts of input FP numbers
  alias SignA_DI : std_logic is A_DI(A_DI'high);
  alias SignB_DI : std_logic is B_DI(B_DI'high);

  alias AbsA_DI : std_logic_vector(EXP_BITS+MAN_BITS-1 downto 0)
    is A_DI(EXP_BITS+MAN_BITS-1 downto 0);
  alias AbsB_DI : std_logic_vector(EXP_BITS+MAN_BITS-1 downto 0)
    is B_DI(EXP_BITS+MAN_BITS-1 downto 0);

  alias ExpA_DI : std_logic_vector(EXP_BITS-1 downto 0)
    is A_DI(EXP_BITS+MAN_BITS-1 downto MAN_BITS);
  alias ExpB_DI : std_logic_vector(EXP_BITS-1 downto 0)
    is B_DI(EXP_BITS+MAN_BITS-1 downto MAN_BITS);

  alias MantA_DI : std_logic_vector(MAN_BITS-1 downto 0)
    is A_DI(MAN_BITS-1 downto 0);
  alias MantB_DI : std_logic_vector(MAN_BITS-1 downto 0)
    is B_DI(MAN_BITS-1 downto 0);

  -- FP classification signals
  signal IsNormalA_S, IsNormalB_S : boolean;
  signal IsInfA_S, IsInfB_S       : boolean;
  signal IsNaNA_S, IsNaNB_S       : boolean;
  signal IsZeroA_S, IsZeroB_S     : boolean;

  signal SignalingNaN_S, SignalingNaNA_S : boolean;
  signal InputInf_S, InputNaN_S          : boolean;

  -- Classification
  signal ClassResult_D                   : rvClassBit_t;
  signal VecClassBlock_D                 : vecClassBlock_t;
  signal ScalarClassRes_D, VecClassRes_D : std_logic_vector(Z_DO'range);


  -- Comparator Outputs
  signal OperandsEqual_S, OperandASmaller_S : boolean;

  -- Operation Select
  signal OpGroup_S : nonCompOpGroup_t;

  -- Results of operations
  signal ResArray_D  : resultArray_t;
  signal ZextArray_S : logicArray_t;

  -- RISC-V FP FLAGS
  signal StatArray_D : statusArray_t;

  -- Final result (pre-pipelining)
  signal Result_D : std_logic_vector(Z_DO'range);
  signal Status_D : rvStatus_t;

  -- Control information about the output
  signal UnpackClass_S           : std_logic;
  signal Zext_S                  : std_logic;
  signal TagInt_D, TagIntPiped_D : std_logic_vector(TAG_WIDTH+1 downto 0);

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Special Value Constant Signals
  -----------------------------------------------------------------------------

  -- Infinity and NaN constants
  INFEXP  <= (others => '1');
  INFMANT <= (others => '0');

  -----------------------------------------------------------------------------
  -- Input Classification
  -----------------------------------------------------------------------------

  -- Normal if non-zero exponents
  IsNormalA_S <= unsigned(ExpA_DI) /= 0;
  IsNormalB_S <= unsigned(ExpB_DI) /= 0;

  -- Infinities have all-ones exponents and zero mantissa
  -- Improperly boxed operands are treated as canonical NaNs
  IsInfA_S <= ExpA_DI = INFEXP and MantA_DI = INFMANT;
  IsInfB_S <= ExpB_DI = INFEXP and MantB_DI = INFMANT;

  -- Nans have all-ones exponents and non-zero mantissa
  IsNaNA_S <= (unsigned(ExpA_DI) = MAXEXP and unsigned(MantA_DI) /= 0) or ABox_SI = '0';
  IsNaNB_S <= (unsigned(ExpB_DI) = MAXEXP and unsigned(MantB_DI) /= 0) or BBox_SI = '0';

  -- Zeroes are encoded by all-zero eponent and mantissa
  IsZeroA_S <= unsigned(std_logic_vector'(ExpA_DI & MantA_DI)) = 0;
  IsZeroB_S <= unsigned(std_logic_vector'(ExpB_DI & MantB_DI)) = 0;

  -- An input is Inf
  InputInf_S <= IsInfA_S or IsInfB_S;

  -- An input is NaN
  InputNaN_S <= IsNaNA_S or IsNaNB_S;

  -- Detect a signaling NaN at the inputs
  -- Improperly boxed operands are treated as canonical NaNs
  SignalingNaNA_S <= (IsNaNA_S and ABox_SI = '1' and MantA_DI(QUIETBIT) = '0');
  SignalingNaN_S  <= SignalingNaNA_S or (IsNaNB_S and BBox_SI = '1' and MantB_DI(QUIETBIT) = '0');

  -- Assign current operation group for later output selection
  with Op_SI select
    OpGroup_S <=
    CLASS  when CLASS,
    CMP    when CMP,
    MINMAX when MINMAX,
    SGNJ   when SGNJ,
    SGNJ   when others;

  -----------------------------------------------------------------------------
  -- Classification
  -----------------------------------------------------------------------------

  --! @brief Classification of input A
  --! @details Generate the RISC-V classification block as well as the
  --! vectorial classification block from input operand A
  p_class : process (all) is
  begin  -- process p_class

    -- Sign into Vectorial Class Block
    VecClassBlock_D.Sign <= SignA_DI;

    -- NaN cases
    if IsNaNA_S then
      if SignalingNaNA_S then
        ClassResult_D         <= SNAN;
        VecClassBlock_D.Class <= SNAN;
      else
        ClassResult_D         <= QNAN;
        VecClassBlock_D.Class <= QNAN;
      end if;

    -- negative cases
    elsif SignA_DI = '1' then
      if IsInfA_S then
        ClassResult_D         <= NEGINF;
        VecClassBlock_D.Class <= INF;
      elsif IsZeroA_S then
        ClassResult_D         <= NEGZERO;
        VecClassBlock_D.Class <= ZERO;
      elsif IsNormalA_S then
        ClassResult_D         <= NEGNORM;
        VecClassBlock_D.Class <= NORM;
      else
        ClassResult_D         <= NEGSUBNORM;
        VecClassBlock_D.Class <= SUBNORM;
      end if;

    -- positive cases
    else
      if IsInfA_S then
        ClassResult_D         <= POSINF;
        VecClassBlock_D.Class <= INF;
      elsif IsZeroA_S then
        ClassResult_D         <= POSZERO;
        VecClassBlock_D.Class <= ZERO;
      elsif IsNormalA_S then
        ClassResult_D         <= POSNORM;
        VecClassBlock_D.Class <= NORM;
      else
        ClassResult_D         <= POSSUBNORM;
        VecClassBlock_D.Class <= SUBNORM;
      end if;

    end if;

    ScalarClassRes_D             <= (others => '0');
    ScalarClassRes_D(3 downto 0) <= to_slv(ClassResult_D);  -- packed slv

    VecClassRes_D             <= (others => '0');
    VecClassRes_D(7 downto 0) <= to_slv(VecClassBlock_D);

  end process p_class;

  -- Scalar class result is sent out packed into the enumerated representation,
  -- Vectorial class result is unpacked right here
  ResArray_D(CLASS) <= VecClassRes_D when VectorialOp_SI = '1' else
                       ScalarClassRes_D;

  -- Classification never raises exceptions
  StatArray_D(CLASS) <= (others => '0');

  -- Output is integer reg, zero extend
  ZextArray_S(CLASS) <= '1';


  -----------------------------------------------------------------------------
  -- Sign Injection - operation is encoded in RoundMode_SI:
  -- RNE = SGNJ, RTZ = SGNJN, RDN = SGNJX, RUP = Passthrough (no NaN-box check)
  -- OpMod_SI enables sign-extension of result (for storing to integer regfile)
  -----------------------------------------------------------------------------

  p_signInjections : process (all) is

    variable SgnjResult_D : std_logic_vector(A_DI'range);
    variable SignA_D, SignB_D : std_logic;

  begin

    -- Assign A or the canonical NaN to the Reuslt first
    if RoundMode_SI = RUP then
      SgnjResult_D := A_DI;
    elsif ABox_SI = '0' then
      SgnjResult_D := NAN(EXP_BITS, MAN_BITS);
    else
      SgnjResult_D := A_DI;
    end if;

    -- In case of improper boxing on input operands, they get can. NaN sign (0)
    SignA_D := SignA_DI and ABox_SI;
    SignB_D := SignB_DI and BBox_SI;

    -- Do the actual sign injection
    if RoundMode_SI = RNE then
      SgnjResult_D(SgnjResult_D'high) := SignB_D;
    elsif RoundMode_SI = RTZ then
      SgnjResult_D(SgnjResult_D'high) := not SignB_D;
    elsif RoundMode_SI = RDN then
      SgnjResult_D(SgnjResult_D'high) := SignA_D xor SignB_D;
    end if;

    ResArray_D(SGNJ) <= SgnjResult_D;

  end process p_signInjections;

  -- Sign Injection never raises exceptions
  StatArray_D(SGNJ) <= (others => '0');

  -- Sign extend if selected by OpMod_SI
  ZextArray_S(SGNJ) <= OpMod_SI and (not ResArray_D(SGNJ)(Z_DO'high));

  -----------------------------------------------------------------------------
  -- Comparators
  -----------------------------------------------------------------------------

  -- All other ops need the comparator outputs for their result
  OperandsEqual_S   <= (signed(A_DI) = signed(B_DI)) or (IsZeroA_S and IsZeroB_S);
  OperandASmaller_S <= (signed(A_DI) < signed(B_DI)) xor (SignA_DI and SignB_DI) = '1';

  -----------------------------------------------------------------------------
  -- Minimum/Maximum - operation is encoded in RoundMode_SI:
  -- RNE = MIN, RTZ = MAX
  -----------------------------------------------------------------------------

  --! @brief Handle MIN/MAX operations and their special cases
  p_minMax : process (all) is
  begin  -- process p_minMax

    -- Default assignment: clear exception flags
    StatArray_D(MINMAX) <= (others => '0');

    -- Min/Max use quiet comparisons - only SNAN are invalid
    StatArray_D(MINMAX)(NV) <= to_sl(SignalingNaN_S);

    -- Both NaN inputs cause a NaN output
    if (IsNaNA_S and IsNaNB_S) then
      ResArray_D(MINMAX) <= NAN(EXP_BITS, MAN_BITS);  -- return canonical qnan

    -- If one operand is QNaN, the non-NaN operand is returned
    elsif IsNaNA_S then
      ResArray_D(MINMAX) <= B_DI;
    elsif IsNaNB_S then
      ResArray_D(MINMAX) <= A_DI;

    -- A is the desired output when smaller and min or larger and max
    elsif ((OperandASmaller_S and RoundMode_SI = RNE)
           or (not OperandASmaller_S and RoundMode_SI = RTZ)) then
      ResArray_D(MINMAX) <= A_DI;

    -- B is the desired output when smaller and min or larger and max
    elsif ((not OperandASmaller_S and RoundMode_SI = RNE)
           or (OperandASmaller_S and RoundMode_SI = RTZ)) then
      ResArray_D(MINMAX) <= B_DI;

    -- otherwise no valid op and optimize away
    else
      ResArray_D(MINMAX) <= (others => '-');

    end if;
  end process p_minMax;

  -- Result is floating-point number, always NaN-box
  ZextArray_S(MINMAX) <= '0';


  -----------------------------------------------------------------------------
  -- Comparisons - operation is encoded in RoundMode_SI:
  -- RNE = LE, RTZ = LT, RDN = EQ
  -- OpMod_SI inverts boolean outputs
  -----------------------------------------------------------------------------

  --! @brief Handle Comparisons and their special cases
  p_cmp : process (all) is
  begin  -- process p_comp

    -- Default assignment: FALSE and clear exception flags
    ResArray_D(CMP)  <= (others => '0');
    StatArray_D(CMP) <= (others => '0');

    -- Signaling NaNs are always illegal
    if SignalingNaN_S then
      -- result is 0 (false), already set
      StatArray_D(CMP)(NV) <= '1';      -- raise invalid exception

    -- LE and LT perform signalling comparisons: any NaN input is invalid
    elsif (RoundMode_SI = RNE or RoundMode_SI = RTZ) and InputNaN_S then
      -- result is 0 (false), already set
      StatArray_D(CMP)(NV) <= '1';      -- raise invalid exception

    -- Less or Equal
    elsif RoundMode_SI = RNE then
      ResArray_D(CMP)(0) <= to_sl(OperandASmaller_S or OperandsEqual_S) xor OpMod_SI;

    -- Less Than -> make sure -0 does not compare less than +0
    elsif RoundMode_SI = RTZ then
      ResArray_D(CMP)(0) <= to_sl(OperandASmaller_S and not OperandsEqual_S) xor OpMod_SI;

    -- Equals
    elsif RoundMode_SI = RDN and not InputNaN_S then
      ResArray_D(CMP)(0) <= to_sl(OperandsEqual_S) xor OpMod_SI;

    -- Equals with NaNs compares to false -> not equals on ANY NaN is actually TRUE!
    elsif InputNaN_S then
      ResArray_D(CMP)(0) <= OpMod_SI;

    -- otherwise no valid op and optimize away
    else
      ResArray_D(CMP) <= (others => '-');

    end if;
  end process p_cmp;

  -- Result is boolean in integer register, always zero-extend
  ZextArray_S(CMP) <= '1';


  -----------------------------------------------------------------------------
  -- Pipeline registers at the outputs of the unit
  -----------------------------------------------------------------------------

  -- We're outputting the enumerated classification for later unpacking since
  -- the scalar classification block is 10 bits wide which breaks FP8 here
  UnpackClass_S <= '1' when OpGroup_S = CLASS and VectorialOp_SI = '0' else
                   '0';

  -- Output should be zero-extended downstream if it doesn't contain FP values
  Zext_S <= ZextArray_S(OpGroup_S);

  -- Select output according to operation group and feed into pipeline
  Result_D <= ResArray_D(OpGroup_S);
  Status_D <= StatArray_D(OpGroup_S);

  -- Pipe through the classification block indicator as well
  TagInt_D <= UnpackClass_S & Zext_S & Tag_DI;

  i_fp_pipe : fp_pipe
    generic map (
      WIDTH     => EXP_BITS+MAN_BITS+1,
      LATENCY   => LATENCY,
      TAG_WIDTH => TAG_WIDTH+2)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      Result_DI      => Result_D,
      Status_DI      => Status_D,
      Tag_DI         => TagInt_D,
      InValid_SI     => InValid_SI,
      InReady_SO     => InReady_SO,
      Flush_SI       => Flush_SI,
      ResultPiped_DO => Z_DO,
      StatusPiped_DO => Status_DO,
      TagPiped_DO    => TagIntPiped_D,
      OutValid_SO    => OutValid_SO,
      OutReady_SI    => OutReady_SI);

  UnpackClass_SO <= TagIntPiped_D(TagIntPiped_D'high);
  Zext_SO        <= TagIntPiped_D(TagIntPiped_D'high-1);
  Tag_DO         <= TagIntPiped_D(Tag_DO'range);

end architecture rtl;

