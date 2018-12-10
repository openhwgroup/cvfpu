-------------------------------------------------------------------------------
-- Title      : Arbiter for FPnew results
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_arbiter.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-04-05
-- Last update: 2018-04-06
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
use work.fpnew_comps_pkg.all;


entity fp_arbiter is

  generic (
    DATA_WIDTH : natural := 16;
    NUM_INPUTS : natural := 2;
    TAG_WIDTH  : natural := 0);

  port (
    InResults_DI  : in  slArray2d_t(0 to NUM_INPUTS-1, DATA_WIDTH-1 downto 0);
    InStatuses_DI : in  statusArray_t(0 to NUM_INPUTS-1);
    InTags_DI     : in  slArray2d_t(0 to NUM_INPUTS-1, TAG_WIDTH-1 downto 0);
    InValid_SI    : in  std_logic_vector(0 to NUM_INPUTS-1);
    InReady_SO    : out std_logic_vector(0 to NUM_INPUTS-1);
    ---------------------------------------------------------------------------
    Priorities_SI : in  std_logic_vector(clog2(NUM_INPUTS)-1 downto 0);
    ---------------------------------------------------------------------------
    OutResult_DO  : out std_logic_vector(DATA_WIDTH-1 downto 0);
    OutStatus_DO  : out rvStatus_t;
    OutTag_DO     : out std_logic_vector(TAG_WIDTH-1 downto 0);
    OutValid_SO   : out std_logic;
    OutReady_SI   : in  std_logic;
    ---------------------------------------------------------------------------
    OutIdx_SO     : out std_logic_vector(clog2(NUM_INPUTS)-1 downto 0));


end entity fp_arbiter;


architecture rtl of fp_arbiter is

  -----------------------------------------------------------------------------
  -- Constant Definitions
  -----------------------------------------------------------------------------

  -- Split the arbitration tree in two half-trees
  constant NUM_LEFT  : natural := 2**(clog2(NUM_INPUTS)-1);
  constant NUM_RIGHT : natural := NUM_INPUTS - NUM_LEFT;

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------

  -- Input side of left subtree
  signal LeftInResults_D  : slArray2d_t(0 to NUM_LEFT-1, OutResult_DO'range);
  signal LeftInStatuses_D : statusArray_t(0 to NUM_LEFT-1);
  signal LeftInTags_D     : slArray2d_t(0 to NUM_LEFT-1, OutTag_DO'range);
  signal LeftInValid_S    : std_logic_vector(0 to NUM_LEFT-1);
  signal LeftInReady_S    : std_logic_vector(0 to NUM_LEFT-1);

  -- Input side of right subtree
  signal RightInResults_D  : slArray2d_t(0 to NUM_RIGHT-1, OutResult_DO'range);
  signal RightInStatuses_D : statusArray_t(0 to NUM_RIGHT-1);
  signal RightInTags_D     : slArray2d_t(0 to NUM_RIGHT-1, OutTag_DO'range);
  signal RightInValid_S    : std_logic_vector(0 to NUM_RIGHT-1);
  signal RightInReady_S    : std_logic_vector(0 to NUM_RIGHT-1);

  -- Result of subtrees
  signal LeftOutResult_D, RightOutResult_D : std_logic_vector(OutResult_DO'range);
  signal LeftOutStatus_D, RightOutStatus_D : rvStatus_t;
  signal LeftOutTag_D, RightOutTag_D       : std_logic_vector(OutTag_DO'range);

  -- Index of subtree results
  signal LeftOutIdx_S, RightOutIdx_S : std_logic_vector(clog2(NUM_INPUTS)-2 downto 0);

  -- Handshake signals for subtree output side
  signal LeftOutValid_S, RightOutVaLid_s : std_logic;
  signal LeftOutReady_S, RightOutReady_S : std_logic;

  -- Arbitration selection
  signal Selection_S : std_logic;       -- 1 for right, 0 for left

  -- Lower chosen index bits to OR onto ours
  signal SubOutIdx_S : std_logic_vector(clog2(NUM_INPUTS)-2 downto 0);


  -----------------------------------------------------------------------------
  -- Component declaration since we're instantiating ourselves
  -----------------------------------------------------------------------------
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


begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Split inputs to create subtree inputs
  -----------------------------------------------------------------------------
  p_subtreeInputs : process (all) is
  begin

    -- Left subtree
    LeftInStatuses_D <= InStatuses_DI(0 to NUM_LEFT-1);
    LeftInValid_S    <= InValid_SI(0 to NUM_LEFT-1);
    for idx in 0 to NUM_LEFT-1 loop
      copy_row(LeftInResults_D, idx, InResults_DI, idx);
      copy_row(LeftInTags_D, idx, InTags_DI, idx);
    end loop;

    -- Right subtree
    RightInStatuses_D <= InStatuses_DI(NUM_LEFT to NUM_INPUTS-1);
    RightInValid_S    <= InValid_SI(NUM_LEFT to NUM_INPUTS-1);
    for idx in 0 to NUM_RIGHT-1 loop
      copy_row(RightInResults_D, idx, InResults_DI, NUM_LEFT+idx);
      copy_row(RightInTags_D, idx, InTags_DI, NUM_LEFT+idx);
    end loop;

  end process p_subtreeInputs;

  -----------------------------------------------------------------------------
  -- Input side outputs coming from the subtrees
  -----------------------------------------------------------------------------

  -- Input side outputs from both subtrees are combined again
  InReady_SO(0 to NUM_LEFT-1) <= LeftInReady_S;
  InReady_SO(NUM_LEFT to NUM_INPUTS-1) <= RightInReady_S;


  -----------------------------------------------------------------------------
  -- Generate left subtree results
  -----------------------------------------------------------------------------

  -- Left side needs more subtrees
  g_instLeftSubree : if NUM_LEFT > 1 generate

    signal NarrowIdx_S : std_logic_vector(clog2(NUM_LEFT)-1 downto 0);

  begin

    -- Instantiate subtree
    i_arbiter_left : fp_arbiter
      generic map (
        DATA_WIDTH => DATA_WIDTH,
        TAG_WIDTH  => TAG_WIDTH,
        NUM_INPUTS => NUM_LEFT)
      port map (
        InResults_DI  => LeftInResults_D,
        InStatuses_DI => LeftInStatuses_D,
        InTags_DI     => LeftInTags_D,
        InValid_SI    => LeftInValid_S,
        InReady_SO    => LeftInReady_S,
        Priorities_SI => Priorities_SI(clog2(NUM_LEFT)-1 downto 0),
        OutResult_DO  => LeftOutResult_D,
        OutStatus_DO  => LeftOutStatus_D,
        OutTag_DO     => LeftOutTag_D,
        OutValid_SO   => LeftOutValid_S,
        OutReady_SI   => LeftOutReady_S,
        OutIdx_SO     => NarrowIdx_S);

    -- Zero-extend subtree index to shared subindex width
    LeftOutIdx_S <= std_logic_vector(resize(unsigned(NarrowIdx_S), SubOutIdx_S'length));

  end generate g_instLeftSubree;

  -- Left side has a leaf with only one input: use it for arbitration
  g_leftLeaf : if NUM_LEFT = 1 generate

    -- First (only) input is taken from the arrays
    get_row(LeftOutResult_D, LeftInResults_D, 0);
    get_row(LeftOutTag_D, LeftInTags_D, 0);
    LeftOutStatus_D <= LeftInStatuses_D(0);
    LeftOutValid_S  <= LeftInValid_S(0);

    -- Hand through ready signal
    LeftInReady_S(0) <= LeftOutReady_S;

    -- Set index to 0
    LeftOutIdx_S <= (others => '0');

  end generate g_leftLeaf;

  -- Left side has a dead leaf with no inputs: disable it
  g_leftDead : if NUM_LEFT = 0 generate

    LeftOutResult_D <= (others => '-');  -- don't care
    LeftOutStatus_D <= (others => '-');  -- don't care
    LeftOutTag_D    <= (others => '-');  -- don't care
    LeftOutIdx_S    <= (others => '-');  -- don't care
    LeftOutValid_S  <= '0';              -- never chosen

  end generate g_leftDead;

  -----------------------------------------------------------------------------
  -- Generate right subtree results
  -----------------------------------------------------------------------------

  -- Right side needs more subtrees
  g_instRightSubree : if NUM_RIGHT > 1 generate

    signal NarrowIdx_S : std_logic_vector(clog2(NUM_RIGHT)-1 downto 0);

  begin

    -- Instantiate subtree
    i_arbiter_right : fp_arbiter
      generic map (
        DATA_WIDTH => DATA_WIDTH,
        TAG_WIDTH  => TAG_WIDTH,
        NUM_INPUTS => NUM_RIGHT)
      port map (
        InResults_DI  => RightInResults_D,
        InStatuses_DI => RightInStatuses_D,
        InTags_DI     => RightInTags_D,
        InValid_SI    => RightInValid_S,
        InReady_SO    => RightInReady_S,
        Priorities_SI => Priorities_SI (clog2(NUM_RIGHT)-1 downto 0),
        OutResult_DO  => RightOutResult_D,
        OutStatus_DO  => RightOutStatus_D,
        OutTag_DO     => RightOutTag_D,
        OutValid_SO   => RightOutValid_S,
        OutReady_SI   => RightOutReady_S,
        OutIdx_SO     => NarrowIdx_S);

    -- Zero-extend subtree index to shared subindex width
    RightOutIdx_S <= std_logic_vector(resize(unsigned(NarrowIdx_S), SubOutIdx_S'length));

  end generate g_instRightSubree;

  -- Right side has a leaf with only one input: use it for arbitration
  g_rightLeaf : if NUM_RIGHT = 1 generate

    -- First (only) input is taken from the arrays
    get_row(RightOutResult_D, RightInResults_D, 0);
    get_row(RightOutTag_D, RightInTags_D, 0);
    RightOutStatus_D <= RightInStatuses_D(0);
    RightOutValid_S  <= RightInValid_S(0);

    -- Hand through ready signal
    RightInReady_S(0) <= RightOutReady_S;

    -- Set index to 0
    RightOutIdx_S <= (others => '0');

  end generate g_rightLeaf;

  -- Right side has a dead leaf with no inputs: disable it
  g_rightDead : if NUM_RIGHT = 0 generate

    RightOutResult_D <= (others => '-');  -- don't care
    RightOutStatus_D <= (others => '-');  -- don't care
    RightOutTag_D    <= (others => '-');  -- don't care
    RightOutIdx_S    <= (others => '-');  -- don't care
    RightOutValid_S  <= '0';              -- never chosen

  end generate g_rightDead;


  -----------------------------------------------------------------------------
  -- Arbitrate between the left and right subtree results and outputs
  -----------------------------------------------------------------------------

  p_selection : process (all) is


  begin  -- process p_arbitrate

    -- both subtrees carry a valid result
    if (LeftOutValid_S and RightOutValid_S) = '1' then
      -- Decide according to the topmost bit of the priority position
      Selection_S <= Priorities_SI (clog2(NUM_INPUTS)-1);

    -- right subtree carries a valid result
    elsif RightOutValid_S = '1' then
      Selection_S <= '1';

    -- otherwise pick left result
    else
      Selection_S <= '0';

    end if;

  end process p_selection;

  -- Select the subtree result according to the selection
  OutResult_DO <= RightOutResult_D when Selection_S = '1' else LeftOutResult_D;
  OutStatus_DO <= RightOutStatus_D when Selection_S = '1' else LeftOutStatus_D;
  OutTag_DO    <= RightOutTag_D    when Selection_S = '1' else LeftOutTag_D;

  -- We're applying a valid output if one of the subtrees is valid
  OutValid_SO <= LeftOutValid_S or RightOutValid_S;

  -- Let the corresponding subtree know we are ready to use its result
  LeftOutReady_S  <= OutReady_SI and (not Selection_S);  -- left chosen
  RightOutReady_S <= OutReady_SI and Selection_S;        -- right chosen

  -- Chosen subIndex
  SubOutIdx_S <= RightOutIdx_S when Selection_S = '1' else LeftOutIdx_S;

  -- The Index of this output is the subIndex with our choice on top
  OutIdx_SO(clog2(NUM_INPUTS)-1)          <= Selection_S;
  OutIdx_SO(clog2(NUM_INPUTS)-2 downto 0) <= SubOutIdx_S;


end architecture rtl;
