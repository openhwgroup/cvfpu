-------------------------------------------------------------------------------
-- Title      : Pipeline Stage for FP Operations
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_pipe.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-03-20
-- Last update: 2018-04-13
-- Platform   : ModelSim (simulation), Synopsys (synthesis)
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Wrapper for pipeline stages to be inserted at the output of
--              individual FP operations.
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
use work.fpnew_pkg.all;

entity fp_pipe is

  generic (
    WIDTH     : natural := 16;
    LATENCY   : natural := 0;
    TAG_WIDTH : natural := 0);

  port (
    Clk_CI         : in  std_logic;
    Reset_RBI      : in  std_logic;
    ---------------------------------------------------------------------------
    Result_DI      : in  std_logic_vector(WIDTH-1 downto 0);
    Status_DI      : in  rvStatus_t;
    Tag_DI         : in  std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI     : in  std_logic;
    InReady_SO     : out std_logic;     -- Ready to Upstream
    Flush_SI       : in  std_logic;
    ---------------------------------------------------------------------------
    ResultPiped_DO : out std_logic_vector(WIDTH-1 downto 0);
    StatusPiped_DO : out rvStatus_t;
    TagPiped_DO    : out std_logic_vector(TAG_WIDTH-1 downto 0);
    ---------------------------------------------------------------------------
    OutValid_SO    : out std_logic;
    OutReady_SI    : in  std_logic);    -- Ready from Downstream

end entity fp_pipe;



architecture rtl of fp_pipe is

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------

  --! @brief Array of output words indexed by latency
  --! @details An output word for each pipeline stage in this unit.
  --! \note This type cannot live in the package since the base type must be
  --! constrained which is dependent on the FP format. VHDL-08 generic packages
  --! would provide a nice solution but are not supported in many tools :(
  type resultPipe_t is array (natural range <>) of std_logic_vector(Result_DI'range);

  type tagPipe_t is array (natural range <>) of std_logic_vector(Tag_DI'range);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------
  -- Pipelining Signals, width determined by LATENCY
  signal ResPipe_D   : resultPipe_t(0 to LATENCY);
  signal StatPipe_D  : statusArray_t(0 to LATENCY);
  signal TagPipe_D   : tagPipe_t(0 to LATENCY);
  signal ValidPipe_S : std_logic_vector(0 to LATENCY);

  signal StageReady_S : std_logic_vector(0 to LATENCY);

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Pipeline Generation
  -----------------------------------------------------------------------------

  -- Assign the leftmost elements of the pipelining vectors (downtstream)
  ResPipe_D(0)   <= Result_DI;
  StatPipe_D(0)  <= Status_DI;
  TagPipe_D(0)   <= Tag_DI;
  ValidPipe_S(0) <= InValid_SI;

  -- Assign the rightmost element of the ready signal
  StageReady_S(LATENCY) <= OutReady_SI;

  --! @brief Generates pipeline registers with bubble popping
  --! @details Generates pipeline stages as given by LATENCY. When LATENCY
  --! is 0, the range 0 to -1 is a null range and nothing is generated.
  --! <pre>
  --!         +---------|---------|---------|----------|---------+
  --!         |         |         |         |          |         |
  --! Result  >=========|=========|=========|====~~====|=========> ResultPiped
  --! Status  >=========|=========|=========|====~~====|=========> StatusPiped
  --! Tag     >=========|=========|=========|====~~====|=========> TagPiped
  --! Enable  >---------|---------|---------|----~~----|---------> Valid
  --! Ready   <---------|---------|---------|----~~----|---------< Ready
  --!         |         |         |         |          |         |
  --! stage # +----0----|----1----|----2----|----..----|-LATENCY-+
  --! </pre>
  --! \note These registers must be retimed in synthesis for sensible
  --! pipelining. Make sure to optimize registers through the instantiating
  --! hierarchy.
  --! \note The ready signal is not a direct feed-through from destination to
  --! source but takes into account intermediate 'bubbles' in the pipeline. As
  --! such, downstream stalls can be hidden when the pipeline is not full.
  --! \note Enable signals on the registers will lead to clock-gated pipeline
  --! stages when this optimization is enabled during synthesis. Make sure to
  --! optimize clock gates through hierarchies.
  g_pipeStage : for i in 0 to LATENCY-1 generate

    -- Determine the ready signal of the current stage - advance the pipeline:
    -- 1. if the next stage is ready for our data
    -- 2. if the next stage only holds a bubble (not valid) -> we can pop it
    StageReady_S(i) <= StageReady_S(i+1) or (not ValidPipe_S(i+1));

    process (Clk_CI, Reset_RBI) is
    begin  -- process
      if Reset_RBI = '0' then           -- asynchronous reset (active low)
        --ResPipe_D(i+1)   <= (others => '0');
        --StatPipe_D(i+1)  <= (others => '0');
        --TagPipe_D(i+1)   <= (others => '0');
        ValidPipe_S(i+1) <= '0';
      elsif Clk_CI'event and Clk_CI = '1' then  -- rising clock edge
        if Flush_SI = '1' then
          ValidPipe_S(i+1) <= '0';
        elsif StageReady_S(i) = '1' then -- only advance pipeline if we're ready
          ValidPipe_S(i+1) <= ValidPipe_S(i);
        end if;

        if StageReady_S(i) = '1' and ValidPipe_S(i) = '1' then  -- Clock-gate data unless we're valid
          ResPipe_D(i+1)  <= ResPipe_D(i);
          StatPipe_D(i+1) <= StatPipe_D(i);
          TagPipe_D(i+1)  <= TagPipe_D(i);
        end if;
      end if;
        --if (StageReady_S(i) or Flush_SI) = '1' then   -- Only advance pipeline if we're ready
        --  ValidPipe_S(i+1) <= ValidPipe_S(i) and not Flush_SI;
        --  if ValidPipe_S(i) = '1' then  -- Clock-gate data unless we're valid
        --    ResPipe_D(i+1)  <= ResPipe_D(i);
        --    StatPipe_D(i+1) <= StatPipe_D(i);
        --    TagPipe_D(i+1)  <= TagPipe_D(i);
        --  end if;
        --end if;
    end process;

  end generate g_pipeStage;

  -----------------------------------------------------------------------------
  -- Output Assignment
  -----------------------------------------------------------------------------

  -- Assign the rightmost elements of the pipelining vectors
  ResultPiped_DO <= ResPipe_D(LATENCY);
  StatusPiped_DO <= StatPipe_D(LATENCY);
  TagPiped_DO    <= TagPipe_D(LATENCY);
  OutValid_SO    <= ValidPipe_S(LATENCY);

  -- Assign the leftmost element of the ready signal
  InReady_SO <= StageReady_S(0);

end architecture rtl;
