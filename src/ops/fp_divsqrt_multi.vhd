-------------------------------------------------------------------------------
-- Title      : Multiformat Division and Square Root
-- Project    :
-------------------------------------------------------------------------------
-- File       : fp_divsqrt_multi.vhd
-- Author     : Stefan Mach  <smach@iis.ee.ethz.ch>
-- Company    : Integrated Systems Laboratory, ETH Zurich
-- Created    : 2018-04-08
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

entity fp_divsqrt_multi is

  generic (
    FORMATS : activeFormats_t := (Active   => (FP32 to FP16ALT => true, others => false),
                                  Encoding => DEFAULTENCODING);

    LATENCY   : natural := 0;
    TAG_WIDTH : natural := 0);

  port (
    Clk_CI           : in  std_logic;
    Reset_RBI        : in  std_logic;
    ---------------------------------------------------------------------------
    A_DI, B_DI       : in  std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
    ABox_SI, BBox_SI : in  fmtLogic_t;
    RoundMode_SI     : in  rvRoundingMode_t;
    Op_SI            : in  fpOp_t;
    OpMod_SI         : in  std_logic;
    FpFmt_SI         : in  fpFmt_t;
    Tag_DI           : in  std_logic_vector(TAG_WIDTH-1 downto 0);
        PrecCtl_SI       : in std_logic_vector(6 downto 0);
    ---------------------------------------------------------------------------
    InValid_SI       : in  std_logic;
    InReady_SO       : out std_logic;
    Flush_SI         : in  std_logic;
    ---------------------------------------------------------------------------
    Z_DO             : out std_logic_vector(MAXWIDTH(FORMATS)-1 downto 0);
    Status_DO        : out rvStatus_t;
    Tag_DO           : out std_logic_vector(TAG_WIDTH-1 downto 0);
    Zext_SO          : out std_logic;
    ---------------------------------------------------------------------------
    OutValid_SO      : out std_logic;
    OutReady_SI      : in  std_logic);

end entity fp_divsqrt_multi;


architecture iterative_lei of fp_divsqrt_multi is

  -----------------------------------------------------------------------------
  -- Constant Definitions
  -----------------------------------------------------------------------------
  constant FP_WIDTH : natural := MAXWIDTH(FORMATS);

  -----------------------------------------------------------------------------
  -- Type Definitions
  -----------------------------------------------------------------------------
  type t_ffsmState is (IDLE, BUSY, HOLD);
  type t_pipeDataSrc is (DIRECT, HOLDREG);

  -----------------------------------------------------------------------------
  -- Signal Declarations
  -----------------------------------------------------------------------------
  -- Input Handshaking
  signal InReady_S : std_logic;
  signal IsInFP8_S : boolean;

  -- DivSqrt input side
  signal DivValid_S, SqrtValid_S : std_logic;
  signal DivSqrtReady_S          : std_logic;
  signal A_D, B_D                : std_logic_vector(63 downto 0);
  signal Fmt_S                   : std_logic_vector(1 downto 0);

  -- DivSqrt output side
  signal DivSqrtDone_S      : std_logic;
  signal DivSqrtResultPre_D : std_logic_vector(63 downto 0);
  signal DivSqrtResult_D    : std_logic_vector(Z_DO'range);
  signal DivSqrtStatusSlv_D : std_logic_vector(4 downto 0);
  signal DivSqrtStatus_D    : rvStatus_t;

  -- Tag buffer
  signal CurrentTag_DP : std_logic_vector(TAG_WIDTH-1 downto 0);
  signal IsOutFP8_SP   : boolean;

  -- Output holding
  signal HoldResult_S  : std_logic;
  signal HoldResult_DP : std_logic_vector(Z_DO'range);
  signal HoldStatus_DP : rvStatus_t;

  -- Output pipelining
  signal PipeInValid_S, PipeInReady_S : std_logic;
  signal PipeInDataSel_S              : t_pipeDataSrc;
  signal PipeInResult_D               : std_logic_vector(Z_DO'range);
  signal PipeInStatus_D               : rvStatus_t;

  -- FSM states
  signal State_DP, State_DN : t_ffsmState;

  -----------------------------------------------------------------------------
  -- Component Declarations
  -----------------------------------------------------------------------------
  component div_sqrt_top_mvp is
    port (
      Clk_CI           : in  std_logic;
      Rst_RBI          : in  std_logic;
      Div_start_SI     : in  std_logic;
      Sqrt_start_SI    : in  std_logic;
      Operand_a_DI     : in  std_logic_vector(63 downto 0);
      Operand_b_DI     : in  std_logic_vector(63 downto 0);
      RM_SI            : in  std_logic_vector(2 downto 0);
      Precision_ctl_SI : in  std_logic_vector(5 downto 0);
      Format_sel_SI    : in  std_logic_vector(1 downto 0);
      Kill_SI          : in  std_logic;
      Result_DO        : out std_logic_vector(63 downto 0);
      Fflags_SO        : out std_logic_vector(4 downto 0);
      Ready_SO         : out std_logic;
      Done_SO          : out std_logic);
  end component div_sqrt_top_mvp;

begin  -- architecture iterative_lei

  -----------------------------------------------------------------------------
  -- Input side signals
  -----------------------------------------------------------------------------

  -- Format encoding of unit
  with FpFmt_SI select
    Fmt_S <=
    "00" when FP32,
    "01" when FP64,
    "10" when FP16,
    "11" when FP16ALT,
    "10" when others;                   -- map fp8 to fp16

  IsInFP8_S <= FpFmt_SI = FP8;

  -- Map FP8 onto FP16
  A_D <= std_logic_vector(resize(unsigned(A_DI), 64) sll 8) when IsInFP8_S else
         std_logic_vector(resize(unsigned(A_DI), 64));

  B_D <= std_logic_vector(resize(unsigned(B_DI), 64) sll 8) when IsInFP8_S else
         std_logic_vector(resize(unsigned(B_DI), 64));

  -- Upstream ready given by FSM
  InReady_SO <= InReady_S;

  -----------------------------------------------------------------------------
  -- Control
  -----------------------------------------------------------------------------
  -- Operation is only started when the control FSM is ready
  DivValid_S  <= InValid_SI and to_sl(Op_SI = DIV) and InReady_S and not Flush_SI;
  SqrtValid_S <= InValid_SI and to_sl(Op_SI /= DIV) and InReady_S and not Flush_SI;

  -- FSM process
  p_flagFSM : process (all) is
  begin
    -- Default Assignments
    InReady_S       <= '0';
    PipeInValid_S   <= '0';
    PipeInDataSel_S <= DIRECT;          -- Divsqrt feeds pipeline directly
    HoldResult_S    <= '0';             -- Don't save divsqrt output to hold
    State_DN        <= State_DP;        -- By default, stay in the same state

    -- FSM
    case State_DP is

      -- Waiting for work
      when IDLE =>
        InReady_S <= '1';               -- We're ready
        -- New work arrives
--         if (DivValid_S or SqrtValid_S) = '1' then
        if ((DivValid_S or SqrtValid_S) and DivSqrtReady_S) = '1' then
          State_DN <= BUSY;
        end if;

      -- Operation in progress
      when BUSY =>
        -- Wait until divsqrt is done
        if DivSqrtDone_S = '1' then
          PipeInValid_S <= '1';         -- Apply outputs to Pipeline
          -- The result will be processed downstream
          if PipeInReady_S = '1' then
            State_DN <= IDLE;           -- We can go back to idling
            -- ..unless there is another incoming instruction
--             if InValid_SI = '1' then
            if (InValid_SI and DivSqrtReady_S) = '1' then
              InReady_S <= '1';         -- We take the next instruction
              State_DN  <= BUSY;        -- And stay busy with it
            end if;
          -- The downstream pipeline is not ready for us
          else
            HoldResult_S <= '1';        -- Activate the hold register
            State_DN     <= HOLD;       -- Wait until the pipeline is unstuck
          end if;
        end if;

      -- Holding data for output pipe
      when HOLD =>
        PipeInDataSel_S <= HOLDREG;     -- Apply data from hold reg to pipe
        PipeInValid_S   <= '1';         -- We have valid data
        -- Wait until result will be processed downstream
        if PipeInReady_S = '1' then
          State_DN <= IDLE;             -- We can go back to idling
          -- ..unless there is another incoming instruction
--           if InValid_SI = '1' then
          if (InValid_SI and DivSqrtReady_S) = '1' then
            InReady_S <= '1';           -- We take the next instruction
            State_DN  <= BUSY;          -- And stay busy with it
          end if;
        end if;

    end case;

    -- Flushing overrides the other actions
    if Flush_SI = '1' then
      PipeInValid_S <= '0';             -- Don't commit to pipe
      State_DN <= IDLE;                 -- Go back to default state
    end if;

  end process p_flagFSM;


  -----------------------------------------------------------------------------
  -- Instance of multifmt div/sqrt unit
  -----------------------------------------------------------------------------

  i_fp_divsqrt : div_sqrt_top_mvp
    port map (
      Clk_CI           => Clk_CI,
      Rst_RBI          => Reset_RBI,
      Div_start_SI     => DivValid_S,
      Sqrt_start_SI    => SqrtValid_S,
      Operand_a_DI     => A_D,
      Operand_b_DI     => B_D,
      RM_SI            => to_slv(RoundMode_SI),
      Precision_ctl_SI => PrecCtl_SI(5 downto 0),
      Format_sel_SI    => Fmt_S,
      Kill_SI          => Flush_SI,
      Result_DO        => DivSqrtResultPre_D,
      Fflags_SO        => DivSqrtStatusSlv_D,
      Ready_SO         => DivSqrtReady_S,
      Done_SO          => DivSqrtDone_S);

  DivSqrtResult_D <= std_logic_vector(resize(unsigned(DivSqrtResultPre_D), Z_DO'length) srl 8) when IsOutFP8_SP else
                     std_logic_vector(resize(unsigned(DivSqrtResultPre_D), Z_DO'length));
  DivSqrtStatus_D <= to_rvStatus(DivSqrtStatusSlv_D);


  -----------------------------------------------------------------------------
  -- Tag Buffer and output hold register, also FSM state keeping
  -----------------------------------------------------------------------------

  p_registers : process (Clk_CI, Reset_RBI) is
  begin  -- process p_tagBuffer
    if Reset_RBI = '0' then             -- asynchronous reset (active low)
      --FSM state--------------------------------------------------------------
      State_DP      <= IDLE;
      --Tag Buffer-------------------------------------------------------------
      CurrentTag_DP <= (others => '0');
      IsOutFP8_SP   <= false;
      --Hold Register----------------------------------------------------------
      HoldResult_DP <= (others => '0');
      HoldStatus_DP <= (others => '0');
    elsif Clk_CI'event and Clk_CI = '1' then  -- rising clock edge
      -- Advance FSM state
      State_DP <= State_DN;
      -- Only store tag if a new operation starts
      if (DivValid_S or SqrtValid_S) = '1' then
        CurrentTag_DP <= Tag_DI;
        IsOutFP8_SP   <= IsInFP8_S;
      end if;
      -- Hold register is enabled if needed
      if HoldResult_S = '1' then
        HoldResult_DP <= DivSqrtResult_D;
        HoldStatus_DP <= DivSqrtStatus_D;
      end if;
    end if;
  end process p_registers;

  -----------------------------------------------------------------------------
  -- Pipeline registers at the outputs of the unit
  -----------------------------------------------------------------------------

  PipeInResult_D <= HoldResult_DP when PipeInDataSel_S = HOLDREG else DivSqrtResult_D;
  PipeInStatus_D <= HoldStatus_DP when PipeInDataSel_S = HOLDREG else DivSqrtStatus_D;

  i_fp_pipe : fp_pipe
    generic map (
      WIDTH     => FP_WIDTH,
      LATENCY   => LATENCY,
      TAG_WIDTH => TAG_WIDTH)
    port map (
      Clk_CI         => Clk_CI,
      Reset_RBI      => Reset_RBI,
      Result_DI      => PipeInResult_D,
      Status_DI      => PipeInStatus_D,
      Tag_DI         => CurrentTag_DP,
      InValid_SI     => PipeInValid_S,
      InReady_SO     => PipeInReady_S,
      Flush_SI       => Flush_SI,
      ResultPiped_DO => Z_DO,
      StatusPiped_DO => Status_DO,
      TagPiped_DO    => Tag_DO,
      OutValid_SO    => OutValid_SO,
      OutReady_SI    => OutReady_SI);

  Zext_SO <= '0';                       -- always NaN-box


end architecture iterative_lei;
