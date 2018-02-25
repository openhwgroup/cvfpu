


library IEEE, DWARE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use DWARE.DWpackages.all;
use DWARE.DW_Foundation_comp_arith.all;


entity tb is

end entity tb;


architecture lol of tb is

  signal A_D, B_D, C_D : std_logic_vector(15 downto 0);
  signal Z_D, Zdw_D    : std_logic_vector(15 downto 0);
  signal ExpOut_D      : signed(6 downto 0);
  signal MantOut_D     : std_logic_vector(12 downto 0);
  signal SignOut_D     : std_logic;

  signal IsNormalA_D, IsNormalB_D, IsNormalC_D : std_logic;



  component sFlt16 is
    generic (
      WIDTH    : natural;
      EXP_BITS : natural;
      MAN_BITS : natural);
    port (
      A_DI, B_DI, C_DI : in  std_logic_vector(WIDTH-1 downto 0);
      RoundMode_SI     : in  std_logic_vector(2 downto 0);
      Z_DO             : out std_logic_vector(WIDTH-1 downto 0));
  end component sFlt16;


begin

  i_sFlt16_1: sFlt16
    generic map (
      WIDTH    => 16,
      EXP_BITS => 5,
      MAN_BITS => 10)
    port map (
      A_DI         => A_D,
      B_DI         => B_D,
      C_DI         => C_D,
      RoundMode_SI => "000",
      Z_DO         => Z_D);


  i_dware : DW_fp_mac
    generic map (
      sig_width       => 10,
      exp_width       => 5,
      ieee_compliance => 1
      )
    port map (
      a      => A_D,
      b      => B_D,
      c      => C_D,
      rnd    => "000",
      z      => Zdw_D,
      status => open);




end architecture lol;
