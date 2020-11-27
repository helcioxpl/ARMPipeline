library IEEE; use IEEE.STD_LOGIC_1164.all;
entity hazarddec is -- single cycle processor
  port(clk, reset:        in  STD_LOGIC;
       MEMRegWrite, ExRegWrite: in  STD_LOGIC;
       MEMRegRd,    WBRegRd :   in  STD_LOGIC_VECTOR(4 downto 0);
       EXRegRn,     IDRegRn :   in  STD_LOGIC_VECTOR(4 downto 0);
       EXRegRm,     IDRegRm :   in  STD_LOGIC_VECTOR(4 downto 0);
       EXMemRead            : in  STD_LOGIC;

       Stall                : out STD_LOGIC);
end;
architecture struct of arm is
begin
end architecture;