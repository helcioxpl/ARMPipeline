library IEEE; use IEEE.STD_LOGIC_1164.all;
entity hazarddec is
  port(clock, reset      : in  STD_LOGIC;

       ExMemRead         : in  STD_LOGIC;
       PCSrc             : in  STD_LOGIC;

       RAs              : in  STD_LOGIC_VECTOR(7 downto 0);
       WA3E, WA3M, WA3W : in  STD_LOGIC_VECTOR(3 downto 0);
       RWE, RWM, RWW    : in  STD_LOGIC;

       Flush, Stall     : out STD_LOGIC);
end;
architecture struct of hazarddec is
    component compReg is
    port(
        Rd, Rn, Rm : in  STD_LOGIC_VECTOR(3 downto 0);
        N15, EN : in STD_LOGIC;
        eq : out STD_LOGIC);
    end component;
    signal EX, MEM, WB : STD_LOGIC := '0';
    signal DoubleStall : STD_LOGIC := '0';
    signal RA1D, RA2D, RA1E, RA2E: STD_LOGIC_VECTOR(3 downto 0);
begin
    (RA1D, RA2D) <= RAs;
    Regs1: entity work.flopenr(asynchronous) generic map(4)
        port map (clock, reset or Flush, '1', RA1D, RA1E);
    Regs2: entity work.flopenr(asynchronous) generic map(4)
        port map (clock, reset or Flush, '1', RA2D, RA2E);

    CompE : compReg port map (WA3E, RA1D, RA1D, RWE, '0', EX);
    CompM : compReg port map (WA3M, RA1E, RA2E, RWM, '1', MEM);
    CompW : compReg port map (WA3W, RA1E, RA2E, RWW, '1', WB);

    process(clock, DoubleStall, MEM)
    begin
        if (clock'event and clock='1') then
            if (DoubleStall = '1') then DoubleStall <= '0';
            elsif MEM = '1' then        DoubleStall <= '1';
            else                        DoubleStall <= '0';
            end if;
        end if;
   end process;

   Stall <= EX or MEM or WB or DoubleStall or PCSrc;
   Flush <= Stall;
end architecture;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity compReg is
  port(
       Rd, Rn, Rm : in  STD_LOGIC_VECTOR(3 downto 0);
       EN, N15 : in STD_LOGIC;
       eq : out STD_LOGIC);
end;
architecture struct of compReg is
    component comp is
    port(
        a, b : in  STD_LOGIC_VECTOR(3 downto 0);
        eq: out STD_LOGIC);
    end component;
    signal a, b, is15 : STD_LOGIC;
begin
    CompA  : comp port map (Rd, Rn, a);
    CompB  : comp port map (Rd, Rm, b);
    Comp15 : comp port map (Rd, "1111", is15);
    eq <= (a or b) and (N15 xor is15) and EN;
end architecture;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity comp is
  port(
    a, b : in  STD_LOGIC_VECTOR(3 downto 0);
    eq: out STD_LOGIC);
end;
architecture struct of comp is
    signal eqv : STD_LOGIC_VECTOR(3 downto 0);
begin
	CompBits:
	FOR I in 0 to 3 generate
        eqv(I) <= a(I) xnor b(I);
	end generate;
    eq <= eqv(0) and eqv(1) and eqv(2) and eqv(3);
end architecture;

