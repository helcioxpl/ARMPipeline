library IEEE; use IEEE.STD_LOGIC_1164.all;
entity hazarddec is
  port(clock, reset      : in  STD_LOGIC;
       MEMRegWrite       : in  STD_LOGIC;
       ExRegWrite        : in  STD_LOGIC;
       WBRegWrite        : in  STD_LOGIC;
       ExRd, MemRd, WbRd : in  STD_LOGIC_VECTOR(4 downto 0);
       ExRn,     IdRn    : in  STD_LOGIC_VECTOR(4 downto 0);
       ExRm,     IdRm    : in  STD_LOGIC_VECTOR(4 downto 0);
       ExMemRead         : in  STD_LOGIC;
       PCSrc             : in  STD_LOGIC;
       Stall             : out STD_LOGIC);
end;
architecture struct of hazarddec is
    component compReg is
    port(
        Rd, Rn, Rm : in  STD_LOGIC_VECTOR(4 downto 0);
        N31 : in STD_LOGIC;
        eq : out STD_LOGIC);
    end component;
    signal EX, MEM, WB : STD_LOGIC;
    signal MEM31, WB31 : STD_LOGIC;
    signal DoubleStall : STD_LOGIC;
begin
    CompEX : compReg port map (ExRd, IdRn, IdRm, '0', EX);
    CompMEM : compReg port map (MemRd, ExRn, ExRm, '1', MEM);
    CompWB : compReg port map (WbRd, ExRn, ExRm, '1', WB);

    process(clock, DoubleStall, MEMRegWrite, MEM)
    begin
        if (clock'event and clock='1') then
            if (DoubleStall = '1') then
                DoubleStall <= '0';
            elsif (MEM = '1' and MEMRegWrite = '1') then
                DoubleStall <= '1';
            end if;
        end if;
   end process;

    Stall <= (EX and EXMemRead)
          or(MEM and MEMRegWrite)
          or (WB and WBRegWrite)
          or DoubleStall
          or PCSrc;
end architecture;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity compReg is
  port(
       Rd, Rn, Rm : in  STD_LOGIC_VECTOR(4 downto 0);
       N31 : in STD_LOGIC;
       eq : out STD_LOGIC);
end;
architecture struct of compReg is
    component comp is
    port(
        a, b : in  STD_LOGIC_VECTOR(4 downto 0);
        eq: out STD_LOGIC);
    end component;
    signal a, b, is31 : STD_LOGIC;
begin
    CompA  : comp port map (Rd, Rn, a);
    CompB  : comp port map (Rd, Rm, b);
    Comp31 : comp port map (Rd, "11111", is31);
    eq <= (a or b) and (N31 xor is31);
end architecture;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity comp is
  port(
    a, b : in  STD_LOGIC_VECTOR(4 downto 0);
    eq: out STD_LOGIC);
end;
architecture struct of comp is
    signal eqv : STD_LOGIC_VECTOR(4 downto 0);
begin
	CompBits:
	FOR I in 0 to 4 generate
        eqv(I) <= a(I) xnor b(I);
	end generate;
    eq <= eqv(0) and eqv(1) and eqv(2) and eqv(3) and eqv(4);
end architecture;

