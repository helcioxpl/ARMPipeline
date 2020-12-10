library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity datapath is  
  port(clk, reset:           in  STD_LOGIC;
       controls:             in  STD_LOGIC_VECTOR(12 downto 0);

       MemWrite:             out STD_LOGIC;
       ALUFlags:             out STD_LOGIC_VECTOR(3 downto 0);
       PC:                   buffer STD_LOGIC_VECTOR(31 downto 0);
       Instr:                in  STD_LOGIC_VECTOR(31 downto 0);
       ALUResultM, WriteDataM: buffer STD_LOGIC_VECTOR(31 downto 0);
       ReadDataM:            in  STD_LOGIC_VECTOR(31 downto 0);

       StageWE:              in STD_LOGIC_VECTOR(3 downto 0);
       FlushE, FlushD:  in STD_LOGIC);
end;

architecture struct of datapath is
  signal PCPlus4, ExtImm, SrcA, Result:     STD_LOGIC_VECTOR(31 downto 0);
  signal WriteDataE, ALUResultE, ReadDataW: std_logic_vector(31 downto 0);

  signal controlsE:    std_logic_vector(13 downto 0);
  signal controlsM:    std_logic_vector(8 downto 0);
  signal controlsW:    std_logic_vector(7 downto 0);

  signal PCSrc, RegWrite, MemtoReg: std_logic;
  signal WA3: std_logic_vector(3 downto 0);
begin
  -- controls order:
  -- WB:  WA3 & PCSrc(IF) & RegWrite & MemtoReg
  -- MEM: MemWrite
  -- EX:  Branch & FlagWrite & ALUControl & ALUSrc
  -- ID:  RegSrc & ImmSrc

  i_IF : entity work.IF_FD(struct) port map(clk, reset, StageWE(0), PCSrc, PC, PCPlus4, Result);
  i_ID : entity work.ID_FD(struct) port map(clk, reset, FlushD, StageWE(1),
    RegSrc, RegWrite, ImmSrc, WA3,    PCPlus4, Instr, Result, ExtImm, SrcA, WriteData);

  (controlsE, RegSrc, ImmSrc) <= Instr(15 downto 12) & controls;
  i_EX : entity work.EX_FD(struct) port map(clk, reset, FlushE, controlsE, controlsM,
      ALUFlags, SrcA, ExtImm, ALUResult, WriteDataE);

  MEM: entity work.regbar(struct) generic map(8, 2) port map(clk, reset, StageWE(3),
    controlsM, controlsW & MemWrite,    WriteDataE & ALUResultE, WriteDataM & ALUResultM);

  WB_FD: entity work.mux2(behave) generic map(32) port map(ALUResultW, ReadDataW, MemtoReg, Result);
  WB: entity work.regbar(struct) generic map(7, 2) port map(clk, reset, StageWE(4),
    controlsW, WA3 & PCSrc & RegWrite & MemtoReg,    ALUResultM & ReadDataM, ALUResultW & ReadDataW);

  HD: entity work.hazarddec(struct) port(
    clock, reset,
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
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity IF_FD is  
  port(clk, reset, write: in  STD_LOGIC;
       PCSrc:             in  STD_LOGIC;
       PC:                buffer STD_LOGIC_VECTOR(31 downto 0);
       PCPlus4:           buffer STD_LOGIC_VECTOR(31 downto 0);
       Result:            in STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of IF_FD is
  signal PCNext, PCF: STD_LOGIC_VECTOR(31 downto 0);
begin
  pcmux: entity work.mux2(behave) generic map(32) port map(PCPlus4, Result, PCSrc, PCNext);
  pcreg: entity work.flopr(asynchronous) generic map(32) port map(clk, reset, PCNext, PCF);
  IFReg: entity work.flopenr(asynchronous) generic map(32) port map(clk, reset, write, PCF, PC);
  pcadd1: entity work.adder(behave) port map(PC, X"00000004", PCPlus4);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity ID_FD is  
  port(clk, reset:        in  STD_LOGIC;
       IDRegClr, IDRegWE: in  STD_LOGIC;
       RegSrc:            in  STD_LOGIC_VECTOR(1 downto 0);
       RegWrite:          in  STD_LOGIC;
       ImmSrc:            in  STD_LOGIC_VECTOR(1 downto 0);

       WA3:               in  STD_LOGIC_VECTOR(3 downto 0);
       PCPlus8D:          in  STD_LOGIC_VECTOR(31 downto 0);
       Instr:             in  STD_LOGIC_VECTOR(31 downto 0);
       Result:            in  STD_LOGIC_VECTOR(31 downto 0);

       ExtImm, RD1, RD2:  out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of ID_FD is
  signal RA1, RA2: STD_LOGIC_VECTOR(3 downto 0);
  signal InstrD  : STD_LOGIC_VECTOR(31 downto 0);
begin
  IDReg: entity work.flopenr(asynchronous) generic map(32) port map(clk, IDRegClr, IDRegWE, Instr, InstrD);

  ra1mux: entity work.mux2(behave) generic map (4)
    port map(InstrD(19 downto 16), "1111", RegSrc(0), RA1);
  ra2mux: entity work.mux2(behave)
    generic map (4) port map(InstrD(3 downto 0), 
    InstrD(15 downto 12), RegSrc(1), RA2);
  rf: entity work.regfile(behave) port map(
    clk, RegWrite, RA1, RA2, WA3,
    Result, PCPlus8D, RD1, RD2);
  ext: entity work.extend(behave) port map(InstrD(23 downto 0), ImmSrc, ExtImm);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity EX_FD is  
  port(clk, reset, clear: in  STD_LOGIC;
       i_controls:        in  STD_LOGIC_VECTOR(10 downto 0);
       o_controls:        in  STD_LOGIC_VECTOR(8 downto 0);

       ALUFlags:          out STD_LOGIC_VECTOR(3 downto 0);
       SrcA:              in STD_LOGIC_VECTOR(31 downto 0);
       ExtImm:            in STD_LOGIC_VECTOR(31 downto 0);
       ALUResult, WriteData: buffer STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of EX_FD is
  signal s_controls : STD_LOGIC_VECTOR(8 downto 0);
  signal ALUControl : STD_LOGIC_VECTOR(1 downto 0);
  signal ALUSrc, Branch, FlagWrite : STD_LOGIC;
  signal SrcAE, ScrBE, WriteDataE, ExtImmE : STD_LOGIC_VECTOR(31 downto 0);
begin
  Regs: entity work.regbar(struct) generic map(10, 3) port map(clk, reset, StageWE(2),
    i_controls, s_controls & Branch & FlagWrite & ALUControl & ALUSrc,
    SrcA & WriteData & ExtImm, SrcAE & WriteDataE & ExtImmE);

  srcbmux: entity work.mux2(behave) generic map(32) 
    port map(WriteDataE, ExtImmE, ALUSrc, SrcBE);
  i_alu: entity work.alu(behave) port map(SrcAE, SrcBE, ALUControl, ALUResult, ALUFlags);

  cl: entity work.condlogic(behave) port map(
    clk, reset, Instr(31 downto 28),
    ALUFlags, FlagWrite, s_controls(9 downto 6), o_controls(9 downto 6));
    o_controls(5 downto 0) <= s_controls(5 downto 0)
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity condlogic is -- Conditional logic
  port(clk, reset:       in  STD_LOGIC;
       Cond:             in  STD_LOGIC_VECTOR(3 downto 0);
       ALUFlags:         in  STD_LOGIC_VECTOR(3 downto 0);
       FlagW:            in  STD_LOGIC_VECTOR(1 downto 0);
       i_controls        in  STD_LOGIC_VECTOR(3 downto 0);
       o_controls        out STD_LOGIC_VECTOR(3 downto 0));
end;

architecture behave of condlogic is
  component flopenr
    generic(width: integer);
    port(clk, reset, en: in  STD_LOGIC;
         d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
         q:          out STD_LOGIC_VECTOR(width-1 downto 0));
  end component;
  signal FlagWrite: STD_LOGIC_VECTOR(1 downto 0);
  signal Flags:     STD_LOGIC_VECTOR(3 downto 0);
  signal CondEx:    STD_LOGIC;
begin
  flagreg1: flopenr generic map(2)
    port map(clk, reset, FlagWrite(1), 
             ALUFlags(3 downto 2), Flags(3 downto 2));
  flagreg0: flopenr generic map(2)
    port map(clk, reset, FlagWrite(0), 
             ALUFlags(1 downto 0), Flags(1 downto 0));
  cc: entity work.condcheck(behave) port map(Cond, Flags, CondEx);
  
  FlagWrite <= FlagW and (CondEx, CondEx); 
  o_controls <= i_controls and (CondEx, CondEx, '1', CondEx);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity condcheck is 
  port(Cond:           in  STD_LOGIC_VECTOR(3 downto 0);
       Flags:          in  STD_LOGIC_VECTOR(3 downto 0);
       CondEx:         out STD_LOGIC);
end;

architecture behave of condcheck is
  signal CondEx_s, neg, zero, carry, overflow, ge: STD_LOGIC;
begin
  (neg, zero, carry, overflow) <= Flags;
  ge <= (neg xnor overflow);
  
  process(all) begin -- Condition checking
    case Cond(3 downto 1) is
      when "000" => CondEx_s <= zero;
      when "001" => CondEx_s <= carry;
      when "010" => CondEx_s <= neg;
      when "011" => CondEx_s <= overflow;
      when "100" => CondEx_s <= carry and (not zero);
      when "101" => CondEx_s <= ge;
      when "110" => CondEx_s <= (not zero) and ge;
      when "111" => CondEx_s <= '1';
      when others => CondEx_s <= '-';
    end case;
  end process;
  CondEx <= CondEx_s xor Cond(0);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity regbar is
  generic(M, N: integer);
  port(clk, reset, we:             in STD_LOGIC;
       D_UC:  in  STD_LOGIC_VECTOR(M-1 downto 0);
       Q_UC:  out STD_LOGIC_VECTOR(M-1 downto 0);
       D_FD:  in  STD_LOGIC_VECTOR(N*32-1 downto 0);
       Q_FD:  out STD_LOGIC_VECTOR(N*32-1 downto 0));
end;

architecture struct of regbar is
begin
	UC: entity work.flopenr(asynchronous) generic map(M) port map (clk, reset, we, D_UC, Q_UC);
	FD:
	FOR I in 0 to N-1 generate
    REGX: entity work.flopenr(asynchronous) generic map(N) port map (clk, reset, we, D_FD(I*32+31 downto I*32), Q_FD(I*32+31 downto I*32);
	end generate;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity regfile is -- three-port register file
  port(clk:           in  STD_LOGIC;
       we3:           in  STD_LOGIC;
       ra1, ra2, wa3: in  STD_LOGIC_VECTOR(3 downto 0);
       wd3, r15:      in  STD_LOGIC_VECTOR(31 downto 0);
       rd1, rd2:      out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of regfile is
  type ramtype is array (31 downto 0) of 
    STD_LOGIC_VECTOR(31 downto 0);
  signal mem: ramtype;
begin
  process(clk) begin
    if rising_edge(clk) then
       if we3 = '1' then mem(to_integer(wa3)) <= wd3;
       end if;
    end if;
  end process;
  process(all) begin
    if (to_integer(ra1) = 15) then rd1 <= r15; 
    else rd1 <= mem(to_integer(ra1));
    end if;
    if (to_integer(ra2) = 15) then rd2 <= r15; 
    else rd2 <= mem(to_integer(ra2));
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity adder is -- adder
  port(a, b: in  STD_LOGIC_VECTOR(31 downto 0);
       y:    out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of adder is
begin
  y <= a + b;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity extend is 
  port(Instr:  in  STD_LOGIC_VECTOR(23 downto 0);
       ImmSrc: in  STD_LOGIC_VECTOR(1 downto 0);
       ExtImm: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of extend is
begin
  process(all) begin
    case ImmSrc is
      when "00"   => ExtImm <= (X"000000", Instr(7 downto 0));
      when "01"   => ExtImm <= (X"00000", Instr(11 downto 0));
      when "10"   => ExtImm <= (Instr(23), Instr(23), Instr(23), 
        Instr(23), Instr(23), Instr(23), Instr(23 downto 0), "00");
      when others => ExtImm <= X"--------";
    end case;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;  
entity flopenr is -- flip-flop with enable and asynchronous reset
  generic(width: integer);
  port(clk, reset, en: in  STD_LOGIC;
       d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
       q:          out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture asynchronous of flopenr is
begin
  process(clk, reset) begin
    if reset then q <= (others => '0');
    elsif rising_edge(clk) then
      if en then 
        q <= d;
      end if;
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;  
entity flopr is -- flip-flop with asynchronous reset
  generic(width: integer);
  port(clk, reset: in  STD_LOGIC;
       d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
       q:          out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture asynchronous of flopr is
begin
  process(clk, reset) begin
    if reset then  q <= (others => '0');
    elsif rising_edge(clk) then
      q <= d;
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity mux2 is -- two-input multiplexer
  generic(width: integer);
  port(d0, d1: in  STD_LOGIC_VECTOR(width-1 downto 0);
       s:      in  STD_LOGIC;
       y:      out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture behave of mux2 is
begin
  y <= d1 when s else d0;
end;


library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity alu is 
  port(a, b:       in  STD_LOGIC_VECTOR(31 downto 0);
       ALUControl: in  STD_LOGIC_VECTOR(1 downto 0);
       Result:     buffer STD_LOGIC_VECTOR(31 downto 0);
       ALUFlags:      out STD_LOGIC_VECTOR(3 downto 0));
end;

architecture behave of alu is
  signal condinvb: STD_LOGIC_VECTOR(31 downto 0);
  signal sum:      STD_LOGIC_VECTOR(32 downto 0);
  signal neg, zero, carry, overflow: STD_LOGIC;
begin
  condinvb <= not b when ALUControl(0) else b;
  sum <= ('0', a) + ('0', condinvb) + ALUControl(0);

  process(all) begin
    case? ALUControl(1 downto 0) is
      when "0-"   => result <= sum(31 downto 0); 
      when "10"   => result <= a and b; 
      when "11"   => result <= a or b; 
      when others => result <= (others => '-');
    end case?;
  end process;

  neg      <= Result(31);
  zero     <= '1' when (Result = 0) else '0';
  carry    <= (not ALUControl(1)) and sum(32);
  overflow <= (not ALUControl(1)) and
             (not (a(31) xor b(31) xor ALUControl(0))) and
             (a(31) xor sum(31));
  ALUFlags    <= (neg, zero, carry, overflow);
end;