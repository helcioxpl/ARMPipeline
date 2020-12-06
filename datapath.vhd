library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity datapath is  
  port(clk, reset:        in  STD_LOGIC;
       RegSrc:            in  STD_LOGIC_VECTOR(1 downto 0);
       RegWrite:          in  STD_LOGIC;
       ImmSrc:            in  STD_LOGIC_VECTOR(1 downto 0);
       ALUSrc:            in  STD_LOGIC;
       ALUControl:        in  STD_LOGIC_VECTOR(1 downto 0);
       MemtoReg:          in  STD_LOGIC;
       PCSrc:             in  STD_LOGIC;
       ALUFlags:          out STD_LOGIC_VECTOR(3 downto 0);
       PC:                buffer STD_LOGIC_VECTOR(31 downto 0);
       Instr:             in  STD_LOGIC_VECTOR(31 downto 0);
       ALUResult, WriteData: buffer STD_LOGIC_VECTOR(31 downto 0);
       ReadData:          in  STD_LOGIC_VECTOR(31 downto 0));

       StageRegWrite      in STD_LOGIC_VECTOR(3 downto 0);
       ID_EX_Reset        in STD_LOGIC);
end;

architecture struct of datapath is
  signal PCNext, PCPlus4, PCPlus8: STD_LOGIC_VECTOR(31 downto 0);
  signal ExtImm, Result:           STD_LOGIC_VECTOR(31 downto 0);
  signal SrcA, SrcB:               STD_LOGIC_VECTOR(31 downto 0);
  signal RA1, RA2:                 STD_LOGIC_VECTOR(3 downto 0);

  signal ALUControlE: std_logic_vector(1 downto 0);
  signal PCSrcE, RegWriteE, MemtoRegE, MemWriteE, BranchE, ALUSrcE, FlagWriteE: std_logic;
  signal PCSrcM, RegWriteM, MemtoRegM, MemWriteM: std_logic;
  signal PCSrcW, RegWriteW, MemtoRegW: std_logic;
begin
  EXUCReg: entity work.regbar(struct) generic map(1, 9) port map(clk, reset, write,
    PCSrc & RegWrite & MemtoReg & MemWrite & ALUControl & Branch & ALUSrc & FlagWrite,
    PCSrcE & RegWriteE & MemtoRegE & MemWriteE & ALUControlE & BranchE & ALUSrcE & FlagWriteE);
  MEMUCReg: entity work.regbar(struct) generic map(1, 4) port map(clk, reset, write,
    PCSrcE & RegWriteE & MemtoRegE & MemWriteE,
    PCSrcM & RegWriteM & MemtoRegM & MemWriteM);
  WBUCReg: entity work.regbar(struct) generic map(1, 3) port map(clk, reset, write,
    PCSrcM & RegWriteM & MemtoRegM,
    PCSrcW & RegWriteW & MemtoRegW);

  IF : entity work.IF(struct) port map(clk, reset, write, PCSrc, PC, Instr, PCPlus4, Result, Instr, InstrF);
  ID : entity work.ID(struct) port map(clk, reset, write, RegSrc, RegWrite, ImmSrc, PCPlus4, Instr, Result, ExtImm, SrcA, WriteData);
  EX : entity work.ID(struct) port map(clk, reset, write, ALUSrc, ALUControl, ALUFlags, SrcA, ExtImm, ALUResult, WriteData);
  MEM : entity work.MEM(struct) port map(clk, reset, write, PCSrc, PC, Instr, Result);
  WB : entity work.mux2(behave) generic map(32) port map(ALUResult, ReadData, MemtoReg, Result);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity IF is  
  port(clk, reset, write: in  STD_LOGIC;
       PCSrc:             in  STD_LOGIC;
       PC:                buffer STD_LOGIC_VECTOR(31 downto 0);
       PCPlus4:           out STD_LOGIC_VECTOR(31 downto 0));
       Result, InstrF:    in STD_LOGIC_VECTOR(31 downto 0);
       Instr:             out  STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of IF is
  signal PCNext, PCPlus4F: STD_LOGIC_VECTOR(31 downto 0);
begin
  pcmux: entity work.mux2(behave) generic map(32) port map(PCPlus4, Result, PCSrc, PCNext);
  pcreg: entity work.flopr(asynchronous) generic map(32) port map(clk, reset, PCNext, PC);
  pcadd1: entity work.adder(behave) port map(PC, X"00000004", PCPlus4F);

  IFReg: entity work.regbar(struct) generic map(3) port map(clk, reset, write,
    InstrF & PCPlus4F, Instr & PCPlus4);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity ID is  
  port(clk, reset, write: in  STD_LOGIC;
       RegSrc:            in  STD_LOGIC_VECTOR(1 downto 0);
       RegWrite:          in  STD_LOGIC;

       ImmSrc:            in  STD_LOGIC_VECTOR(1 downto 0);
       PCPlus4:           in  STD_LOGIC_VECTOR(31 downto 0);
       Instr:             in  STD_LOGIC_VECTOR(31 downto 0);
       Result:            in  STD_LOGIC_VECTOR(31 downto 0);

       ExtImm:            out STD_LOGIC_VECTOR(31 downto 0);
       SrcA:              out STD_LOGIC_VECTOR(31 downto 0);
       WriteData:         buffer STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of ID is
  signal PCPlus8 : STD_LOGIC_VECTOR(31 downto 0);
  signal RA1, RA2: STD_LOGIC_VECTOR(3 downto 0);
  signal RD1, RD2: STD_LOGIC_VECTOR(31 downto 0);
begin
  pcadd2: entity work.adder(behave) port map(PCPlus4, X"00000004", PCPlus8);
  ra1mux: entity work.mux2(behave) generic map (4)
    port map(Instr(19 downto 16), "1111", RegSrc(0), RA1);
  ra2mux: entity work.mux2(behave)
    generic map (4) port map(Instr(3 downto 0), 
    Instr(15 downto 12), RegSrc(1), RA2);
  rf: entity work.regfile(behave) port map(
    clk, RegWrite, RA1, RA2, 
    Instr(15 downto 12), Result, 
    PCPlus8, RD1, RD2);
  ext: entity work.extend(behave) port map(Instr(23 downto 0), ImmSrc, ExtImmD);

  IDReg: entity work.regbar(struct) generic map(32, 3) port map(clk, reset, write,
    RD1 & RD2 & ExtImmD,
    SrcA & WriteData & ExtImm);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity EX is  
  port(clk, reset, write: in  STD_LOGIC;
       ALUSrc:            in  STD_LOGIC;
       ALUControl:        in  STD_LOGIC_VECTOR(1 downto 0);
       ALUFlags:          out STD_LOGIC_VECTOR(3 downto 0);
       SrcA:              in STD_LOGIC_VECTOR(31 downto 0);
       ExtImm:            in STD_LOGIC_VECTOR(31 downto 0);
       ALUResult, WriteData: buffer STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of EX is
  signal SrcB: STD_LOGIC_VECTOR(31 downto 0);
begin
  srcbmux: mux2 generic map(32) 
    port map(WriteData, ExtImm, ALUSrc, SrcB);
  i_alu: alu port map(SrcA, SrcB, ALUControl, ALUResult, ALUFlags);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity regbar is
  generic(M: integer, N: integer);
  port(clk, reset, we:             in STD_LOGIC;
       D:  in array (N*M-1 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
       Q: out array (N*M-1 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
end;

architecture struct of regbar is
begin
	REGS:
	FOR I in 0 to N-1 generate
    REGX: entity work.reg(struct) port map (clk, reset, we, D((I+1)*M-1 downto I*M), Q((I+1)*M-1 downto I*M);
	end generate;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity reg is
  port(clk, reset:   in  STD_LOGIC;
       we:           in  STD_LOGIC;
       D:            in STD_LOGIC_VECTOR(31 downto 0);
       Q:            out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of reg is
  signal data: STD_LOGIC_VECTOR(31 downto 0);
begin
  process(clk) begin
    if rising_edge(clk) then
      if reset = '1' then data <= x"0000";
      elsif we = '1' then data <= D;
      end if;
    end if;
  end process;
  Q <= data;
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