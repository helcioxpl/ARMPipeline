library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity datapath is  
  port(clk, reset:           in  STD_LOGIC;
       RegSrc:               in  STD_LOGIC_VECTOR(1 downto 0);
       RegWrite:             in  STD_LOGIC;
       ImmSrc:               in  STD_LOGIC_VECTOR(1 downto 0);
       ALUSrc:               in  STD_LOGIC;
       ALUControl:           in  STD_LOGIC_VECTOR(1 downto 0);
       MemtoReg:             in  STD_LOGIC;
       PCSrc:                in  STD_LOGIC;
       ALUFlags:             out STD_LOGIC_VECTOR(3 downto 0);
       PC:                   buffer STD_LOGIC_VECTOR(31 downto 0);
       Instr:                in  STD_LOGIC_VECTOR(31 downto 0);
       ALUResult, WriteData: buffer STD_LOGIC_VECTOR(31 downto 0);
       ReadData:             in  STD_LOGIC_VECTOR(31 downto 0);

       StageWE:              in STD_LOGIC_VECTOR(3 downto 0);
       FlushE, FlushD:  in STD_LOGIC);
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

  signal ALUResultW, ReadDataW, ResultW: std_logic_vector(21 downto 0);
  signal WA3E, WA3M, WA3W: std_logic_vector(3 downto 0);
begin
  IF : entity work.IF_FD(struct) port map(clk, reset, StageWE(0), PCSrc, PC, PCPlus4, Result);
  ID : entity work.ID_FD(struct) port map(clk, reset, FlushD, StageWE(1), RegSrc, RegWriteW, WA3W, ImmSrc, PCPlus4, Instr, ResultW, ExtImm, SrcA, WriteData);

  EX : entity work.EX_FD(struct) port map(clk, reset, FlushE, ALUSrcE, ALUControlE, ALUFlags, SrcA, ExtImm, ALUResult, WriteData);
  EX_PTReg: entity work.regbar(struct) generic map(1, 13) port map(clk, reset, StageWE(2),
    PCSrc & RegWrite & MemtoReg & MemWrite & ALUControl & Branch & ALUSrc & FlagWrite & Instr(15 downto 12),
    PCSrcE & RegWriteE & MemtoRegE & MemWriteE & ALUControlE & BranchE & ALUSrcE & FlagWriteE & WA3E);

  MEM : entity work.regbar(struct) generic map(1, 34) port map(clk, reset, StageWE(3),
    ReadData & RegWriteM & MemtoRegM, ReadDataW & RegWriteW & MemtoRegW);
  MEM_PTReg: entity work.regbar(struct) generic map(1, 8) port map(clk, reset, StageWE(3),
    PCSrcE & RegWriteE & MemtoRegE & MemWriteE & WA3E, PCSrcM & RegWriteM & MemtoRegM & MemWriteM & WA3M);

  WB : entity work.mux2(behave) generic map(32) port map(ALUResultW, ReadDataW, MemtoRegW, ResultW);
  WB_PTReg: entity work.regbar(struct) generic map(1, 7) port map(clk, reset, StageWE(4),
    PCSrcM & RegWriteM & MemtoRegM & WA3M,    PCSrcW & RegWriteW & MemtoRegW & WA3W);
  WBReg : entity work.mux2(behave) generic map(2, 32) port map(clk, reset, StageWE(4),
    ALUResultM & ReadDataM, ALUResultW & ReadDataW);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity IF_FD is  
  port(clk, reset, write: in  STD_LOGIC;
       PCSrc:             in  STD_LOGIC;
       PC:                buffer STD_LOGIC_VECTOR(31 downto 0);
       PCPlus4:           buffer STD_LOGIC_VECTOR(31 downto 0));
       Result:            in STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of IF_FD is
  signal PCNext, PCF: STD_LOGIC_VECTOR(31 downto 0);
begin
  pcmux: entity work.mux2(behave) generic map(32) port map(PCPlus4, Result, PCSrc, PCNext);
  pcreg: entity work.flopr(asynchronous) generic map(32) port map(clk, reset, PCNext, PCF);
  IFReg: entity work.reg(behave) port map(clk, reset, write, PCF, PC);
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

       ExtImm:            out STD_LOGIC_VECTOR(31 downto 0);
       SrcA:              out STD_LOGIC_VECTOR(31 downto 0);
       WriteData:         buffer STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of ID_FD is
  signal PCPlus8 : STD_LOGIC_VECTOR(31 downto 0);
  signal RA1, RA2: STD_LOGIC_VECTOR(3 downto 0);
  signal InstrD  : STD_LOGIC_VECTOR(31 downto 0);
begin
  IDReg: entity work.reg(behave) port map(clk, IDRegClr, IDRegWE, Instr, InstrD);

  ra1mux: entity work.mux2(behave) generic map (4)
    port map(InstrD(19 downto 16), "1111", RegSrc(0), RA1);
  ra2mux: entity work.mux2(behave)
    generic map (4) port map(InstrD(3 downto 0), 
    InstrD(15 downto 12), RegSrc(1), RA2);
  rf: entity work.regfile(behave) port map(
    clk, RegWrite, RA1, RA2, WA3,
    Result, PCPlus8D, SrcA, WriteData);
  ext: entity work.extend(behave) port map(InstrD(23 downto 0), ImmSrc, ExtImmD);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
entity EX_FD is  
  port(clk, reset, clear: in  STD_LOGIC;
       ALUSrc:            in  STD_LOGIC;
       ALUControl:        in  STD_LOGIC_VECTOR(1 downto 0);
       ALUFlags:          out STD_LOGIC_VECTOR(3 downto 0);
       SrcA:              in STD_LOGIC_VECTOR(31 downto 0);
       ExtImm:            in STD_LOGIC_VECTOR(31 downto 0);
       ALUResult, WriteData: buffer STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of EX_FD is
  signal SrcB: STD_LOGIC_VECTOR(31 downto 0);
begin
  EXReg: entity work.regbar(struct) generic map(3, 32) port map(clk, clear, '1',
    RD1 & RD2 & ExtImmD, SrcA & WriteData & ExtImm);

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