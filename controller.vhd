library IEEE; use IEEE.STD_LOGIC_1164.all;
entity controller is -- single cycle control decoder
  port(clk, reset:        in  STD_LOGIC;
       Stall:             in  STD_LOGIC;
       Instr:             in  STD_LOGIC_VECTOR(31 downto 12);
       ALUFlags:          in  STD_LOGIC_VECTOR(3 downto 0);
       RegSrc:            out STD_LOGIC_VECTOR(1 downto 0);
       RegWrite:          out STD_LOGIC;
       ImmSrc:            out STD_LOGIC_VECTOR(1 downto 0);
       ALUSrc:            out STD_LOGIC;
       ALUControl:        out STD_LOGIC_VECTOR(1 downto 0);
       MemWrite:          out STD_LOGIC;
       MemtoReg:          out STD_LOGIC;
       PCSrc:             out STD_LOGIC;

       PCWrite            out STD_LOGIC;
       IF_ID_Write        out STD_LOGIC;
       ID_EX_Write        out STD_LOGIC;
       ID_EX_Reset        out STD_LOGIC;
       EX_MEM_Write       out STD_LOGIC;
       MEM_WB_Write       out STD_LOGIC);
end;

architecture struct of controller is
  component decoder
    port(Op:               in  STD_LOGIC_VECTOR(1 downto 0);
         Funct:            in  STD_LOGIC_VECTOR(5 downto 0);
         Rd:               in  STD_LOGIC_VECTOR(3 downto 0);
         FlagW:            out STD_LOGIC_VECTOR(1 downto 0);
         PCS, RegW, MemW:  out STD_LOGIC;
         MemtoReg, ALUSrc: out STD_LOGIC;
         ImmSrc, RegSrc:   out STD_LOGIC_VECTOR(1 downto 0);
         ALUControl:       out STD_LOGIC_VECTOR(1 downto 0));
  end component;
  component condlogic
    port(clk, reset:       in  STD_LOGIC;
         Cond:             in  STD_LOGIC_VECTOR(3 downto 0);
         ALUFlags:         in  STD_LOGIC_VECTOR(3 downto 0);
         FlagW:            in  STD_LOGIC_VECTOR(1 downto 0);
         PCS, RegW, MemW:  in  STD_LOGIC;
         PCSrc, RegWrite:  out STD_LOGIC;
         MemWrite:         out STD_LOGIC);
  end component;
  signal FlagW: STD_LOGIC_VECTOR(1 downto 0);
  signal PCS, RegW, MemW: STD_LOGIC;

  signal PCWrite_s, IF_ID_Write_s, ID_EX_Write_s: STD_LOGIC;
begin
  dec: decoder port map(Instr(27 downto 26), Instr(25 downto 20),
                       Instr(15 downto 12), FlagW, PCS, 
                       RegW, MemW, MemtoReg, ALUSrc, ImmSrc, 
                       RegSrc, ALUControl);
  cl: condlogic port map(clk, reset, Instr(31 downto 28), 
                         ALUFlags, FlagW, PCS, RegW, MemW,
                         PCSrc, RegWrite, MemWrite);

   ID_EX_Write <= ID_EX_Write_s;
   ID_EX_Reset <= ID_EX_Write_s and Stall;

   PCWrite <= PCWrite_s and not Stall;
   IF_ID_Write <= IF_ID_Write_s and not Stall;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity decoder is -- main control decoder
  port(Op:               in  STD_LOGIC_VECTOR(1 downto 0);
       Funct:            in  STD_LOGIC_VECTOR(5 downto 0);
       Rd:               in  STD_LOGIC_VECTOR(3 downto 0);
       FlagW:            out STD_LOGIC_VECTOR(1 downto 0);
       PCS, RegW, MemW:  out STD_LOGIC;
       MemtoReg, ALUSrc: out STD_LOGIC;
       ImmSrc, RegSrc:   out STD_LOGIC_VECTOR(1 downto 0);
       ALUControl:       out STD_LOGIC_VECTOR(1 downto 0));
end;

architecture behave of decoder is
  signal controls:      STD_LOGIC_VECTOR(9 downto 0);
  signal ALUOp, Branch: STD_LOGIC;
  signal op2:           STD_LOGIC_VECTOR(3 downto 0);
begin
  op2 <= (Op, Funct(5), Funct(0));
  process(all) begin -- Main Decoder
    case? (op2) is
      when "000-" => controls <= "0000001001";
      when "001-" => controls <= "0000101001";
      when "01-0" => controls <= "1001110100";
      when "01-1" => controls <= "0001111000";
      when "10--" => controls <= "0110100010";
      when others => controls <= "----------";
    end case?;
  end process;

  (RegSrc, ImmSrc, ALUSrc, MemtoReg, RegW, MemW, 
    Branch, ALUOp) <= controls;
    
  process(all) begin -- ALU Decoder
    if (ALUOp) then
      case Funct(4 downto 1) is
        when "0100" => ALUControl <= "00"; -- ADD
        when "0010" => ALUControl <= "01"; -- SUB
        when "0000" => ALUControl <= "10"; -- AND
        when "1100" => ALUControl <= "11"; -- ORR
        when others => ALUControl <= "--"; -- unimplemented
      end case;
      FlagW(1) <= Funct(0);
      FlagW(0) <= Funct(0) and (not ALUControl(1));
    else 
      ALUControl <= "00";
      FlagW <= "00";
    end if;
  end process;
  
  PCS <= ((and Rd) and RegW) or Branch;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity condlogic is -- Conditional logic
  port(clk, reset:       in  STD_LOGIC;
       Cond:             in  STD_LOGIC_VECTOR(3 downto 0);
       ALUFlags:         in  STD_LOGIC_VECTOR(3 downto 0);
       FlagW:            in  STD_LOGIC_VECTOR(1 downto 0);
       PCS, RegW, MemW:  in  STD_LOGIC;
       PCSrc, RegWrite:  out STD_LOGIC;
       MemWrite:         out STD_LOGIC);
end;

architecture behave of condlogic is
  component condcheck
    port(Cond:           in  STD_LOGIC_VECTOR(3 downto 0);
         Flags:          in  STD_LOGIC_VECTOR(3 downto 0);
         CondEx:         out STD_LOGIC);
  end component;
  component flopenr generic(width: integer);
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
  cc: condcheck port map(Cond, Flags, CondEx);
  
  FlagWrite <= FlagW and (CondEx, CondEx); 
  RegWrite  <= RegW  and CondEx;
  MemWrite  <= MemW  and CondEx;
  PCSrc     <= PCS   and CondEx;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity condcheck is 
  port(Cond:           in  STD_LOGIC_VECTOR(3 downto 0);
       Flags:          in  STD_LOGIC_VECTOR(3 downto 0);
       CondEx:         out STD_LOGIC);
end;

architecture behave of condcheck is
  signal neg, zero, carry, overflow, ge: STD_LOGIC;
begin
  (neg, zero, carry, overflow) <= Flags;
  ge <= (neg xnor overflow);
  
  process(all) begin -- Condition checking
    case Cond is
      when "0000" => CondEx <= zero;
      when "0001" => CondEx <= not zero;
      when "0010" => CondEx <= carry;
      when "0011" => CondEx <= not carry;
      when "0100" => CondEx <= neg;
      when "0101" => CondEx <= not neg;
      when "0110" => CondEx <= overflow;
      when "0111" => CondEx <= not overflow;
      when "1000" => CondEx <= carry and (not zero);
      when "1001" => CondEx <= not(carry and (not zero));
      when "1010" => CondEx <= ge;
      when "1011" => CondEx <= not ge;
      when "1100" => CondEx <= (not zero) and ge;
      when "1101" => CondEx <= not ((not zero) and ge);
      when "1110" => CondEx <= '1';
      when others => CondEx <= '-';
    end case;
  end process;
end;