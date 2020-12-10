library IEEE; use IEEE.STD_LOGIC_1164.all;
entity controller is -- single cycle control decoder
  port(clk, reset:        in  STD_LOGIC;
       Instr:             in  STD_LOGIC_VECTOR(31 downto 12);
       controls:          out STD_LOGIC_VECTOR(12 downto 0));
end;

architecture struct of controller is
  signal PCS, RegW, MemW: STD_LOGIC;
  signal control_s:  STD_LOGIC_VECTOR(9 downto 0);
  signal op2:        STD_LOGIC_VECTOR(3 downto 0);
  signal FlagW:      STD_LOGIC_VECTOR(1 downto 0);
  signal ALUControl: STD_LOGIC_VECTOR(1 downto 0);
begin
  (Op, Funct) <= Instr(27 downto 20);
  op2 <= (Op, Funct(5), Funct(0));
  Rd <= Instr(15 downto 12);

  -- PCSource, RegW, MemtoReg, MemW, Branch, FlagW, ALUControl, ALUSrc, RegSrc, ImmSrc
  process(all) begin -- Main Decoder
    case? (op2) is
      when "000-" => control_s <= "1000000001";
      when "001-" => control_s <= "1000100001";
      when "01-0" => control_s <= "0110110010";
      when "01-1" => control_s <= "1100100010";
      when "10--" => control_s <= "0001101100";
      when others => control_s <= "----------";
    end case?;
  end process;

  --     (((and Rd) and RegW) or Branch)
  PCS <= (((and Rd) and control_s(0)) or control_s(4);
  controls <= PCS & control_s(9 downto 6) & FlagW & ALUControl & control_s(5 downto 1);

  process(all) begin -- ALU Decoder
    if (control_s(0)) then
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
end;