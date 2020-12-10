---------------------------------------------------------------
-- arm_single.vhd
-- David_Harris@hmc.edu, Sarah.Harris@unlv.edu 6 March 2014
-- Single-cycle implementation of a subset of ARMv4
--
-- Compile in ModelSim at the command line with the command 
-- vcom -2008 arm_single.vhd
-- Expect plenty of simulation warnings of metavalues detected
-- run 210
-- Expect at time 205 ns a message of
-- Failure: NO ERRORS: Simulation succeeded
-- when the value 7 is written to address 100 (0x64)
---------------------------------------------------------------

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;
entity testbench is
end;

architecture test of testbench is
  component top
    port(clk, reset:          in  STD_LOGIC;
         WriteData, DatAadr:  out STD_LOGIC_VECTOR(31 downto 0);
         MemWrite:            out STD_LOGIC);
  end component;
  signal WriteData, DataAdr:     STD_LOGIC_VECTOR(31 downto 0);
  signal clk, reset,  MemWrite:  STD_LOGIC;
begin

  -- instantiate device to be tested
  dut: top port map(clk, reset, WriteData, DataAdr, MemWrite);

  -- Generate clock with 10 ns period
  process begin
    clk <= '1';
    wait for 5 ns; 
    clk <= '0';
    wait for 5 ns;
  end process;

  -- Generate reset for first two clock cycles
  process begin
    reset <= '1';
    wait for 22 ns;
    reset <= '0';
    wait;
  end process;

  -- check that 7 gets written to address 84 
  -- at end of program
  process (clk) begin
    if (clk'event and clk = '0' and MemWrite = '1') then
      if (to_integer(DataAdr) = 100 and 
          to_integer(WriteData) = 7) then 
        report "NO ERRORS: Simulation succeeded" severity failure;
      elsif (DataAdr /= 96) then 
        report "Simulation failed" severity failure;
      end if;
    end if;
  end process;
end;

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;
entity top is -- top-level design for testing
  port(clk, reset:           in     STD_LOGIC;
       WriteData, DataAdr:   buffer STD_LOGIC_VECTOR(31 downto 0);
       MemWrite:             buffer STD_LOGIC);
end;

architecture test of top is
  component arm 
    port(clk, reset:        in  STD_LOGIC;
         PC:                out STD_LOGIC_VECTOR(31 downto 0);
         Instr:             in  STD_LOGIC_VECTOR(31 downto 0);
         MemWrite:          out STD_LOGIC;
         ALUResult, WriteData: out STD_LOGIC_VECTOR(31 downto 0);
         ReadData:          in  STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component imem
    port(a:  in  STD_LOGIC_VECTOR(31 downto 0);
         rd: out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component dmem
    port(clk, we:  in STD_LOGIC;
         a, wd:    in STD_LOGIC_VECTOR(31 downto 0);
         rd:       out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  signal PC, Instr, 
         ReadData: STD_LOGIC_VECTOR(31 downto 0);
begin
  -- instantiate processor and memories
  i_arm: arm port map(clk, reset, PC, Instr, MemWrite, DataAdr, 
                       WriteData, ReadData);
  i_imem: imem port map(PC, Instr);
  i_dmem: dmem port map(clk, MemWrite, DataAdr, 
                             WriteData, ReadData);
end;

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all; 
entity dmem is -- data memory
  port(clk, we:  in STD_LOGIC;
       a, wd:    in STD_LOGIC_VECTOR(31 downto 0);
       rd:       out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of dmem is
begin
  process is
    type ramtype is array (63 downto 0) of 
                    STD_LOGIC_VECTOR(31 downto 0);
    variable mem: ramtype;
  begin -- read or write memory
    loop
      if clk'event and clk = '1' then
          if (we = '1') then 
            mem(to_integer(a(7 downto 2))) := wd;
          end if;
      end if;
      rd <= mem(to_integer(a(7 downto 2))); 
      wait on clk, a;
    end loop;
  end process;
end;

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;  
entity imem is -- instruction memory
  port(a:  in  STD_LOGIC_VECTOR(31 downto 0);
       rd: out STD_LOGIC_VECTOR(31 downto 0));
end;
architecture behave of imem is -- instruction memory
begin
  process is
    file mem_file: TEXT;
    variable L: line;
    variable ch: character;
    variable i, index, result: integer;
    type ramtype is array (63 downto 0) of 
                    STD_LOGIC_VECTOR(31 downto 0);
    variable mem: ramtype;
  begin
    -- initialize memory from file
    for i in 0 to 63 loop -- set all contents low
      mem(i) := (others => '0'); 
    end loop;
    index := 0; 
    FILE_OPEN(mem_file, "memfile.dat", READ_MODE);
    while not endfile(mem_file) loop
      readline(mem_file, L);
      result := 0;
      for i in 1 to 8 loop
        read(L, ch);
        if '0' <= ch and ch <= '9' then 
            result := character'pos(ch) - character'pos('0');
        elsif 'a' <= ch and ch <= 'f' then
           result := character'pos(ch) - character'pos('a')+10;
        elsif 'A' <= ch and ch <= 'F' then
           result := character'pos(ch) - character'pos('A')+10;
        else report "Format error on line " & integer'image(index)
             severity error;
        end if;
        mem(index)(35-i*4 downto 32-i*4) := 
          to_std_logic_vector(result,4);
      end loop;
      index := index + 1;
    end loop;

    -- read memory
    loop
      rd <= mem(to_integer(a(7 downto 2))); 
      wait on a;
    end loop;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
entity arm is -- single cycle processor
  port(clk, reset:        in  STD_LOGIC;
       PC:                out STD_LOGIC_VECTOR(31 downto 0);
       Instr:             in  STD_LOGIC_VECTOR(31 downto 0);
       MemWrite:          out STD_LOGIC;
       ALUResult, WriteData: out STD_LOGIC_VECTOR(31 downto 0);
       ReadData:          in  STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of arm is
  component controller
    port(clk, reset:        in  STD_LOGIC;
         Instr:             in  STD_LOGIC_VECTOR(31 downto 12);
         ALUFlags:          in  STD_LOGIC_VECTOR(3 downto 0);
         controls:          out STD_LOGIC_VECTOR(12 downto 0);
         Branch:            in  STD_LOGIC;
         FlagWrite:         in  STD_LOGIC);
  end component;
  component datapath
    port(clk, reset:        in  STD_LOGIC;
         controls:          in  STD_LOGIC_VECTOR(12 downto 0);

         Branch:            out STD_LOGIC;
         MemWrite:          out STD_LOGIC;
         FlagWrite:         out STD_LOGIC;

         ALUFlags:          out STD_LOGIC_VECTOR(3 downto 0);
         PC:                buffer STD_LOGIC_VECTOR(31 downto 0);
         Instr:             in  STD_LOGIC_VECTOR(31 downto 0);
         ALUResult, WriteData: buffer STD_LOGIC_VECTOR(31 downto 0);
         ReadData:          in  STD_LOGIC_VECTOR(31 downto 0));
  end component;
  signal Branch, FlagWrite: STD_LOGIC;
  signal ALUFlags: STD_LOGIC_VECTOR(3 downto 0);
begin
  cont: controller port map(
    clk, reset, Instr(31 downto 12), 
    ALUFlags, controls, Branch, FlagWrite);
  dp: datapath port map(
    clk, reset, controls, 
    Branch, MemWrite, FlagWrite, 
    ALUFlags, PC, Instr, ALUResult, 
    WriteData, ReadData);
end;