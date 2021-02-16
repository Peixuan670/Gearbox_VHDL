-- Code your testbench here
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.gb_package.all;

entity gearboxI_tb is
end gearboxI_tb;

architecture tb of gearboxI_tb is
component gearbox_I
  generic (
    g_FLOW_NUM        : integer;     -- number of flow
    g_L2_FLOW_NUM     : integer;     -- log2 of number of flows
    g_LEVEL_NUM       : integer;     -- number of levels
    g_L2_LEVEL_NUM    : integer;     -- log2 of number of levels
    g_FIFO_NUM        : integer;     -- number of FIFOs
    g_L2_FIFO_NUM     : integer;     -- log2 of number of FIFOs
    g_FIFO_SIZE       : integer;     -- size (depth) of each FIFO
    g_DESC_BIT_WIDTH  : integer;     -- Descriptor width
    g_VC_BIT_WIDTH    : integer;     -- number of bits in VC
    g_PKT_CNT_WIDTH   : integer;     -- packet count width
    g_BYTE_CNT_WIDTH  : integer      -- byte count width
  );
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    -- enq i/f
    enq_cmd                          : in  std_logic;
    enq_desc                         : in  std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    -- deq i/f
    deq_cmd                          : in  std_logic;
    deq_desc                         : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    deq_desc_valid                   : out std_logic;
    -- drop i/f
    drop_cmd                         : out std_logic;
    drop_desc                        : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    -- pkt count i/f
    gb_pkt_cnt                       : out unsigned(g_L2_LEVEL_NUM+g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0)

  );
end component;


constant  g_FLOW_NUM                       : integer := 4;     -- number of flow
constant  g_L2_FLOW_NUM                    : integer := 2;     -- log2 of number of flows
constant  g_FIFO_NUM                       : integer := 16;
constant  g_L2_FIFO_NUM                    : integer := 4;
constant  g_LEVEL_NUM                      : integer := 4;
constant  g_L2_LEVEL_NUM                   : integer := 2;
constant  g_FIFO_SIZE                      : integer := 256;
constant  g_DESC_BIT_WIDTH                 : integer := 11+20+2+16;
constant  g_VC_BIT_WIDTH                   : integer := 20;
constant  g_PKT_CNT_WIDTH                  : integer := 9;
constant  g_BYTE_CNT_WIDTH                 : integer := 11+9;

constant  MAX_SIM_TIME : time := 80 ns;
constant  CLK_PERIOD   : time := 4 ns;
constant  LOGIC_DELAY  : time := 100 ps;

signal    rst                              : std_logic := '0';
signal    clk                              : std_logic := '0';
    -- enq i/f
signal    enq_cmd                          : std_logic := '0';
signal    enq_desc                         : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0) := (others => '0');
signal    enq_done                         : std_logic := '0';
    -- deq i/f
signal    deq_cmd                          : std_logic := '0';
signal    deq_desc                         : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0) := (others => '0');
signal    deq_desc_valid                   : std_logic := '0';
    -- drop i/f
signal    drop_cmd                         : std_logic := '0';
signal    drop_desc                        : std_logic_vector(DESC_BIT_WIDTH-1 downto 0) := (others => '0');
    -- pkt count i/f
signal    gb_pkt_cnt                       : unsigned(g_L2_LEVEL_NUM+g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0) := (others => '0');

begin

i_gb: gearbox_I
  generic map(
    g_FLOW_NUM        => g_FLOW_NUM,       -- number of flows
    g_L2_FLOW_NUM     => g_L2_FLOW_NUM,    -- log2 of number of flows
    g_LEVEL_NUM       => g_LEVEL_NUM,      -- number of levels
    g_L2_LEVEL_NUM    => g_L2_LEVEL_NUM,   -- log2 of number of levels
    g_FIFO_NUM        => g_FIFO_NUM,       -- number of FIFOs
    g_L2_FIFO_NUM     => g_L2_FIFO_NUM,    -- log2 of number of FIFOs
    g_FIFO_SIZE       => g_FIFO_SIZE,      -- size (depth) of each FIFO
    g_DESC_BIT_WIDTH  => g_DESC_BIT_WIDTH, -- Descriptor width
    g_VC_BIT_WIDTH    => g_VC_BIT_WIDTH,   -- number of bits in VC
    g_PKT_CNT_WIDTH   => g_PKT_CNT_WIDTH,  -- packet count width
    g_BYTE_CNT_WIDTH  => g_BYTE_CNT_WIDTH  -- byte count width
  )
  port map (
    rst                              => rst,
    clk                              => clk,
    -- enq i/f
    enq_cmd                          => enq_cmd,
    enq_desc                         => enq_desc,
    --enq_done                         => enq_done,
    -- deq i/f
    deq_cmd                          => deq_cmd,
    deq_desc                         => deq_desc,
    deq_desc_valid                   => deq_desc_valid,
    -- drop i/f
    drop_cmd                         => drop_cmd,
    drop_desc                        => drop_desc,
    -- pkt count i/f
    gb_pkt_cnt                       => gb_pkt_cnt
  );
  

p_clk: process
  begin
    while NOW < MAX_SIM_TIME loop
      clk <= not clk;
      wait for CLK_PERIOD/2;
    end loop;
    wait;
end process p_clk;

p_main: process
alias gb_lvl_enq_cmd_A is << signal i_gb.lvl_enq_cmd_A : std_logic_vector(g_LEVEL_NUM-1 downto 0) >> ;     -- enq level index set A
alias gb_lvl_enq_cmd_B is << signal i_gb.lvl_enq_cmd_B : std_logic_vector(g_LEVEL_NUM-2 downto 0) >> ;     -- enq level index set B
alias gb_lvl_enq_fifo_index is << signal i_gb.lvl_enq_fifo_index : unsigned(g_L2_FIFO_NUM-1 downto 0) >> ; -- enq fifo index
--alias gb_last_enq_level_arr is << signal i_gb.last_enq_level_arr: t_last_enq_level_arr >> ; --[TODO] how to init, need import type?  -- last enque level array
--alias gb_fin_time_arr is << signal i_gb.fin_time_arr : t_fin_time_arr; >> ; --[TODO] how to init, need import type?  -- last fin_time array
alias gb_fin_time is << signal i_gb.fin_time : unsigned(FIN_TIME_BIT_WIDTH-1 downto 0) >> ;    -- fin time of current pkt
alias gb_enq_level is << signal i_gb.enq_level : unsigned(g_L2_LEVEL_NUM-1 downto 0) >> ;    -- enq_level of current pkt
--alias gb_v_enq_level is << variable i_gb.p_enqueue.v_enq_level : unsigned(g_L2_LEVEL_NUM-1 downto 0) >> ;    -- v_enq_level of current pkt (final enque level after comparison)


  begin

    wait until rising_edge(clk);
    rst <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);
    rst <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);

    -- enque testing

    -- 1. enqueue a desc into Gearbox with flow_id = 0
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(5, FIN_TIME_BIT_WIDTH))   &  -- TODO: pkt transmission time = 5
                                        std_logic_vector(to_unsigned(0, g_L2_FLOW_NUM))        &  -- TODO: flow id = 0
                                        std_logic_vector(to_unsigned(1, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
                                        
    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 5
    assert (to_integer(gb_fin_time) = 5) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
    
    
    wait until rising_edge(clk);
    -- @ (after) clk 03:
    -- Check final enque level (v_enq_level)
    -- Final enque level = 0
    --assert (to_integer(gb_v_enq_level) = 0) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0001") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "000") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 5
    assert (to_integer(gb_lvl_enq_fifo_index) = 5) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;
    wait until rising_edge(clk);

    

    -- 2. enqueue a subsequent desc into Gearbox with flow_id = 0
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(12, FIN_TIME_BIT_WIDTH))  &  -- TODO: pkt transmission time = 12
                                        std_logic_vector(to_unsigned(0, g_L2_FLOW_NUM))        &  -- TODO: flow id = 0
                                        std_logic_vector(to_unsigned(1, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 17
    assert (to_integer(gb_fin_time) = 17) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
                    
    wait until rising_edge(clk);
    -- @ (after) clk 03:
    -- Check final enque level (v_enq_level)
    -- Final enque level = 1
    --assert (to_integer(gb_v_enq_level) = 1) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0010") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "0000") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 1
    assert (to_integer(gb_lvl_enq_fifo_index) = 1) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;

    wait for CLK_PERIOD;

    -- 2.1 check last pkt enque level
      -- Should be 0
     
    -- 2.2 check last pkt fin time
      -- Should be 5
     
    -- 2.3 check fin_time
      -- Should be 15
     
    -- 2.4 check level
      -- Should be 1
 
    -- 2.5 check index
      -- Should be 1
    

    -- 3. enqueue a desc into Gearbox with flow_id = 1
    
                                        -- TODO: flow id = 1
                                        -- TODO: pkt transmission time = 6
    
    -- 3.1 check last pkt enque level
      -- Should be 0
    
    -- 3.2 check last pkt fin time
      -- Should be 0
    
    -- 3.3 check fin_time
      -- Should be 6
    
    -- 3.4 check level
      -- Should be 0

    -- 3.5 check index
      -- Should be 6


    -- 4. enqueue a subsequent desc into Gearbox with flow_id = 0
                                        -- TODO: flow id = 0
                                        -- TODO: pkt transmission time = 2
    
    -- 4.1 check last pkt enque level
      -- Should be 1
     
    -- 4.2 check last pkt fin time
      -- Should be 15
     
    -- 4.3 check fin_time
      -- Should be 17
     
    -- 4.4 check level
      -- Should be 1
 
    -- 4.5 check index
      -- Should be 1

    
    wait;
end process p_main;
    
 

end tb;