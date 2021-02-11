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

constant  g_FIFO_NUM                       : integer := 32;
constant  g_L2_FIFO_NUM                    : integer := 5;
constant  g_LEVEL_NUM                      : integer := 4;
constant  g_L2_LEVEL_NUM                   : integer := 2;
constant  g_FIFO_SIZE                      : integer := 256;
constant  g_DESC_BIT_WIDTH                 : integer := 11+16+16;
constant  g_PKT_CNT_WIDTH                  : integer := 9;
constant  g_BYTE_CNT_WIDTH                 : integer := 11+9;

constant  MAX_SIM_TIME : time := 80 ns;
constant  CLK_PERIOD   : time := 4 ns;


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

p_clk: process
  begin
    while NOW < MAX_SIM_TIME loop
      clk <= not clk;
      wait for CLK_PERIOD/2;
    end loop;
    wait;
end process p_clk;

p_main: process
  begin
    wait for 100 ps;
    rst <= '1';
    wait for CLK_PERIOD;
    rst <= '0';
    wait for CLK_PERIOD;

    -- enque testing

    -- 1. enque first flow first pkt
      -- 1.1 check last pkt enque level
      -- 1.2 check last pkt fin time  
      -- 1.3 check fin_time
      -- 1.4 check level
      -- 1.5 check set
      -- 1.6 check index

    -- 1. enqueue a desc into Gearbox with flow_id = 0
    enq_cmd                          <= '1';
    enq_desc                         <= std_logic_vector(to_unsigned(64, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(100, FIN_TIME_BIT_WIDTH)) &
                                        -- TODO: flow id = 0 &
                                        std_logic_vector(to_unsigned(1, PKT_ID_BIT_WIDTH));
                                        -- TODO: pkt transmission time = 5
    wait for CLK_PERIOD;                -- TODO: how may clk cycles we need?
    enq_cmd                          <= '0';
    
    -- @ clk 02:
    -- 1.1 check last pkt enque level
      -- Should be 0
    
    -- 1.2 check last pkt fin time
      -- signal <fin_time_arr(to_integer(flow_id))>
      -- Should be 0
    
    -- 1.3 check fin_time
      -- signal <fin_time>
      -- Should be 5
    
    -- @ clk 02:
    
    -- 1.1 check last pkt enque level
      -- Should be 0
    
    -- 1.4 check level
      -- Should be 0

    -- 1.5 check index
      -- Should be 5

    assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;
    wait for CLK_PERIOD;

    

    -- 2. enqueue a subsequent desc into Gearbox with flow_id = 0

                                        -- TODO: flow id = 0
                                        -- TODO: pkt transmission time = 10

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
    
i_gb: gearbox
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
    enq_done                         => enq_done,
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
end tb;