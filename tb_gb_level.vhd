-- Code your testbench here
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.gb_package.all;

entity gb_level_tb is
end gb_level_tb;

architecture tb of gb_level_tb is
component gearbox_level
  generic (
    g_FIFO_NUM        : integer;     -- number of FIFOs
    g_L2_FIFO_NUM     : integer;     -- log2 of number of FIFOs
    g_FIFO_SIZE       : integer;     -- size (depth) of each FIFO
    g_DESC_BIT_WIDTH  : integer;     -- Descriptor width
    g_PKT_CNT_WIDTH   : integer;     -- packet count width
    g_BYTE_CNT_WIDTH  : integer      -- byte count width
  );
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    -- enq i/f
    enq_cmd                          : in  std_logic;
    enq_fifo_index                   : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    enq_desc                         : in  std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    enq_done                         : out std_logic;
    -- deq i/f
    deq_cmd                          : in  std_logic;
    deq_fifo_index                   : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    deq_desc                         : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    deq_desc_valid                   : out std_logic;
    -- find earliest fifo i/f
    find_earliest_non_empty_fifo_cmd : in  std_logic;
    current_fifo_index               : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    find_earliest_non_empty_fifo_rsp : out std_logic;
    earliest_fifo_index              : out unsigned(g_L2_FIFO_NUM-1 downto 0);
    all_fifos_empty                  : out std_logic;
    -- fifo count i/f
    get_fifo_cnts_cmd                : in  std_logic;
    get_fifo_cnts_index              : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    get_fifo_cnts_rsp                : out std_logic;
    fifo_pkt_cnt                     : out unsigned(g_PKT_CNT_WIDTH-1 downto 0);
    fifo_byte_cnt                    : out unsigned(g_BYTE_CNT_WIDTH-1 downto 0);
    -- level count i/f (always valid)
    level_pkt_cnt                    : out unsigned(g_L2_FIFO_NUM+g_PKT_CNT_WIDTH-1 downto 0);
    level_byte_cnt                   : out unsigned(g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0)

  );
end component;

constant  g_FIFO_NUM                       : integer := 32;
constant  g_L2_FIFO_NUM                    : integer := 5;
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
signal    enq_fifo_index                   : unsigned(g_L2_FIFO_NUM-1 downto 0) := (others => '0');
signal    enq_desc                         : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0) := (others => '0');
signal    enq_done                         : std_logic := '0';
    -- deq i/f
signal    deq_cmd                          : std_logic := '0';
signal    deq_fifo_index                   : unsigned(g_L2_FIFO_NUM-1 downto 0) := (others => '0');
signal    deq_desc                         : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0) := (others => '0');
signal    deq_desc_valid                   : std_logic := '0';
    -- find earliest fifo i/f
signal    find_earliest_non_empty_fifo_cmd : std_logic := '0';
signal    current_fifo_index               : unsigned(g_L2_FIFO_NUM-1 downto 0) := (others => '0');
signal    find_earliest_non_empty_fifo_rsp : std_logic := '0';
signal    earliest_fifo_index              : unsigned(g_L2_FIFO_NUM-1 downto 0) := (others => '0');
signal    all_fifos_empty                  : std_logic := '0';
    -- fifo count i/f
signal    get_fifo_cnts_cmd                : std_logic := '0';
signal    get_fifo_cnts_index              : unsigned(g_L2_FIFO_NUM-1 downto 0) := (others => '0');
signal    get_fifo_cnts_rsp                : std_logic := '0';
signal    fifo_pkt_cnt                     : unsigned(g_PKT_CNT_WIDTH-1 downto 0) := (others => '0');
signal    fifo_byte_cnt                    : unsigned(g_BYTE_CNT_WIDTH-1 downto 0) := (others => '0');
    -- level count i/f (always valid)
signal    level_pkt_cnt                    : unsigned(g_L2_FIFO_NUM+g_PKT_CNT_WIDTH-1 downto 0) := (others => '0');
signal    level_byte_cnt                   : unsigned(g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0) := (others => '0');

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
    
    -- issue find earliest non-empty FIFO - expect all FIFOs empty
    find_earliest_non_empty_fifo_cmd <= '1';
    current_fifo_index               <= (others => '0');
    wait for CLK_PERIOD;
    find_earliest_non_empty_fifo_cmd <= '0';
    assert (find_earliest_non_empty_fifo_rsp = '1' and all_fifos_empty = '1') 
      report "find_earliest_non_empty_fifo_rsp = " &
             STD_LOGIC'IMAGE(find_earliest_non_empty_fifo_rsp) & 
             " all_fifos_empty = " & STD_LOGIC'IMAGE(all_fifos_empty) severity failure;
    wait for CLK_PERIOD;
             
    -- enqueue a desc into FIFO index 5
    enq_cmd                          <= '1';
    enq_fifo_index                   <= to_unsigned(5, enq_fifo_index'length);
    enq_desc                         <= std_logic_vector(to_unsigned(64, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(100, FIN_TIME_BIT_WIDTH)) &
                                        std_logic_vector(to_unsigned(1, PKT_ID_BIT_WIDTH));
    wait for CLK_PERIOD;
    enq_cmd                          <= '0';
    assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;
    wait for CLK_PERIOD;
    
    -- issue find earliest FIFO again - expect FIFO index 5
    find_earliest_non_empty_fifo_cmd <= '1';
    current_fifo_index               <= to_unsigned(1, current_fifo_index'length);
    wait for CLK_PERIOD;
    find_earliest_non_empty_fifo_cmd <= '0';
    assert (find_earliest_non_empty_fifo_rsp = '1' and 
            earliest_fifo_index = to_unsigned(5, earliest_fifo_index'length) and
            all_fifos_empty = '0') 
      report "find_earliest_non_empty_fifo_rsp = " &
             STD_LOGIC'IMAGE(find_earliest_non_empty_fifo_rsp) & 
             " earliest_fifo_index = " & INTEGER'IMAGE(to_integer(earliest_fifo_index)) &
             " all_fifos_empty = " & STD_LOGIC'IMAGE(all_fifos_empty) 
        severity failure;
    wait for CLK_PERIOD;

    -- check level counts - expect 1 packet and 64 bytes
    assert (level_pkt_cnt = to_unsigned(1, level_pkt_cnt'length) and
            level_byte_cnt = to_unsigned(64, level_byte_cnt'length)) 
      report "level_pkt_cnt = " & INTEGER'IMAGE(to_integer(level_pkt_cnt)) &
             " level_byte_cnt = " & INTEGER'IMAGE(to_integer(level_byte_cnt)) 
      severity failure;
 
    -- check counts for FIFO 5 - expect 1 packet and 64 bytes
    get_fifo_cnts_cmd <= '1';
    get_fifo_cnts_index <= to_unsigned(5, enq_fifo_index'length);
    wait for CLK_PERIOD;
    get_fifo_cnts_cmd <= '0';
    assert (get_fifo_cnts_rsp = '1' and 
            fifo_pkt_cnt = to_unsigned(1, fifo_pkt_cnt'length) and
            fifo_byte_cnt = to_unsigned(64, fifo_byte_cnt'length)) 
      report "get_fifo_cnts_rsp = " &
             STD_LOGIC'IMAGE(get_fifo_cnts_rsp) & 
             " fifo_pkt_cnt = " & INTEGER'IMAGE(to_integer(fifo_pkt_cnt)) &
             " fifo_byte_cnt = " & INTEGER'IMAGE(to_integer(fifo_byte_cnt)) 
      severity failure;
    wait for CLK_PERIOD;
    
    -- dequeue descriptor from FIFO 5
    deq_cmd                          <= '1';
    deq_fifo_index                   <= to_unsigned(5, deq_fifo_index'length);
    wait for CLK_PERIOD;
    deq_cmd                          <= '0';
    wait for CLK_PERIOD;
    assert (deq_desc_valid = '1' and 
            deq_desc = std_logic_vector(to_unsigned(64, PKT_LEN_BIT_WIDTH))   &
                       std_logic_vector(to_unsigned(100, FIN_TIME_BIT_WIDTH))   &
                       std_logic_vector(to_unsigned(1, PKT_ID_BIT_WIDTH)))
      report " deq_desc_valid = " &
             STD_LOGIC'IMAGE(deq_desc_valid) & 
             " deq_desc = 0x" & to_hstring(deq_desc)
      severity failure;
    wait for CLK_PERIOD;
    
    -- check earliest non-empty FIFO - expect FIFOs are all empty
    find_earliest_non_empty_fifo_cmd <= '1';
    wait for CLK_PERIOD;
    find_earliest_non_empty_fifo_cmd <= '0';
    assert (find_earliest_non_empty_fifo_rsp = '1' and all_fifos_empty = '1') 
      report "find_earliest_non_empty_fifo_rsp = " &
             STD_LOGIC'IMAGE(find_earliest_non_empty_fifo_rsp) & 
             " all_fifos_empty = " & STD_LOGIC'IMAGE(all_fifos_empty) severity failure;
    wait for CLK_PERIOD;


    -- check level counts - expect all counts are zero
    assert (level_pkt_cnt = to_unsigned(0, level_pkt_cnt'length) and
            level_byte_cnt = to_unsigned(0, level_byte_cnt'length)) 
      report "level_pkt_cnt = " & INTEGER'IMAGE(to_integer(level_pkt_cnt)) &
             " level_byte_cnt = " & INTEGER'IMAGE(to_integer(level_byte_cnt)) 
      severity failure;

    -- check counts for FIFO 5 - expect zero counts
    get_fifo_cnts_cmd <= '1';
    get_fifo_cnts_index <= to_unsigned(5, enq_fifo_index'length);
    wait for CLK_PERIOD;
    get_fifo_cnts_cmd <= '0';
    assert (get_fifo_cnts_rsp = '1' and 
            fifo_pkt_cnt = to_unsigned(0, fifo_pkt_cnt'length) and
            fifo_byte_cnt = to_unsigned(0, fifo_byte_cnt'length)) 
      report "get_fifo_cnts_rsp = " &
             STD_LOGIC'IMAGE(get_fifo_cnts_rsp) & 
             " fifo_pkt_cnt = " & INTEGER'IMAGE(to_integer(fifo_pkt_cnt)) &
             " fifo_byte_cnt = " & INTEGER'IMAGE(to_integer(fifo_byte_cnt)) 
      severity failure;
    wait for CLK_PERIOD;
    wait;
end process p_main;
    
i_gb_level: gearbox_level
  generic map(
    g_FIFO_NUM        => g_FIFO_NUM,       -- number of FIFOs
    g_L2_FIFO_NUM     => g_L2_FIFO_NUM,    -- log2 of number of FIFOs
    g_FIFO_SIZE       => g_FIFO_SIZE,      -- size (depth) of each FIFO
    g_DESC_BIT_WIDTH  => g_DESC_BIT_WIDTH, -- Descriptor width
    g_PKT_CNT_WIDTH   => g_PKT_CNT_WIDTH,  -- packet count width
    g_BYTE_CNT_WIDTH  => g_BYTE_CNT_WIDTH  -- byte count width
  )
  port map (
    rst                              => rst,
    clk                              => clk,
    -- enq i/f
    enq_cmd                          => enq_cmd,
    enq_fifo_index                   => enq_fifo_index,
    enq_desc                         => enq_desc,
    enq_done                         => enq_done,
    -- deq i/f
    deq_cmd                          => deq_cmd,
    deq_fifo_index                   => deq_fifo_index,
    deq_desc                         => deq_desc,
    deq_desc_valid                   => deq_desc_valid,
    -- find earliest fifo i/f
    find_earliest_non_empty_fifo_cmd => find_earliest_non_empty_fifo_cmd,
    current_fifo_index               => current_fifo_index,
    find_earliest_non_empty_fifo_rsp => find_earliest_non_empty_fifo_rsp,
    earliest_fifo_index              => earliest_fifo_index,
    all_fifos_empty                  => all_fifos_empty,
    -- fifo count i/f
    get_fifo_cnts_cmd                => get_fifo_cnts_cmd,
    get_fifo_cnts_index              => get_fifo_cnts_index,
    get_fifo_cnts_rsp                => get_fifo_cnts_rsp,
    fifo_pkt_cnt                     => fifo_pkt_cnt,
    fifo_byte_cnt                    => fifo_byte_cnt,
    -- level count i/f (always valid)
    level_pkt_cnt                    => level_pkt_cnt,
    level_byte_cnt                   => level_byte_cnt

  );
end tb;