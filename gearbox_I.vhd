-- gearboxI module
-- TO DO:
--   packet count? enq and deq separate?
--   fifo fill level?

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.math_real."ceil";
use IEEE.math_real."log2";
use work.gb_package.all;

entity gearbox_I is
  generic (
    g_FIFO_NUM        : integer;     -- number of FIFOs
    g_L2_FIFO_NUM     : integer;     -- log2 of number of FIFOs
    g_FIFO_SIZE       : integer;     -- size (depth) of each FIFO
    g_DESC_BIT_WIDTH  : integer;     -- Descriptor width
    g_VC_BIT_WIDTH    : integer;     -- number of bits in VC
    g_L2_GRANULARITY  : integer      -- log2 of granularity
  );
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    vc                               : in  unsigned(g_VC_BIT_WIDTH-1 downto 0);
    enq_cmd                          : in  std_logic;
    enq_desc                         : in  std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    enq_fifo_index                   : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    deq_cmd                          : in  std_logic;
    deq_fifo_index                   : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    deq_desc                         : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    deq_desc_valid                   : out std_logic;
    drop_cmd                         : out std_logic;
    drop_desc                        : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    find_earliest_non_empty_fifo_cmd : in  std_logic;
    find_earliest_non_empty_fifo_rsp : out std_logic;
    earliest_fifo_index              : out unsigned(g_L2_FIFO_NUM-1 downto 0);
    all_fifos_empty                  : out std_logic;
    current_fifo_index               : out unsigned(g_L2_FIFO_NUM-1 downto 0);

    pkt_cnt                          : out unsigned(g_L2_FIFO_NUM-1 downto 0) -- [Peixuan TODO] what should be the pkt_cnt's bit width?

  );
end gearbox_I;

architecture gearbox_I_arch of gearbox_I is

  constant COUNT_BIT_WIDTH : positive := positive(ceil(log2(real(g_FIFO_SIZE)))) + 1;     -- [Peixuan TODO] What's this? pkt cnt bitwidth?

  signal data_valid        : std_logic_vector(g_FIFO_NUM-1 downto 0);
  type   t_dout_array is array(0 to g_FIFO_NUM-1) of std_logic_vector(DESC_BIT_WIDTH-1 downto 0);   -- [Peixuan TODO] What's this t_dout?
  signal dout              : t_dout_array;                                -- [Peixuan TODO]: data out?
  signal empty             : std_logic_vector(g_FIFO_NUM-1 downto 0);     -- FIFO empty indicator arr
  signal full              : std_logic_vector(g_FIFO_NUM-1 downto 0);     -- FIFO full indicator arr
  type   t_count_array is array(0 to g_FIFO_NUM-1) of std_logic_vector(COUNT_BIT_WIDTH-1 downto 0); -- FIFO pkt_cnt arr
  signal rd_data_count     : t_count_array;
  signal wr_data_count     : t_count_array;
  signal din               : std_logic_vector(DESC_BIT_WIDTH-1 downto 0); -- [Peixuan TODO]: data in?
  signal rd_en             : std_logic_vector(g_FIFO_NUM-1 downto 0);
  signal wr_en             : std_logic_vector(g_FIFO_NUM-1 downto 0);
  signal cur_fifo_index    : unsigned(g_L2_FIFO_NUM-1 downto 0);
  signal fifo_index_offset : unsigned(g_L2_FIFO_NUM-1 downto 0);
  signal pkt_valid         : boolean;
  signal enq_cmd_d1        : std_logic;   -- [Peixuan TODO]: what's this? what's d1?
  signal enq_done          : std_logic;
  signal pkt_invalid_cnt   : unsigned(15 downto 0); -- [Peixuan TODO]: what's this?
  signal enq_fifo_index_d1 : unsigned(g_L2_FIFO_NUM-1 downto 0);
  signal deq_fifo_index_d1 : unsigned(g_L2_FIFO_NUM-1 downto 0);

begin

  -- Instantiate FIFOs  
  g_GENERATE_FIFOS: for i in 0 to g_FIFO_NUM-1 generate

    -- xpm_fifo_sync: Synchronous FIFO
    -- Xilinx Parameterized Macro, version 2020.1

    xpm_fifo_sync_inst : xpm_fifo_sync
    generic map (
      DOUT_RESET_VALUE    => "0",                  -- String
      ECC_MODE            => "no_ecc",             -- String
      FIFO_MEMORY_TYPE    => "auto",               -- String
      FIFO_READ_LATENCY   => 1,                    -- DECIMAL
      FIFO_WRITE_DEPTH    => g_FIFO_SIZE,          -- DECIMAL
      FULL_RESET_VALUE    => 0,                    -- DECIMAL
      PROG_EMPTY_THRESH   => 10,                   -- DECIMAL
      PROG_FULL_THRESH    => 10,                   -- DECIMAL
      RD_DATA_COUNT_WIDTH => count_width,          -- DECIMAL
      READ_DATA_WIDTH     => DESC_BIT_WIDTH,       -- DECIMAL
      READ_MODE           => "std",                -- String
      SIM_ASSERT_CHK      => 0,                    -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
      USE_ADV_FEATURES    => "1404",               -- String
      WAKEUP_TIME         => 0,                    -- DECIMAL
      WRITE_DATA_WIDTH    => DESC_BIT_WIDTH,       -- DECIMAL
      WR_DATA_COUNT_WIDTH => COUNT_BIT_WIDTH       -- DECIMAL
    )
    port map (
      -- [Peixuan TODO]: what's open? Is these for the FIFO module?
      almost_empty  => open,             -- 1-bit output: Almost Empty : When asserted, this signal indicates that
                                         -- only one more read can be performed before the FIFO goes to empty.

      almost_full   => open,             -- 1-bit output: Almost Full: When asserted, this signal indicates that
                                         -- only one more write can be performed before the FIFO is full.

      data_valid    => data_valid(i),    -- 1-bit output: Read Data Valid: When asserted, this signal indicates
                                         -- that valid data is available on the output bus (dout).

      dbiterr       => open,             -- 1-bit output: Double Bit Error: Indicates that the ECC decoder
                                         -- detected a double-bit error and data in the FIFO core is corrupted.

      dout          => dout(i),          -- READ_DATA_WIDTH-bit output: Read Data: The output data bus is driven
                                         -- when reading the FIFO.

      empty         => empty(i),         -- 1-bit output: Empty Flag: When asserted, this signal indicates that
                                         -- the FIFO is empty. Read requests are ignored when the FIFO is empty,
                                         -- initiating a read while empty is not destructive to the FIFO.

      full          => full(i),          -- 1-bit output: Full Flag: When asserted, this signal indicates that the
                                         -- FIFO is full. Write requests are ignored when the FIFO is full,
                                         -- initiating a write when the FIFO is full is not destructive to the
                                         -- contents of the FIFO.

      overflow      => open,             -- 1-bit output: Overflow: This signal indicates that a write request
                                         -- (wren) during the prior clock cycle was rejected, because the FIFO is
                                         -- full. Overflowing the FIFO is not destructive to the contents of the
                                         -- FIFO.

      prog_empty    => open,             -- 1-bit output: Programmable Empty: This signal is asserted when the
                                         -- number of words in the FIFO is less than or equal to the programmable
                                         -- empty threshold value. It is de-asserted when the number of words in
                                         -- the FIFO exceeds the programmable empty threshold value.

      prog_full     => open,             -- 1-bit output: Programmable Full: This signal is asserted when the
                                         -- number of words in the FIFO is greater than or equal to the
                                         -- programmable full threshold value. It is de-asserted when the number
                                         -- of words in the FIFO is less than the programmable full threshold
                                         -- value.

      rd_data_count => rd_data_count(i), -- RD_DATA_COUNT_WIDTH-bit output: Read Data Count: This bus indicates
                                         -- the number of words read from the FIFO.

      rd_rst_busy   => open,             -- 1-bit output: Read Reset Busy: Active-High indicator that the FIFO
                                         -- read domain is currently in a reset state.

      sbiterr       => open,             -- 1-bit output: Single Bit Error: Indicates that the ECC decoder
                                         -- detected and fixed a single-bit error.

      underflow     => open,             -- 1-bit output: Underflow: Indicates that the read request (rd_en)
                                         -- during the previous clock cycle was rejected because the FIFO is
                                         -- empty. Under flowing the FIFO is not destructive to the FIFO.

      wr_ack        => open,             -- 1-bit output: Write Acknowledge: This signal indicates that a write
                                         -- request (wr_en) during the prior clock cycle is succeeded.

      wr_data_count => wr_data_count(i), -- WR_DATA_COUNT_WIDTH-bit output: Write Data Count: This bus indicates
                                         -- the number of words written into the FIFO.

      wr_rst_busy   => open,             -- 1-bit output: Write Reset Busy: Active-High indicator that the FIFO
                                         -- write domain is currently in a reset state.

      din           => din,              -- WRITE_DATA_WIDTH-bit input: Write Data: The input data bus used when
                                         -- writing the FIFO.

      injectdbiterr => '0',              -- 1-bit input: Double Bit Error Injection: Injects a double bit error if
                                         -- the ECC feature is used on block RAMs or UltraRAM macros.

      injectsbiterr => '0',              -- 1-bit input: Single Bit Error Injection: Injects a single bit error if
                                         -- the ECC feature is used on block RAMs or UltraRAM macros.

      rd_en         => rd_en(i),         -- 1-bit input: Read Enable: If the FIFO is not empty, asserting this
                                         -- signal causes data (on dout) to be read from the FIFO. Must be held
                                         -- active-low when rd_rst_busy is active high.

      rst           => rst,              -- 1-bit input: Reset: Must be synchronous to wr_clk. The clock(s) can be
                                         -- unstable at the time of applying reset, but reset must be released
                                         -- only after the clock(s) is/are stable.

      sleep         => '0',              -- 1-bit input: Dynamic power saving- If sleep is High, the memory/fifo
                                         -- block is in power saving mode.

      wr_clk        => clk,              -- 1-bit input: Write clock: Used for write operation. wr_clk must be a
                                         -- free running clock.

      wr_en         => wr_en(i)          -- 1-bit input: Write Enable: If the FIFO is not full, asserting this
                                         -- signal causes data (on din) to be written to the FIFO Must be held
                                         -- active-low when rst or wr_rst_busy or rd_rst_busy is active high

    );

  end generate g_GENERATE_FIFOS;
  
  -- calculate current FIFO index
  cur_fifo_index <= vc(g_L2_FIFO_NUM - 1 downto 0); -- [Peixuan TODO]: This need to modify if we have 2 sets. 
  ---- [Peixuan TODO]: Or this makes no difference? Only the current serving set matters

  -- find earliest non-empty FIFO
  ---p_earliest_non_empty_fifo: process(rst, clk)

  ---begin
  
  ---end process p_earliest_non_empty_fifo;


  -- [Function] Enqueue process <enque_p>
  p_enqueue: process(rst, clk)

  -- [Peixuan TODO] Logic:

  -- (1) Get enque dat:
    -- i) get pkt
    -- ii) get fin_time
    -- iii) get flow_id
  
  -- (2) Calculate enque level <find_insert_level(pkt_finish_time)>
    -- Traverse all levels, from level 0 to top level
    
    -- for (level = 0; level < level_num; level++):
      -- if (fin_time <= level_max_vc):
        -- level_max_vc = (floor(vc/granularity[level]) + fifo_num[level]) * granularity[level]
        -- return level  
    -- return -1 (overflow)
  
  -- (3) If insert_level = -1: drop pkt

  -- (4) Two cases: a) insert level = top level; b) insert level < top level

    -- (4) a) insert top level
      -- Top level only have one set (set A)

      -- i) get cur_fifo of top level set A
      -- ii) get enque fifo index offset by fin_time & cur_fifo & granularity
        -- offset = floor(fin_time / granularity) - floor(vc / granularity)
      -- iii) get  enque fifo index:
        -- if (cur_fifo + offset < fifo_num)
          -- enque_fifo_index = cur_fifo + offset
        -- else
          -- enque_fifo_index = cur_fifo + offset - fifo_num
      -- iv) enque pkt to top level set A enque_fifo_index
      -- v) gearbox pkt_cnt + 1
    
    -- (4) b) insert level < top level
      -- We have 2 sets (set A & B)

      -- i) get cur_fifo of current serving set (A or B by is_serve_A[level] array)
      -- ii) get enque fifo index offset by fin_time & cur_fifo & granularity
        -- offset = floor(fin_time / granularity) - floor(vc / granularity)
      -- iii) get  enque fifo index:
        -- if (cur_fifo + offset < fifo_num)
          -- enque current set
          -- enque_fifo_index = cur_fifo + offset
        -- else
          -- enque other set
          -- enque_fifo_index = cur_fifo + offset - fifo_num
      -- iv) enque pkt to this level set (A or B, based on the above if statment) enque_fifo_index
      -- v) gearbox pkt_cnt + 1

  begin
    
  end process p_enqueue;
  
  -- [Function] dequeue process <deque_p>
  p_dequeue: process(rst, clk)
  begin
    
  end process p_dequeue;
  

  -- [Function] find_earliest_non_empty_level_fifo_p

    -- 1. yield on find_earliest_non_empty_level_and_fifo_signal

    -- 2. Get earliest non empty fifo from all the levels and sets (this should be better with VHDL parrell)
      -- Get max VC of earliest non empty fifo from all the levels and sets
      -- Jump VC = min{ max VC of earliest non empty fifo from all the levels and sets }

end gearbox_I_arch;