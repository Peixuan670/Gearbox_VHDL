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
    
    deq_cmd                          : in  std_logic;
    deq_desc                         : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    deq_desc_valid                   : out std_logic;
    drop_cmd                         : out std_logic;
    drop_desc                        : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);

    pkt_cnt                          : out unsigned(g_L2_FIFO_NUM-1 downto 0) -- [Peixuan TODO] what should be the pkt_cnt's bit width?

  );
end gearbox_I;

architecture gearbox_I_arch of gearbox_I is

  -- [Modules]:
  -- <gearbox_levels arr> level_set_A[level_num]
  -- <gearbox_levels arr> level_set_B[level_num - 1]

  -- [Signals]: (port map to sub-modules)
  -- All the in/out signals of sub-modules (this evloves a lot of signals)

  -- [Variables]:

  -- <int arr> granularity_list[level_num]
  -- <int arr> fifo_num_list[level_num]
  -- <int arr> fifo_size_list[level_num]

  -- <int> vc (cur vc of gearbox)
  -- <int> update_vc (vc to update)

  -- <int> cur_fifo (cur_fifo index of a sepecific level, set)
  -- <int> enque_fifo (fifo index to enque)
  -- <int> deque_fifo (fifo index to deque)

  -- <boolean arr> is_serve_setA[level_num] (ping-pong indicator)
  
  -- <int arr>  bytes_to_serve[level_num] (bytes to serve in this round)
  -- <int arr>  served_bytes[level_num] (bytes serced in this round)

  -- <int arr> prev_enq_level_lst[flow_num] (previous enqued level index)


begin

  -- Instantiate FIFOs  
  g_GENERATE_FIFOS: for i in 0 to g_FIFO_NUM-1 generate

    -- xpm_fifo_sync: Synchronous FIFO
    -- Xilinx Parameterized Macro, version 2020.1

    xpm_fifo_sync_inst : xpm_fifo_sync
    generic map (
      
    )
    port map (
      -- We need multiple gearbox_level module

    );

  end generate g_GENERATE_FIFOS;


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

  -- (4) Check last enque level of this flow:
    -- If last_enque_level[flow_id] < insert_level:
      -- last_enque_level[flow_id] = insert_level
    -- else:
      -- insert_level = last_enque_level[flow_id]

  -- (5) Two cases: a) insert level = top level; b) insert level < top level

    -- (5) a) insert top level
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
    
    -- (5) b) insert level < top level
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
  
  -- [Peixuan TODO] Logic:

  -- (1) Wait for deque signal

  -- (2) deque_level = find_deque_level() -- find deque level by function find_deque_level()

  -- (3) while deque_level == -1 (means all the levels are served in this round):
    -- update vc until deque_level != -1 (find levels with bytes to serve)
    -- send signal to find_update_vc process
    -- get updated_vc: updated_vc = yield self.gb_update_VC_dat.get() from <find_earliest_non_empty_level_fifo_p>
    -- update vc to updated_vc
    -- get new deque_level: deque_level = find_deque_level()
  
  -- (4) now, deque_level is not -1, deque from deque_level
    -- i) Find cur serving set A or set B: self.level_ping_pong_arr[deque_level_index]
    -- ii) Find cur_fifo by vc: self.cur_fifo = math.floor(self.vc / self.granularity) % self.fifo_num
    -- iii) Deque cur set(A or B) gearbox_level cur_fifo
  
  -- (5) Update served bytes of the dequed level
    -- Served_bytes[level] = Served_bytes[level] + pkt_bytes
  -- (6) Update pkt_cnt of gearbox scheduler
    -- pkt_cnt = pkt_cnt - 1
  
  -- (6) Put dequed pkt to dout
  
    
  end process p_dequeue;

  -- [Function] find_deque_level
    -- From level 0 to top_level:
      -- if served_byte[level] < byte_to_serve[level]
        -- return current traversed level
    -- return -1 if no level served byte < byte to serve (finished serving all levels)
  

  -- [Function] find_earliest_non_empty_level_fifo_p

    -- 1. yield on find_earliest_non_empty_level_and_fifo_signal

    -- 2. Get earliest non empty fifo from all the levels and sets (this should be better with VHDL parrell)
      -- Get max VC of earliest non empty fifo from all the levels and sets
      -- updated_vc = min{ max VC of earliest non empty fifo from all the levels and sets }
      -- return updated_vc
  
  -- [Function] update_vc(updated_vc)

    -- 1. update is_serve_setA (ping-pong indicator)
      -- is_serve_set_A in level <level_l> = floor( updated_vc / (level_l's granularity * level_l's fifo num)) % 2 == 0
      -- if == 0, serve set A, else serve set B
          -- serve_set_A = (math.floor(float(updated_vc) / (self.granularity_list[index] * self.fifo_num_list[index])) % 2 == 0)
      -- self.level_ping_pong_arr[index] = serve_set_A
    
    -- 2. find current serving fifo of each level, we can also do this else where when we need to access cur fifo

    -- 3. update top level deque bytes
      -- Traverse all levels
        -- i) find cur_fifo of this level
        -- ii) find the byte cnt of this fifo
        -- iii) deque_byte = ((level0's_granularity / this_level's_granularity) * ceil(cur_fifo_index/this_level's_fifo_num)) * byte_cnt_of_cur_fifo
        -- iv) update deque_byte (byte to deque for this level in this round) to the array
        -- v) reset dequed_byte (byte already dequed for this level in this round) to 0
    
    -- 4. send out updated_vc to outter module if necessary

end gearbox_I_arch;