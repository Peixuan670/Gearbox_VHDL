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
    g_FLOW_NUM        : integer;     -- number of flow
    g_LEVEL_NUM       : integer;     -- number of levels
    g_L2_LEVEL_NUM    : integer;     -- log2 of number of levels
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

type t_fin_time_arr is array(0 to g_FLOW_NUM-1) of unsigned(FIN_TIME_BIT_WIDTH-1 downto 0);
fin_time_arr : t_fin_time_arr := (others => (others => '0'));

begin

  -- <gearbox_levels arr> level_set_A[level_num]
  -- Instantiate level set A  
  g_GENERATE_LEVELS_A: for i in 0 to g_LEVEL_NUM-1 generate
    i_gb_level_a: gearbox_level
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
      enq_cmd                          => enq_cmd_a(i),
      enq_fifo_index                   => enq_fifo_index,
      enq_desc                         => enq_desc,
      enq_done                         => enq_done_a(i),
      -- deq i/f
      deq_cmd                          => deq_cmd_a(i),
      deq_fifo_index                   => deq_fifo_index,
      deq_desc                         => deq_desc_a,
      deq_desc_valid                   => deq_desc_valid_a,
      -- find earliest fifo i/f
      find_earliest_non_empty_fifo_cmd => find_earliest_non_empty_fifo_cmd_a,
      current_fifo_index               => current_fifo_index,
      find_earliest_non_empty_fifo_rsp => find_earliest_non_empty_fifo_rsp_a,
      earliest_fifo_index              => earliest_fifo_index_a,
      all_fifos_empty                  => all_fifos_empty_a,
      -- fifo count i/f
      get_fifo_cnts_cmd                => get_fifo_cnts_cmd_a(i),
      get_fifo_cnts_index              => get_fifo_cnts_index,
      get_fifo_cnts_rsp                => get_fifo_cnts_rsp_a,
      fifo_pkt_cnt                     => fifo_pkt_cnt_a,
      fifo_byte_cnt                    => fifo_byte_cnt_a,
      -- level count i/f (always valid)
      level_pkt_cnt                    => level_pkt_cnt_a,
      level_byte_cnt                   => level_byte_cnt_a
    );
  end generate g_GENERATE_LEVELS_A;

  -- <gearbox_levels arr> level_set_B[level_num - 1]
  -- Instantiate level set B  
  g_GENERATE_LEVELS_B: for i in 0 to g_LEVEL_NUM-2 generate
    i_gb_level_b: gearbox_level
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
      enq_cmd                          => enq_cmd_b(i),
      enq_fifo_index                   => enq_fifo_index,
      enq_desc                         => enq_desc,
      enq_done                         => enq_done_b(i),
      -- deq i/f
      deq_cmd                          => deq_cmd_b(i),
      deq_fifo_index                   => deq_fifo_index,
      deq_desc                         => deq_desc_b,
      deq_desc_valid                   => deq_desc_valid_b,
      -- find earliest fifo i/f
      find_earliest_non_empty_fifo_cmd => find_earliest_non_empty_fifo_cmd_b,
      current_fifo_index               => current_fifo_index,
      find_earliest_non_empty_fifo_rsp => find_earliest_non_empty_fifo_rsp_b,
      earliest_fifo_index              => earliest_fifo_index_b,
      all_fifos_empty                  => all_fifos_empty_b,
      -- fifo count i/f
      get_fifo_cnts_cmd                => get_fifo_cnts_cmd_b(i),
      get_fifo_cnts_index              => get_fifo_cnts_index,
      get_fifo_cnts_rsp                => get_fifo_cnts_rsp_b,
      fifo_pkt_cnt                     => fifo_pkt_cnt_b,
      fifo_byte_cnt                    => fifo_byte_cnt_b,
      -- level count i/f (always valid)
      level_pkt_cnt                    => level_pkt_cnt_b,
      level_byte_cnt                   => level_byte_cnt_b
    );
  end generate g_GENERATE_LEVELS_B;

  -- [Function] Enqueue process <enque_p>
  p_enqueue: process(rst, clk)
  variable v_fin_time : unsigned(FIN_TIME_BIT_WIDTH-1 downto 0);
  begin

  -- (1) Get enque dat:
    -- i) get pkt
    -- ii) get fin_time
    -- iii) get flow_id
    -- process enqueue command
    if enq_cmd = '1' then
      -- load both drop and level enqueue descriptor - we will use only one of them
      drop_desc    <= enq_desc;
      lvl_enq_desc <= enq_desc;
      -- extract finish time and flow id from descriptor
      v_fin_time := unsigned(enq_desc(g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - 1 downto 
                                        g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH));
      v_flow_id  := unsigned(enq_desc(g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH - 1 downto 
                                        g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH - FLOW_ID_BIT_WIDTH);
      enq_cmd_d1 <= '1';
    end if;
  -- (2) Calculate enque level <find_insert_level(pkt_finish_time)>
    -- Traverse all levels, from level 0 to top level
    
    -- for (level = 0; level < level_num; level++):
      -- if (fin_time <= level_max_vc):
        -- level_max_vc = (floor(vc/granularity[level]) + fifo_num[level]) * granularity[level]
        -- return level  
    -- return -1 (overflow)
    if enq_cmd_d1 <= '1' then
      if vc >= fin_time_arr(to_integer(v_flow_id)) then
        fin_time <= v_fin_time;
      else
        fin_time <= v_fin_time + fin_time_arr(to_integer(v_flow_id)) - vc;
      end if;
    
      -- (3) If insert_level = -1: drop pkt
      if fin_time(FIN_TIME_BIT_WIDTH - 1 downto g_LEVEL_NUM*FIN_TIME_BIT_WIDTH/GRANULARITY) /= 0 then
        drop_cmd <= '1';
      
      for i in g_LEVEL_NUM downto 1 loop
        if fin_time(i*FIN_TIME_BIT_WIDTH/GRANULARITY - 1 downto (i-1)*FIN_TIME_BIT_WIDTH/GRANULARITY) /= 0 and 
             v_enq_level_found = false then
          v_enq_level := to_unsigned(i - 1, g_L2_LEVEL_NUM);
          v_fifo_offset := fin_time(i*FIN_TIME_BIT_WIDTH/GRANULARITY - 1 downto (i-1)*FIN_TIME_BIT_WIDTH/GRANULARITY);
          v_enq_level_found := true;
        end if;
      end loop;
      enq_cmd_d2 <= '1';
    end if;
    
  -- (4) Check last enque level of this flow:
    -- If last_enque_level[flow_id] < insert_level:
      -- last_enque_level[flow_id] = insert_level
    -- else:
      -- insert_level = last_enque_level[flow_id]
    if enq_cmd_d2 = '1' and drop_cmd = '0' then
      if v_enq_level > last_enq_level_arr(to_integer(to_integer(v_flow_id))) then
        v_enq_level := last_enq_level_arr(to_integer(to_integer(v_flow_id)));
      else
        last_enq_level_arr(to_integer(v_flow_id)) := v_enq_level;
      end if;
    
  -- (5) Two cases: a) insert level = top level; b) insert level < top level

    -- (5) a) insert top level
      -- Top level only have one set (set A)
      if v_enq_level = g_LEVEL_NUM - 1 then
      -- i) get cur_fifo of top level set A      
      -- ii) get enque fifo index offset by fin_time & cur_fifo & granularity
        -- offset = floor(fin_time / granularity) - floor(vc / granularity)
      -- iii) get  enque fifo index:
        -- if (cur_fifo + offset < fifo_num)
          -- enque_fifo_index = cur_fifo + offset
        -- else
          -- enque_fifo_index = cur_fifo + offset - fifo_num
      -- iv) enque pkt to top level set A enque_fifo_index
        enq_fifo_index                 <= curr_fifo_arr_A(v_enq_level) + v_fifo_offset;
        lvl_enq_cmd_a(g_LEVEL_NUM - 1) <= '1';
      
    
      else
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
        if is_serve_A_arr(to_integer(v_enq_level)) then
          if curr_fifo_arr_A(v_enq_level) + v_fifo_offset < g_FIFO_NUM then
             enq_fifo_index                 <= curr_fifo_arr_A(v_enq_level) + v_fifo_offset;
             lvl_enq_cmd_A(g_LEVEL_NUM - 1) <= '1';
          else
             enq_fifo_index                 <= curr_fifo_arr_Bv_enq_level) + v_fifo_offset;
             lvl_enq_cmd_B(g_LEVEL_NUM - 1) <= '1';
          end if;
        else
          if curr_fifo_arr_B(v_enq_level) + v_fifo_offset < g_FIFO_NUM then
             enq_fifo_index                 <= curr_fifo_arr_B(v_enq_level) + v_fifo_offset;
             lvl_enq_cmd_B(g_LEVEL_NUM - 1) <= '1';
          else
             enq_fifo_index                 <= curr_fifo_arr_Av_enq_level) + v_fifo_offset;
             lvl_enq_cmd_A(g_LEVEL_NUM - 1) <= '1';
          end if;
        end if;      
      end if;
      -- v) gearbox pkt_cnt + 1
      gb_pkt_cnt <= gb_pkt_cnt + 1;

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