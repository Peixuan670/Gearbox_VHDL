-- gearboxI module

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
    g_VC_BIT_WIDTH    : integer      -- number of bits in VC
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

  -- <int> vc (cur vc of gearbox)
  -- <int> update_vc (vc to update)

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

constant  GRANULARITY : integer := g_L2_FIFO_NUM;

type t_fin_time_arr is array(0 to g_FLOW_NUM-1) of unsigned(g_VC_BIT_WIDTH-1 downto 0);
fin_time_arr : t_fin_time_arr := (others => (others => '0'));
type t_curr_fifo_arr is array(0 to g_LEVEL_NUM-1) of unsigned(g_L2_FIFO_NUM-1 downto 0);
curr_fifo_arr: t_curr_fifo_arr;

signal enq_cmd_d1   : std_logic;
signal enq_cmd_d2   : std_logic;
signal enq_cmd_d3   : std_logic;
signal lvl_enq_desc : std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
signal lvl_enq_cmd_A: std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal lvl_enq_cmd_B: std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal pkt_time     : unsigned(PKT_TIME_BIT_WIDTH-1 downto 0):
signal flow_id      : unsigned(FLOW_ID_BIT_WIDTH-1 downto 0);
signal fin_time     : unsigned(PKT_TIME_BIT_WIDTH-1 downto 0);
signal enq_level    : unsigned(g_L2_LEVEL_NUM-1 downto 0);
signal fifo_offset  : unsigned(g_L2_FIFO_NUM-1 downto 0);
signal vc           : unsigned(g_VC_BIT_WIDTH-1 downto 0);

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
      enq_cmd                          => lvl_enq_cmd_A(i),
      enq_fifo_index                   => lvl_enq_fifo_index,
      enq_desc                         => lvl_enq_desc,
      enq_done                         => open,
      -- deq i/f
      deq_cmd                          => lvl_deq_cmd_A(i),
      deq_fifo_index                   => lvl_deq_fifo_index,
      deq_desc                         => lvl_deq_desc_A(i),
      deq_desc_valid                   => lvl_deq_desc_valid_A(i),
      -- find earliest fifo i/f
      find_earliest_non_empty_fifo_cmd => find_earliest_non_empty_fifo_cmd_a,
      current_fifo_index               => current_fifo_index,
      find_earliest_non_empty_fifo_rsp => find_earliest_non_empty_fifo_rsp_a,
      earliest_fifo_index              => earliest_fifo_index_a,
      all_fifos_empty                  => all_fifos_empty_a,
      -- fifo count i/f
      get_fifo_cnts_cmd                => get_fifo_cnts_cmd_A(i),
      get_fifo_cnts_index              => get_fifo_cnts_index,
      get_fifo_cnts_rsp                => get_fifo_cnts_rsp_A(i),
      fifo_pkt_cnt                     => fifo_pkt_cnt_A(i),
      fifo_byte_cnt                    => fifo_byte_cnt_A(i),
      -- level count i/f (always valid)
      level_pkt_cnt                    => level_pkt_cnt_A(i),
      level_byte_cnt                   => level_byte_cnt_A(i)
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
      enq_cmd                          => lvl_enq_cmd_B(i),
      enq_fifo_index                   => lvl_enq_fifo_index,
      enq_desc                         => lvl_enq_desc,
      enq_done                         => open,
      -- deq i/f
      deq_cmd                          => lvl_deq_cmd_B(i),
      deq_fifo_index                   => lvl_deq_fifo_index,
      deq_desc                         => lvl_deq_desc_B(i),
      deq_desc_valid                   => lvl_deq_desc_valid_B(i),
      -- find earliest fifo i/f
      find_earliest_non_empty_fifo_cmd => find_earliest_non_empty_fifo_cmd_B(i),
      current_fifo_index               => current_fifo_index,
      find_earliest_non_empty_fifo_rsp => find_earliest_non_empty_fifo_rsp_B(i),
      earliest_fifo_index              => earliest_fifo_index_B(i),
      all_fifos_empty                  => all_fifos_empty_B(i),
      -- fifo count i/f
      get_fifo_cnts_cmd                => get_fifo_cnts_cmd_B(i),
      get_fifo_cnts_index              => get_fifo_cnts_index,
      get_fifo_cnts_rsp                => get_fifo_cnts_rsp_B(i),
      fifo_pkt_cnt                     => fifo_pkt_cnt_B(i),
      fifo_byte_cnt                    => fifo_byte_cnt_B(i),
      -- level count i/f (always valid)
      level_pkt_cnt                    => level_pkt_cnt_B(i),
      level_byte_cnt                   => level_byte_cnt_B(i)
    );
  end generate g_GENERATE_LEVELS_B;

  -- [Function] Enqueue process <enque_p>
  p_enqueue: process(rst, clk)
  variable v_enq_level_found: boolean := false;
  begin
    if rst = '1' then
      drop_cmd   <= '0';
      enq_cmd_d1 <= '0';
      enq_cmd_d2 <= '0';
      enq_cmd_d3 <= '0';
      lvl_enq_cmd_A <= (other => '0');
      lvl_enq_cmd_B <= (other => '0');
      
    elsif clk'event and clk = '1' then
      -- defaults
      enq_cmd_d1 <= '0';
      enq_cmd_d2 <= '0';
      enq_cmd_d3 <= '0';
      lvl_enq_cmd_A <= (other => '0');
      lvl_enq_cmd_B <= (other => '0');
      
      -- determine current FIFO per level based on virtual clock 
      for i in 0 to g_LEVEL_NUM - 1 loop
        curr_fifo_arr(i) <= vc((i+1)*g_VC_BIT_WIDTH/GRANULARITY - 1 downto i*g_VC_BIT_WIDTH/GRANULARITY);
      end loop;

      -- Clk 1: process enqueue command
      if enq_cmd = '1' then
        -- load both drop and level enqueue descriptor - we will use only one of them
        drop_desc    <= enq_desc;
        lvl_enq_desc <= enq_desc;
        -- extract packet time and flow id from descriptor
        pkt_time <= unsigned(enq_desc(g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - 1 downto 
                                        g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH));
        flow_id  <= unsigned(enq_desc(g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH - 1 downto 
                                        g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH - FLOW_ID_BIT_WIDTH);
        enq_cmd_d1 <= '1';
      end if;
      
      -- Clk 2: Calculate enqueue level 
      if enq_cmd_d1 <= '1' then
        -- calculate incremental finish time
        if vc >= fin_time_arr(to_integer(flow_id)) then
          fin_time <= pkt_time;
        else
          fin_time <= pkt_time + fin_time_arr(to_integer(flow_id)) - vc;
        end if;
    
        -- if incremental finish time greater than max level capacity, drop pkt (check MSB bits)
        if fin_time(FIN_TIME_BIT_WIDTH - 1 downto g_LEVEL_NUM*FIN_TIME_BIT_WIDTH/GRANULARITY) /= 0 then
          drop_cmd <= '1';
      
        -- determine enq level by examining slices of fin_time.  enq_level = highest non-zero slice
        for i in g_LEVEL_NUM downto 1 loop
          if fin_time(i*FIN_TIME_BIT_WIDTH/GRANULARITY - 1 downto (i-1)*FIN_TIME_BIT_WIDTH/GRANULARITY) /= 0 and 
               v_enq_level_found = false then
            enq_level <= to_unsigned(i - 1, g_L2_LEVEL_NUM);
            fifo_offset <= fin_time(i*FIN_TIME_BIT_WIDTH/GRANULARITY - 1 downto (i-1)*FIN_TIME_BIT_WIDTH/GRANULARITY);
            v_enq_level_found := true;
          end if;
        end loop;
        enq_cmd_d2 <= '1';
      end if;
    
      -- Clk 3: Check last enque level of this flow:
      -- If last_enque_level[flow_id] < insert_level:
      --   last_enque_level[flow_id] = insert_level
      -- else:
      --   insert_level = last_enque_level[flow_id]
      if enq_cmd_d2 = '1' and drop_cmd = '0' then
        -- update fin_time_arr entry for flow_id
        fin_time_arr(to_integer(flow_id)) <= vc + fin_time;
        
        if enq_level > last_enq_level_arr(to_integer(to_integer(flow_id))) then
          last_enq_level_arr(to_integer(flow_id)) <= enq_level;
          v_enq_level := enq_level
        else
          v_enq_level := last_enq_level_arr(to_integer(to_integer(flow_id)));
        end if;
    
        -- Two cases: a) insert level = top level; b) insert level < top level
        -- a) insert top level
        -- Top level only have one set (set A)
        if v_enq_level = g_LEVEL_NUM - 1 then
          lvl_enq_fifo_index             <= curr_fifo_arr(v_enq_level) + fifo_offset;
          lvl_enq_cmd_A(g_LEVEL_NUM - 1) <= '1';
        else
          -- b) insert level < top level
          -- We have 2 sets (set A & B)
          -- get cur_fifo of current serving set (A or B by is_serve_A[level] array)
          -- if (cur_fifo + offset < fifo_num)
          --   enque current set
          --   enque_fifo_index = curr_fifo + offset
          -- else
          --   enque other set
          --   enque_fifo_index = curr_fifo + offset - fifo_num          
          if is_serve_A_arr(to_integer(v_enq_level)) then
            if curr_fifo_arr(v_enq_level) + fifo_offset < g_FIFO_NUM then
              lvl_enq_fifo_index             <= curr_fifo_arr(v_enq_level) + fifo_offset;
              lvl_enq_cmd_A(g_LEVEL_NUM - 1) <= '1';
            else
              lvl_enq_fifo_index             <= curr_fifo_arr(v_enq_level) + fifo_offset;
              lvl_enq_cmd_B(g_LEVEL_NUM - 1) <= '1';
            end if;
          else
            if curr_fifo_arr(v_enq_level) + fifo_offset < g_FIFO_NUM then
              lvl_enq_fifo_index             <= curr_fifo_arr(v_enq_level) + fifo_offset;
              lvl_enq_cmd_B(g_LEVEL_NUM - 1) <= '1';
            else
              lvl_enq_fifo_index             <= curr_fifo_arr(v_enq_level) + fifo_offset;
              lvl_enq_cmd_A(g_LEVEL_NUM - 1) <= '1';
            end if;
          end if;      
        end if;
        -- increment packet counter
        gb_pkt_cnt <= gb_pkt_cnt + 1;
      end if;
    end if;
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