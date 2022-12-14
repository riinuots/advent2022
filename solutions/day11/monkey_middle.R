library(tidyverse)
library(unglue)

input_orig = read_csv("solutions/day11/input", col_names = "all")
n_monkeys = nrow(input_orig)/6

input_pattern = "Monkey {monkey}: Starting items: {old} Operation: new = old {op} {op_n} Test: divisible by {div} If true: throw to monkey {div1} If false: throw to monkey {div2}"

items_monkeys = input_orig %>% 
  mutate(split = rep(1:n_monkeys, each = 6)) %>% 
  group_by(split) %>% 
  summarise(all = paste(all, collapse = " ")) %>% 
  #slice(3) %>% pull(all)
  unglue_unnest(all, input_pattern, convert = TRUE) %>% 
  separate_rows(old, sep = ", ", convert = TRUE) %>% 
  mutate(monkey = monkey + 1,
         div1   = div1   + 1,
         div2   = div2   + 1,
         op_n = parse_number(op_n))

items = items_monkeys %>% 
  select(monkey, old)

monkeys = items_monkeys %>% 
  distinct(monkey, op, op_n, div, div1, div2)

monkey_handles = as.list(rep(0, nrow(monkeys)))

magic = prod(monkeys$div)

# Parts I and II
start = Sys.time()
for (round in 1:10000){
  if (round %% 100 == 0){
    print(paste("round:", round))
  }
  for (mymonkey in unique(monkeys$monkey)){
    #mymonkey = 3
    #print(paste("monkey:", mymonkey))
    m_items   = filter(items, monkey == mymonkey) %>% 
      left_join(monkeys, by = "monkey")
    if (nrow(m_items) == 0){next}
    rem_items = filter(items, monkey != mymonkey)
    monkey_handles[[mymonkey]] = monkey_handles[[mymonkey]] + nrow(m_items)
    
    items = bind_rows(rem_items,
                      m_items %>% 
                        mutate(op_n = if_else(is.na(op_n), as.numeric(old), op_n)) %>% 
                        rowwise() %>% # bc get() is not vectorised
                        # Part I
                        # mutate(new = floor((get(op)(old, op_n))/3)) %>% 
                        #Part II
                        mutate(new = get(op)(old, op_n)) %>%
                        ungroup() %>% 
                        mutate(new = new %% magic) %>% 
                        mutate(monkey = if_else(new %% div == 0, div1, div2)) %>% 
                        select(monkey, old = new)
    )
  }
}


monkey_handles %>% 
  unlist() %>% 
  sort() %>%
  tail(2) %>%
  prod() %>% 
  print(digits = 10)

took = Sys.time() - start

# options(pillar.sigfig = 100)
# items
