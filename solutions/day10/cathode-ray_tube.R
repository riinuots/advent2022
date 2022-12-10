library(tidyverse)

input_orig = read_csv("solutions/day10/input", col_names = "instr")

cycles = input_orig %>% 
  separate(instr, into = c("instr", "value"), sep = " ", convert = TRUE) %>% 
  mutate(value = replace_na(value, 0)) %>% 
  mutate(n_cycles = if_else(instr == "addx", 2, 1)) %>% 
  rowid_to_column("instr_id") %>% 
  uncount(n_cycles) %>% 
  rowid_to_column("cycle_id") %>% 
  mutate(value = if_else(instr_id == lead(instr_id, default = 0), as.integer(0), value)) %>% 
  mutate(X = cumsum(value) + 1)

  
# Part I 
cycles %>% 
  slice(20, 60, 100, 140, 180, 220) %>% 
  mutate(strength = cycle_id*(X-value)) %>% 
  summarise(sum(strength))

# Part II
screen = cycles %>% 
  mutate(loc = rep(1:40, 6),
         row = rep(1:6, each = 40)) %>% 
  mutate(current = lag(X, default = 1)) %>% 
  rowwise() %>% 
  mutate(draw = if_else((current + 1) %in% c(loc - 1, loc, loc + 1),
                        "lit",
                        "dark"))

screen %>% 
  ggplot(aes(loc, -row, fill = draw)) +
  geom_tile() +
  scale_fill_manual(values = c("black", "white")) +
  coord_fixed()

#RLEZFLGE