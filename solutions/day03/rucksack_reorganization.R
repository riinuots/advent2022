library(tidyverse)

input_orig = read_table("solutions/day03/input", col_names = "content")

priorities = tibble(content = c(letters, LETTERS), priority = 1:52)

# Part I
rucksacks = input_orig %>% 
  mutate(n      = str_count(content, ""),
         first  = str_sub(content, 1, n/2),
         second = str_sub(content, n/2+1, n)) %>% 
  select(-content, -n) %>% 
  rowid_to_column("ruck_id")

content = input_orig %>% 
  mutate(content = str_split(content, "")) %>% 
  rowid_to_column("ruck_id") %>% 
  unnest(content) %>% 
  group_by(ruck_id) %>% 
  rowid_to_column("content_id") %>% 
  mutate(compartment = if_else(content_id < n()/2, "first", "second")) %>% 
  ungroup()


content %>% 
  count(ruck_id, compartment, content) %>% 
  ungroup() %>% 
  left_join(rucksacks) %>% 
  #filter(content == "L") %>% 
  filter(str_detect(first, content) & str_detect(second, content)) %>% 
  distinct(ruck_id, content) %>% 
  left_join(priorities) %>% 
  summarise(sum(priority))


# Part II 

n_elves = nrow(input_orig)/3

elves = input_orig %>% 
  mutate(group_id = rep(1:n_elves, each = 3)) %>% 
  mutate(elf_id   = paste0("elf", rep(1:3, n_elves))) %>% 
  rename(full = content) %>% 
  pivot_wider(names_from = elf_id, values_from = full)

content %>% 
  mutate(group_id = ceiling(ruck_id/3)) %>% 
  left_join(elves) %>% 
  filter(str_detect(elf1, content) & str_detect(elf2, content) & str_detect(elf3, content))  %>% 
  distinct(group_id, content) %>% 
  left_join(priorities) %>% 
  summarise(sum(priority))
