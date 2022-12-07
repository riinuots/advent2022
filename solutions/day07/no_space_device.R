library(tidyverse)
library(igraph)

# clumsy input wrangling
input_orig = read_lines("solutions/day07/input-test2")

parents = tibble(name = input_orig) %>% 
  rowid_to_column() %>% 
  mutate(parent = if_else(name == "$ ls",
                          paste("dir",rowid, lag(name), sep = "_"), NA_character_) %>% 
           str_remove("\\$ cd ")) %>% 
  fill(parent, .direction = "down") %>% 
  filter(!str_starts(name, "\\$")) %>% 
  mutate(parent = str_replace(parent, "\\/", "top")) %>% 
  mutate(is_dir = str_detect(name, "dir")) %>% 
  separate(name, into = c("size", "name"), sep = " ") %>% 
  mutate(weights = parse_number(size) %>% 
           replace_na(0)) %>% 
  select(name, parent, weights) %>% 
  mutate(name = paste(name, weights, sep = "_"))

dirs = distinct(parents, parent)
files = parents %>% 
  filter(weights != 0)

# Part I
g = graph_from_data_frame(parents)

children_files = distances(g, to = V(g), mode = "out") %>% 
  as_tibble() %>% 
  mutate(from = colnames(.)) %>% 
  pivot_longer(-from, names_to = "dir") %>% 
  filter(! is.infinite(value) & from != dir & ! str_starts(from, "dir"))

sums = children_files %>% 
  left_join(select(parents, -parent), by = c("from" = "name")) %>% 
  group_by(dir) %>% 
  summarise(total = sum(weights))

parents$weights %>% sum()

sums %>% 
  filter(total <= 100000) %>% 
  summarise(sum(total))

1538808 - 1538808

# 1401436 too low
# 1444547 too low
# 1538808 too low
# dirs$parent
sums = distances(g, to = dirs$parent, mode = "out", weights = as.numeric(parents$weights)) %>% 
  as_tibble() %>% 
  pivot_longer(everything()) %>% 
  mutate(value = na_if(value, Inf)) %>% 
  drop_na() %>% 
  group_by(name) %>% 
  summarise(total = sum(value))

sums %>% 
  filter(total <= 100000) %>% 
  summarise(sum(total))


