library(tidyverse)
library(igraph)
n = read_lines("solutions/day12/input", n_max = 1)
input_orig = read.fwf("solutions/day12/input", rep(1, str_count(n)))

area_orig = input_orig %>% 
  rowid_to_column("x") %>% 
  pivot_longer(-x, names_to = "y", values_to = "height") %>% 
  mutate(y = str_remove(y, "V") %>% parse_integer()) %>% 
  mutate(id = paste(x, y, sep = "-"))

nbs = tribble(
  ~dx, ~dy,
  -1,  0, 
  0, -1, 
  0,  1, 
  1,  0
)

current     = filter(area_orig, height == "S") %>% pull(id)
destination = filter(area_orig, height == "E") %>% pull(id)

area_orig = area_orig %>% 
  mutate(height = case_when(height == "S" ~ "a",
                            height == "E" ~ "z",
                            TRUE          ~ height) %>% 
           factor(levels = letters) %>% # abc to 123
           as.numeric())

area_joined = area_orig %>% 
  tidyr::crossing(nbs) %>% 
  mutate(x = x + dx,
         y = y + dy) %>% 
  rename(height0 = height) %>% 
  left_join(select(area_orig, -id)) %>% 
  drop_na() %>% 
  mutate(id_nb = paste(x, y, sep = "-")) %>% 
  filter((height0 + 1) >= height)


area = area_joined %>%
  select(id, id_nb) %>% 
  graph_from_data_frame()

# Part I
distances(area, current, destination, mode = "out")

# Part II

starting = filter(area_orig, height == 1) %>% pull(id)
min(distances(area, starting, destination, mode = "out"))
