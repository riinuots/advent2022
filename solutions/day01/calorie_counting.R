library(tidyverse)

input_orig = read_csv("solutions/day01/input",
                      col_names = "calories",
                      skip_empty_rows = FALSE)

# Part I
calories = input_orig %>% 
  rowid_to_column() %>% 
  mutate(elf_rowid = if_else(is.na(calories), paste("elf", rowid), NA_character_)) %>% 
  fill(elf_rowid, .direction = "up") %>% 
  drop_na() %>% 
  group_by(elf_rowid) %>% 
  summarise(total = sum(calories)) %>% 
  ungroup()

calories %>% 
  slice_max(total)

# Part II

calories %>% 
  slice_max(total, n = 3) %>% 
  summarise(sum(total))
