library(tidyverse)

input_orig = read_csv("solutions/day04/input", col_names = c("elf1", "elf2")) 
                 
camp = input_orig %>% 
  rowid_to_column("pair_id") %>% 
  separate(elf1, into = c("elf1_start", "elf1_end"), convert = TRUE) %>% 
  separate(elf2, into = c("elf2_start", "elf2_end"), convert = TRUE) 

# Part I
camp %>% 
  filter((elf1_start >= elf2_start & elf1_end <= elf2_end) |
           elf2_start >= elf1_start & elf2_end <= elf1_end) %>% 
  nrow()
  
# Part II

camp %>% 
  filter((elf2_start <= elf1_end & elf2_end >= elf1_start) |
           (elf1_start <= elf2_end & elf1_end >= elf2_start)) %>% 
  nrow()
  