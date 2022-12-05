library(tidyverse)
library(unglue)

# read in stacks and make into lists
stacks = read_lines("solutions/day05/input")[1:8] %>% 
  str_replace_all("    ", "[]") %>% 
  str_remove_all(" ") %>% 
  unglue_data("[{s1}][{s2}][{s3}][{s4}][{s5}][{s6}][{s7}][{s8}][{s9}]") %>% 
  pivot_longer(everything()) %>% 
  mutate(value = na_if(value, "")) %>% 
  drop_na() %>% 
  group_by(name) %>% 
  arrange(name) %>% 
  group_map(~rev(pull(.x, value)))

# read in instructions
instructions = read_lines("solutions/day05/input", skip = 10) %>% 
  unglue_data("move {n} from {from} to {to}", convert = TRUE)

# Parts I and II
stacks_mod = stacks
is_part2 = TRUE
for (i in 1:nrow(instructions)){
  n    = instructions[i, "n"]
  from = instructions[i, "from"]
  to   = instructions[i, "to"]
  print(stacks_mod)
  print(instructions[i, ])
  print("lifting:")
  print(rev(tail(stacks_mod[[from]], n)))
  if (is_part2){
    stacks_mod[[to]] = c(stacks_mod[[to]], tail(stacks_mod[[from]], n))
  } else{
    stacks_mod[[to]] = c(stacks_mod[[to]], rev(tail(stacks_mod[[from]], n)))
  }
  stacks_mod[[from]] = head(stacks_mod[[from]], length(stacks_mod[[from]]) - n)
}

paste(map(stacks_mod, ~tail(.x, 1)), collapse = "")


