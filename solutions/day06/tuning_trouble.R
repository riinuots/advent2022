library(tidyverse)

input_orig = read_file("solutions/day06/input")

# Parts I and II
# changed 3 to 13 manually
for (i in 1:str_count(input_orig)){
  signal = str_sub(input_orig, i, i + 13) %>% 
    str_split("") %>% 
    table()
  if (all(signal == 1)){
    print("Marker after:");
    print(i + 13)
    break
  }
}
