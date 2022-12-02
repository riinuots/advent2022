library(tidyverse)

input_orig = read_table("solutions/day02/input",
                        col_names = c("elf_code", "me_code"))

# Part I 
me_playing = tribble(~me_code,   ~me_real,    ~points_playing,
                     "X",      "Rock",       1,
                     "Y",      "Paper",      2,
                     "Z",      "Scissors",   3)

elf_playing = tribble(~elf_code,   ~elf_real,
                      "A",      "Rock",
                      "B",      "Paper",
                      "C",      "Scissors")

game_results = crossing(elf = c("Rock", "Paper", "Scissors"),
                        me  = c("Rock", "Paper", "Scissors")) %>% 
  mutate(points_winning = c(3, 0, 6, 6, 3, 0, 0, 6, 3)) %>% 
  unite("game", elf, me)

input_orig %>% 
  left_join(me_playing) %>% 
  left_join(elf_playing) %>% 
  unite("game", elf_real, me_real) %>% 
  left_join(game_results) %>% 
  summarise(points_playing  = sum(points_playing),
            points_winning = sum(points_winning)) %>% 
  mutate(points_playing + points_winning)

# Part II

game_results2 = tribble(~me_code,   ~result,    ~points_winning,
                     "X",          "Lose",       0,
                     "Y",          "Draw",      3,
                     "Z",          "Win",       6)

me_toplay = crossing(elf_real = c("Rock", "Paper", "Scissors"),
                      result  = c("Lose", "Draw", "Win")) %>% 
  mutate(me_real = c("Paper",    "Rock",     "Scissors",
                     "Rock",     "Scissors", "Paper",
                     "Scissors", "Paper",    "Rock"))

input_orig %>% 
  left_join(elf_playing) %>% 
  left_join(game_results2) %>% 
  left_join(me_toplay) %>% 
  left_join(select(me_playing, me_real, points_playing)) %>% 
  summarise(points_playing  = sum(points_playing),
            points_winning = sum(points_winning)) %>% 
  mutate(points_playing + points_winning)
