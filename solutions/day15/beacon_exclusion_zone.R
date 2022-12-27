library(tidyverse)
library(scales)
theme_set(theme_bw())
library(ggforce)
library(unglue)


input_orig = read_lines("solutions/day15/input-test")

sensors = input_orig %>% 
  unglue_data("Sensor at x={x}, y={y}: closest beacon is at x={bx}, y={by}", convert = TRUE)

# Plot for inspiration
sensors %>% 
  mutate(dist = abs(x - bx) + abs(y - by)) %>% 
  ggplot() +
  geom_point(aes(x, -y), colour = "blue") +
  geom_point(aes(bx, -by), colour = "red") +
  geom_circle(aes(x0 = x, y0 = -y, r = dist), fill = "green", alpha = 0.2) +
  coord_fixed(xlim = c(0, 20), ylim = c(-20, 0), expand = 0)

# Part I
sensors %>% 
  mutate(dist = abs(x - bx) + abs(y - by)) %>% 
  mutate(dy = 2000000 - y,
         dx = dist - abs(dy)) %>% 
  mutate(endx1 = x - dx, endx2 = x + dx) %>% 
  summarise(minx = min(endx1), maxx = max(endx2)) %>% 
  mutate(abs(minx) + maxx)



