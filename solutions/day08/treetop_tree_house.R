library(tidyverse)
n = read_lines("solutions/day08/input", n_max = 1) %>% str_count()
trees = read.fwf("solutions/day08/input", rep(1, n))

# Part I
vis = 4*n - 4 # outers
for (i in 2:(n-1)){
  for (j in 2:(n-1)){
    current = trees[i, j]
    if (current > max(trees[i, (j-1):1])){vis = vis + 1; next} # look left
    if (current > max(trees[i, (j+1):n])){vis = vis + 1; next} # look right
    if (current > max(trees[(i-1):1, j])){vis = vis + 1; next} # look up
    if (current > max(trees[(i+1):n, j])){vis = vis + 1; next} # look down
  }
}
vis
