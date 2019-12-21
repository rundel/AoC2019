library(tidyverse)
library(future)
library(furrr)
library(stringr)
plan(multiprocess, workers=8)

source(here::here("intcode.R"))
input = scan(here::here("day19/input.txt"), sep=",")

mat_to_df = function(m) {
  df = expand.grid(
    y = seq_len(nrow(m)),
    x = seq_len(ncol(m))
  )
  
  df$z = m[as.matrix(df)]
  
  df
}



check_loc = function(x,y) {
  tb = intcode$new(input, c(x,y))$run()
  tb$output
}

df = expand.grid(
  x = seq_len(50)-1,
  y = seq_len(50)-1
) %>%
  as_tibble() %>%
  mutate(
    z = furrr::future_map2_dbl(x, y, check_loc, .progress = TRUE)
  ) %>%
  filter(z != 0)

saveRDS(df, here::here("day19/part1.rds"))



find_lb = function(x, y) {
  cur = check_loc(x,y)
  
  if (cur == 0) {
    y+1
  } else {
    y
  }
}

find_ub = function(x, y) {
  cur = check_loc(x,y+1)
  
  if (cur == 0) {
    y
  } else {
    y+1
  }
}

find_bounds = function(x, y) {
  if (check_loc(x,y) == 0) 
    stop("Bad initial guess")
  
  map_dbl(
    c(-1,1),
    function(dir) {
      repeat {
        res = check_loc(x, y+dir)
        if (res == 0)
          break
        
        y = y + dir
      }
      y
    }
  )
}


res = df %>%
  filter(z != 0) %>%
  filter(x < 10) %>%
  group_by(x) %>%
  summarize(y_lb = min(y), y_ub = max(y)) %>%
  pmap(
    function(x, y_lb, y_ub) {
      c(x, y_lb, y_ub)
    }
  )

for(x in 10:49) {
  i = length(res)
  y_lb = find_lb(x, res[[i]][2])
  y_ub = find_ub(x, res[[i]][3])
  res[[length(res)+1]] = c(x, y_lb, y_ub)
}


df2 = do.call(rbind, res) %>%
  as.data.frame() %>%
  setNames(c("x","y_lb","y_ub")) %>%
  as_tibble()


plot(dplyr::select(df, x, y))
points(dplyr::select(df2, x, y_lb), col='red', pch=16)
points(dplyr::select(df2, x, y_ub), col='blue', pch=16)


start_x = 1200
res = list(c(start_x, find_bounds(start_x, 800)))
for(x in start_x+(1:300)) {
  i = length(res)
  y_lb = find_lb(x, res[[i]][2])
  y_ub = find_ub(x, res[[i]][3])
  res[[i+1]] = c(x, y_lb, y_ub)
}

df3 = do.call(rbind, res) %>%
  as.data.frame() %>%
  setNames(c("x","y_lb","y_ub")) %>%
  as_tibble()

df3 %>%
  pivot_longer(starts_with("y_")) %>%
  ggplot(aes(x=x, y=value, color=name)) +
    geom_point() +
    geom_point(data = data.frame(x = c(1328,1328,1427,1427), value=c(865,964,865,964)), color = "blue", size=0.5) +
    xlim(c(1328, 1427)) +
    ylim(c(865, 964))

df3 %>%
  mutate(
    x1 = x,
    x2 = lead(x, 99),
    y1 = lead(y_lb, 99),
    y2 = y_ub,
    height = y2 - y1 + 1,
    width = x2 - x1 +1
  ) %>%
  filter(height == 100, width == 100) %>%
  slice(1) %>%
  summarize(x1 * 10000 + y1)
