library(tidyverse)

read_map = function(file) {
  read_file(file) %>%
    str_split("\n") %>%
    .[[1]] %>%
    str_split("") %>%
    do.call(rbind, .)
}

map_to_df = function(map) {
  df = expand.grid(
    y = seq_len(nrow(map)),
    x = seq_len(ncol(map))
  )
  
  df$z = map[as.matrix(df)]
  
  df
}

df_to_map = function(df) {
  df = arrange(df, x, y)
  matrix(df$z, nrow = max(df$y), ncol = max(df$x))
}

available_keys = function(df) {
  d = df %>%
    filter(z != "#") %>%
    filter(!z %in% LETTERS)
  
  loc = which(d$z == "@")
  keys = which(d$z %in% letters)
  
  adj = dist(d[,c("x","y")]) %>%
    as.matrix() 
  adj[adj>1] = 0
  
  g = igraph::graph_from_adjacency_matrix(adj)
  p = suppressWarnings(
    igraph::shortest_paths(g, loc, keys)
  )
  
  
  tibble(
    key = d$z[keys],
    steps = map_int(p$vpath, length)-1,
    keys_in_path = map_int(p$vpath, ~ sum(.x %in% keys))
  ) %>%
    filter(keys_in_path == 1) %>%
    select(-keys_in_path)
}

move = function(df, key) {
  #print(df_to_map(df))
  df %>%
    mutate(
      z = ifelse(z == "@", ".", z),
      z = ifelse(z == key, "@", z),
      z = ifelse(z == toupper(key), ".", z),
    )
}



solve = function(df) {
  avail = available_keys(df) 
  #print(avail)
  #print(df_to_map(df))
  
  if (nrow(avail) == 0) {
    return(0)
  }
  
  pmap(
    avail,
    function(key, steps) {
      new_df = move(df, key)
      steps + solve(new_df)
    }
  ) %>% unlist()
}

ex1 = read_map(here::here("day18/ex1.txt")) %>% map_to_df()
ex2 = read_map(here::here("day18/ex2.txt")) %>% map_to_df()
ex3 = read_map(here::here("day18/ex3.txt")) %>% map_to_df()
ex4 = read_map(here::here("day18/ex4.txt")) %>% map_to_df()
ex5 = read_map(here::here("day18/ex5.txt")) %>% map_to_df()


solve(ex1) %>% min()
solve(ex2) %>% min()
solve(ex3) %>% min()
solve(ex4) %>% min()
solve(ex5) %>% min()


z = solve(input) 
min(z)

