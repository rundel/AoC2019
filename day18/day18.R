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

ex1 = read_map(here::here("day18/ex1.txt")) %>% map_to_df()
ex2 = read_map(here::here("day18/ex2.txt")) %>% map_to_df()
ex3 = read_map(here::here("day18/ex3.txt")) %>% map_to_df()
ex4 = read_map(here::here("day18/ex4.txt")) %>% map_to_df()
ex5 = read_map(here::here("day18/ex5.txt")) %>% map_to_df()
input = read_map(here::here("day18/input.txt")) %>% map_to_df()



make_symm = function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  m
}

get_mats = function(df) {
  d = df %>%
    filter(z != "#")
  
  locs = which(d$z %in% c("@", letters, 1:4))
  name = d$z[locs]
  
  adj = dist(d[,c("x","y")]) %>%
    as.matrix() 
  adj[adj>1] = 0
  
  g = igraph::graph_from_adjacency_matrix(adj)
  
  dist_mat = matrix(NA, length(locs), length(locs), dimnames = list(name,name))
  gate_mat = matrix(NA, length(locs), length(locs), dimnames = list(name,name))
  
  for(i in seq_along(locs)) {
    sub = i:length(locs)
    
    p = suppressWarnings(
      igraph::shortest_paths(g, locs[i], locs[sub])
    )
    dist_mat[sub, i] = map_dbl(p$vpath, ~max(length(.x)-1, 0))
    gate_mat[sub, i] = map_chr(
      p$vpath,
      function(path) {
        steps = d$z[path]
        paste(steps[steps %in% LETTERS], collapse = "")
      }
    )
  }
  
  list(
    dist_mat = make_symm(dist_mat),
    gate_mat = make_symm(gate_mat)
  )
}

mats = get_mats(ex2)

get_mask = function(pos, mats, keys="") {
  if (keys == "")
    1*(mats$gate_mat[pos,] == keys)
  else
    1*map_lgl(mats$gate_mat[pos,], str_detect, pattern = glue::glue("^[{keys}]+$")) +
    1*(mats$gate_mat[pos,] == "")
}

available_keys = function(pos, mats, keys="") {
  map_dfr(
    pos,
    function(pos) {
      #print(mats)
      #print(pos)
      #print(keys)
      all_keys = mats$dist_mat[pos,] * get_mask(pos, mats, keys) 
      steps = all_keys[all_keys != 0]
      
      tibble(
        from = pos,
        to = names(steps),
        steps = steps
      )
    }
  )
}

del_rc = function(mat, n) {
  i = which(row.names(mat) == n)
  mat = mat[-i,,drop = FALSE]
  mat[,-i, drop = FALSE]
}

del_node = function(mats, node) {
  list(
    dist_mat = del_rc(mats$dist_mat, node),
    gate_mat = del_rc(mats$gate_mat, node)
  )
}

sort_keys = function(keys) {
  str_split(keys, "") %>%
    map_chr(
      ~ sort(.x) %>% paste(collapse="")
    )
}

get_pos = function(df) {
  df %>%
    filter(!z %in% LETTERS) %>%
    filter(!z %in% letters) %>%
    filter(!z %in% c(".","#")) %>%
    pull(z)
}


solve = function(df) {
  
  n = sum(df$z %in% letters)
  
  res = list(
    list(
      mats = get_mats(df),
      pos = get_pos(df),
      keys = "",
      steps = 0
    )
  )
  
  
  for(i in seq_len(n)) {
    cat(glue::glue("Iter {i} of {n}, checking {length(res)}."), "\n")
    
    moves = map(
      res,
      function(state) {
        ak = available_keys(state$pos, state$mats, state$keys)
        pmap(
          ak,
          function(from, to, steps) {
            pos = state$pos
            pos[pos == from] = to
            list(
              mats = del_node(state$mats, from),
              pos = pos,
              keys = paste0(state$keys, toupper(to)),
              steps = state$steps + steps
            )
          }
        )
      }
    )
    
    moves = flatten(moves)
    
    orig_keys = map_chr(moves, "keys")
    keys = paste(
      map_chr(moves, "keys") %>% sort_keys(),
      map(moves, "pos") %>% map_chr(paste, collapse=""),
      sep = ":"
    )
    steps = map_dbl(moves, "steps")
    
    dupes = keys[duplicated(keys)] %>% unique()
    to_rm = c() 
    
    for(dupe in dupes) {
      idx = which(keys == dupe)
      best = idx[which.min(steps[idx])]
      to_rm = append(to_rm, idx[idx!=best])
    }
    #print(orig_keys)
    #print(keys)
    #print(steps)
    #print(to_rm)
    
    if (length(to_rm) != 0)
      moves = moves[-to_rm]
    
    res = moves
    #print(map_chr(res, "keys"))
  }
  
  
  map_dbl(res, "steps") %>%
    setNames(map_chr(res, "keys"))
}








#solve(ex1) # 8
#solve(ex2) # 86
#solve(ex3) # 132
#solve(ex4) # 136
#solve(ex5) # 81
#
#solve(input) 


### Part2

input_part2 = {
  m = read_map(here::here("day18/input.txt"))
  orig = which(m == "@", arr.ind = TRUE)
  y = orig[1]
  x = orig[2]
  
  m[y-1,x+(-1:1)] = c("1","#","2")
  m[y+0,x+(-1:1)] = c("#","#","#")
  m[y+1,x+(-1:1)] = c("3","#","4")
  map_to_df(m)
}

ex2_1 = read_map(here::here("day18/ex2_1.txt")) %>% map_to_df()
ex2_2 = read_map(here::here("day18/ex2_2.txt")) %>% map_to_df()
ex2_3 = read_map(here::here("day18/ex2_3.txt")) %>% map_to_df()

solve(ex2_1) #8
solve(ex2_2) #24
solve(ex2_3) #32

solve(input_part2)
