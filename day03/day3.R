library(sf)
library(tidyverse)

wire_ex1 = list(c("R8","U5","L5","D3"), 
                c("U7","R6","D4","L4"))
wire_ex2 = list(c("R75","D30","R83","U83","L12","D49","R71","U7","L72"),
                c("U62","R66","U55","R34","D71","R55","D58","R83"))
wire_ex3 = list(c("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"),
                c("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"))


wire_task1 = readr::read_lines(here::here("day3/input.txt")) %>%
  strsplit(",")

wire_to_sfg = function(wire) {
  cmd  = substring(wire, 1, 1)
  dist = substring(wire, 2) %>% as.integer()
  
  cur_pos = c(x=0,y=0)
  res = purrr::map2(
    cmd, dist,
    function(cmd, dist) {
      if (cmd == "R") {
        change = c( dist,0)
      } else if (cmd == "L") {
        change = c(-dist,0)
      } else if (cmd == "D") {
        change = c(0,-dist)
      } else if (cmd == "U") {
        change = c(0, dist)
      }
      
      cur_pos <<- cur_pos + change
    }
  )
  
  do.call(rbind, c(list(c(x=0,y=0)), res)) %>%
    sf::st_linestring()
}

wires_to_sfc = function(wires) {
  map(wires, wire_to_sfg) %>% st_sfc()  
}

find_pt_intersection_min_dist = function(wires) {
  sfc = wires_to_sfc(wires)
  int = st_intersection(sfc)
  is_pt = purrr::map_lgl(int, inherits, what = "MULTIPOINT")
  
  pts = int[is_pt][[1]] %>% as.matrix()
  
  dist = apply(abs(pts), 1, sum)
  
  min(dist[dist > 0])
}

find_pt_intersection_min_dist = function(wires) {
  sfc = wires_to_sfc(wires)
  segs = st_difference(sfc[[1]], sfc[[2]])
  dist = purrr::map_dbl(
    segs[-1],
    ~ sum(abs(.x)[1,])
  )
  
  min(dist)
}


find_pt_intersection_min_dist(wire_ex1)
find_pt_intersection_min_dist(wire_ex2)
find_pt_intersection_min_dist(wire_ex3)


find_pt_intersection_min_dist(wire_task1)






calc_step_from_seg = function(seg) {
  purrr::map_dfr(
    seq_len(length(seg)-1),
    function(i) {
      c(
        seg[[i+1]][1,] %>% setNames(c("x","y")),
        steps = st_union(seg[1:i]) %>% st_length()
      ) %>% as.list()
    }
  ) %>%
    distinct(x, y, .keep_all = TRUE)
}

find_min_step_intersection = function(wire) {
  
  sfc = wires_to_sfc(wire) 
  seg1 = st_difference(sfc[[1]], sfc[[2]]) %>% st_sfc() %>% st_cast("LINESTRING")
  seg2 = st_difference(sfc[[2]], sfc[[1]]) %>% st_sfc() %>% st_cast("LINESTRING")
  
  df1 = calc_step_from_seg(seg1)
  df2 = calc_step_from_seg(seg2)

  full_join(df1, df2, by = c("x", "y")) %>%
    mutate(total = steps.x + steps.y) %>%
    arrange(total)
}

  


find_min_step_intersection(wire_ex1)
find_min_step_intersection(wire_ex2)
find_min_step_intersection(wire_ex3)


find_min_step_intersection(wire_task1)
