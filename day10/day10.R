library(tidyverse)

parse_asteroid = function(text) {
  text = str_split(text, "\n")[[1]] %>% str_trim()
  nr = length(text)
  
  str_split(text, "") %>% 
    unlist() %>%
    matrix(nrow = nr, byrow = TRUE)
}

get_asteroids = function(map) {
  map %>% 
    {which(. == "#", arr.ind = TRUE)}
}

calc_los = function(map, index = 1) {
  ast = get_asteroids(map)
  pos = ast[index, ]
  ast = ast[-index, ] %>% as.data.frame()
  
  ast %>% 
    mutate(
      dist = sqrt( (row - pos[1])^2 + (col-pos[2])^2 ),
      delta = pmap(., ~c(.x,.y) - pos) %>% map(~ (.x / abs(numbers::mGCD(.x))) %>% setNames(c("dy","dx")))
    ) %>%
    unnest_wider(delta) %>%
    arrange(dist)
}

los_map = function(los, map) {
  los %>%
    select(row, col) %>%
    pwalk(
      function(row, col) {
        map[row,col] <<- "O"
      }
    )
  
   map
}


find_best = function(map) {
  n_ast = get_asteroids(map) %>% nrow()
  
  viewable = map_int(
    seq_len(n_ast),
    ~ {calc_los(map, .x) %>% 
        distinct(dy, dx, .keep_all = TRUE) %>%
        nrow()
      }
  )
  
  cat("index: ", which.max(viewable),"\n")
  cat("n vis: ", viewable[which.max(viewable)],"\n")
  get_asteroids(map)[which.max(viewable),]
}


translate_pos = function(pos) {
  names(pos) = NULL
  rev(pos)-1
}

ex1 = parse_asteroid(
 ".#..#
  .....
  #####
  ....#
  ...##"
)

find_best(ex1) %>% translate_pos()

ex2 = parse_asteroid(
  "......#.#.
   #..#.#....
   ..#######.
   .#.#.###..
   .#..#.....
   ..#....#.#
   #..#....#.
   .##.#..###
   ##...#..#.
   .#....####"
)

find_best(ex2)

ex3 = parse_asteroid(
  "#.#...#.#.
   .###....#.
   .#....#...
   ##.#.#.#.#
   ....#.#.#.
   .##..###.#
   ..#...##..
   ..##....##
   ......#...
   .####.###."
)

find_best(ex3) %>% translate_pos()

ex4 = parse_asteroid(
  ".#..#..###
   ####.###.#
   ....###.#.
   ..###.##.#
   ##.##.#.#.
   ....###..#
   ..#.#..#.#
   #..#.#.###
   .##...##.#
   .....#.#.."
)

find_best(ex4) %>% translate_pos()

ex5 = parse_asteroid(
  ".#..##.###...#######
   ##.############..##.
   .#.######.########.#
   .###.#######.####.#.
   #####.##.#.##.###.##
   ..#####..#.#########
   ####################
   #.####....###.#.#.##
   ##.#################
   #####.##.###..####..
   ..######..##.#######
   ####.##.####...##..#
   .#####..#.######.###
   ##...#.##########...
   #.##########.#######
   .####.#.###.###.#.##
   ....##.##.###..#####
   .#.#.###########.###
   #.#.#.#####.####.###
   ###.##.####.##.#..##"
)

find_best(ex5) %>% translate_pos()

input = readr::read_file(here::here("day10/input.txt")) %>% str_trim() %>% parse_asteroid()

find_best(input) 



ex2_1 = parse_asteroid(
  ".#....#####...#..
   ##...##.#####..##
   ##...#...#.#####.
   ..#.....#...###..
   ..#.#.....#....##"
)

target_list = function(map, index=16) {
  map %>%
    calc_los(index) %>%
    mutate(
      angle = (90 + atan2(dy,dx) *360 / (2*pi)) %% 360 ,
      angle = ifelse(angle < 0, angle + 360, angle)
    ) %>%
    arrange(angle) %>%
    group_by(dy,dx) %>%
    mutate(group_order = seq_len(n())) %>%
    ungroup() %>%
    arrange(group_order, angle)
}

find_best(ex5)
target_list(ex5, 174)[200,]

targets = input %>% target_list(277)
targets[200,]



