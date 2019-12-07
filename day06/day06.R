ex1 = c("COM)B", "B)C", "C)D","D)E", "E)F","B)G", "G)H","D)I", "E)J", "J)K", "K)L")

library(tidygraph)
library(tidyverse)

to_tidy_graph = function(orbits) {
 orbits %>% 
    str_split("\\)") %>%
    do.call(rbind, .) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("from","to")) %>%
    tidygraph::tbl_graph(edges = ., )
} 

count_orbits = function(g) {
  g %>%
    to_tidy_graph() %>%
    activate(nodes) %>%
    mutate(
      root = node_is_root(),
      depth = bfs_dist(root = which(root))
    ) %>%
    pull(depth) %>%
    sum()
}

count_orbits(ex1)

input = readLines(here::here("day06/input.txt"))
count_orbits(input)  


ex2 = c("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN")

count_transfers = function(g) {
  g = g %>%
    to_tidy_graph()
  
  nodes = g %>% pull(name) %>% {which(. %in% c("YOU","SAN"))}
  
  g %>% 
    activate(nodes) %>%
    to_shortest_path(nodes[1],nodes[2],mode="all") %>%
    .$shortest_path %>%
    igraph::gsize() - 2
}

count_transfers(ex2)
count_transfers(input)
