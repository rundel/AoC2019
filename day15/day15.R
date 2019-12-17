library(raster)
library(tidyverse)

intcode = R6::R6Class(
  "intcode",
  public = list(
    valid_opcode = c(1:9,99),
    n_params = c(3,3,1,1,2,2,3,3,1),
    state = NA,
    output = c(),
    input = c(),
    program = c(),
    cur_pos = 0,
    rel_base = 0,
    
    initialize = function(program, input = c()) {
      self$program = program
      self$input  = input
      
      self$cur_pos = 0
      self$rel_base = 0
      self$state = "initialized"
    },
    
    run = function() {
      
      repeat {
        cmd = private$get_val(self$cur_pos)
        res = private$parse_cmd(cmd)
        
        opcode = res$opcode
        mode = res$mode
        
        args = purrr::imap_dbl(
          rev(mode), # Backwards bullshit
          function(mode, i) {
            if (mode == 0) {        # position mode
              private$get_val(self$cur_pos + i)
            } else if (mode == 1) { # immediate mode
              self$cur_pos + i
            } else if (mode == 2) { # relative mode
              private$get_val(self$cur_pos + i) + self$rel_base
            } else {
              stop("Invalid mode.")
            }
          }
        )
        
        #cat(cmd, "->", opcode, " [", paste(mode, collapse=""), "] ", args, "\n")
        
        
        if (opcode == 1) {
          self$program[args[3]+1] = private$get_val(args[1]) + private$get_val(args[2])
        } else if (opcode == 2) {
          self$program[args[3]+1] = private$get_val(args[1]) * private$get_val(args[2])
        } else if (opcode == 3) {
          if (length(self$input) == 0) {
            self$state = "waiting for input"
            break
          }
          self$program[args[1]+1] = self$input[1]
          self$input = self$input[-1]
        } else if (opcode == 4) {
          self$output = append(self$output, private$get_val(args[1]))
        } else if (opcode == 5) {
          if (private$get_val(args[1]) != 0) {
            self$cur_pos = private$get_val(args[2])
            next
          }
        } else if (opcode == 6) {
          if (private$get_val(args[1]) == 0) {
            self$cur_pos = private$get_val(args[2])
            next
          }
        } else if (opcode == 7) {
          self$program[args[3]+1] = as.double(private$get_val(args[1]) < private$get_val(args[2]))
        } else if (opcode == 8) {
          self$program[args[3]+1] = as.double(private$get_val(args[1]) == private$get_val(args[2]))
        } else if (opcode == 9) { 
          self$rel_base = self$rel_base + private$get_val(args[1])
        } else if (opcode == 99) {
          self$state = "finished"
          break
        } else {
          stop("Bad opcode: ", opcode, "@", self$cur_pos)
        }
        
        self$cur_pos = self$cur_pos + length(args) + 1 
        
      }
      
      self
    },
    print = function() {
      usethis:::kv_line("Program ", self$program)
      usethis:::kv_line("cur_pos ", self$cur_pos)
      usethis:::kv_line("rel_base", self$rel_base)
      usethis:::kv_line("State   ", self$state)
      usethis:::kv_line("input   ", self$input)
      usethis:::kv_line("output  ", self$output)
    },
    
    reset_output = function() {
      self$output = character()
    }
    
  ),
  private = list(
    get_val = function(i) {
      val = self$program[i+1]
      if (is.na(val))
        val = 0
      
      val
    },
    
    parse_cmd = function(cmd) {
      cmd = as.character(cmd)
      parse = stringr::str_match(cmd, "^(\\d*?)(\\d{1,2})$") %>% c()
      
      opcode = parse[3] %>% as.integer()
      
      if (!opcode %in% self$valid_opcode)
        stop("Invalid opcode: ", opcode, " from command: ", cmd, call. = FALSE)
      
      if (opcode == 99) {
        mode = 0
      } else {
        mode = parse[2] %>% 
          str_pad(self$n_params[opcode], pad = "0") %>% 
          str_split("") %>% .[[1]] %>%
          as.integer()
      }
      
      list(
        opcode = opcode,
        mode = mode
      )
    }
  )
)

repair_bot = R6::R6Class(
  "repair_bot",
  public = list(
    initialize = function(prg) {
      private$intcode = intcode$new(prg)
      self$map = tibble(x = 0, y = 0, z = "S")
      private$pos = c(0,0)
    },
    
    explore = function(init_dir = 1) {
      i = 1
      dirs = list( #north (1), south (2), west (3), and east (4)
        c( 0, 1),
        c( 0,-1),
        c(-1, 0),
        c( 1, 0)
      )
      
      check_neighbors = function(pos) {
        left_join(
          imap_dfr(
            dirs,
            ~list(dir=.y, x=.x[1]+pos[1], y=.x[2]+pos[2])
          ),
          self$map,
          by = c("x", "y")
        )
        
      }
      
      rhr_dir = c(
        4, # if north go east
        3, # if south go west
        1, # if west go north
        2  # if east go south
      )
      
      lhr_dir = c(
        3,
        4,
        2,
        1
      )
      max = 1e5
      cur_dir = init_dir
      
      repeat {
        private$intcode$input = cur_dir
        private$intcode$run()
        out = private$intcode$output
        private$intcode$reset_output()
        
        #cat(private$pos, cur_dir, " : ", out,  "\n")
      
        if (out == 0) {
          self$map = bind_rows(
            self$map,
            list(x = private$pos[1]+dirs[[cur_dir]][1], 
                 y = private$pos[2]+dirs[[cur_dir]][2], 
                 z = "#")
          ) %>%
            distinct()
          cur_dir = lhr_dir[cur_dir]
        } else if (out == 1) {
          private$pos = private$pos + dirs[[cur_dir]]
          self$map = bind_rows(
            self$map,
            list(x = private$pos[1], y = private$pos[2], z = ".")
          ) %>% 
            distinct()
          cur_dir = rhr_dir[cur_dir]
        } else if (out == 2) {
          private$pos = private$pos + dirs[[cur_dir]]
          self$map = bind_rows(
            self$map,
            list(x = private$pos[1], 
                 y = private$pos[2], 
                 z = "O")
          )
          cur_dir = rhr_dir[cur_dir]
          max = min(4*i, max)
        } else {
          stop("Bad direction")
        }
        
        neigh = check_neighbors(private$pos)
        
        unexplored = which(is.na(neigh$z))
        walkable = which(neigh$z == ".")
        
        #if (length(unexplored) != 0) {
        #  cur_dir = sample(unexplored, 1)
        #} else {
        #  cur_dir = sample(walkable, 1)
        #}
        
        
        
        i = i+1
        
        if (i %% 100 == 0) {
          self$draw_map()
        }
        
        if (i > max) 
          break
        
      }
    },
    map = NULL,
    draw_map = function() {
      self$map %>%
        bind_rows(
          list(x = private$pos[1], y = private$pos[2], z="C")
        ) %>%
        mutate(z = as.factor(z) %>% as.integer()) %>%
        as.matrix() %>%
        raster::rasterFromXYZ() %>%
        plot()
    }
  ),
  private = list(
    intcode = NULL,
    pos = NULL,
    prev_pos = NULL
  )
)
  
input = scan(here::here("day15/input.txt"), sep=",")

rb = repair_bot$new(input)
rb$explore()
rb$map

start = rb$map %>%
  filter(z != "#") %>%
  {which(.$z == "S")}
end = rb$map %>%
  filter(z != "#") %>%
  {which(.$z == "O")}

adj = rb$map %>%
  filter(z != "#") %>%
  dplyr::select(-z) %>%
  dist() %>%
  as.matrix()

adj[adj > 1] = 0 

g = igraph::graph_from_adjacency_matrix(adj) 

path = igraph::shortest_paths(g, start, end) 
length(path$vpath[[1]])-1

all = igraph::all_shortest_paths(g, end)
map_int(all$res, length) %>% max() %>% .-1
