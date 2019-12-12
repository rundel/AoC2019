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

robot = R6::R6Class(
  "robot",
  public = list(
    initialize = function(program, init_color = 0) {
      private$cpu = intcode$new(program)
      private$pos = c(0,0)
      private$dir = c(0,1)
      private$path = tibble(pos_x = integer(), pos_y = integer(), color = integer())
      private$init_color = init_color
    },
    
    get_color = function(loc) {
      if (nrow(private$path) == 0)
        return(private$init_color)
      
      loc_ops = private$path %>%
        filter(pos_x == loc[1], pos_y == loc[2]) %>%
        slice(n())
      
      if (nrow(loc_ops) == 0)
        return(0)
      
      loc_ops %>% pull(color)
    },
    
    run = function() {
      j = 0
      repeat {
        
        #cat(self$get_color(private$pos), "\n")
        private$cpu$input = self$get_color(private$pos)
        private$cpu$run()
        
        if (private$cpu$state == "finished")
          break
        
        color = as.integer(private$cpu$output[1])
        dir   = as.integer(private$cpu$output[2])
        private$cpu$reset_output()
        
        #cat(color, ", ", dir, ", ", private$cpu$output, "\n")
        
        step = c(private$pos, color) %>% setNames(names(private$path))
        
        private$path = bind_rows(private$path, step)
        
        if (dir == 0) {
          R = matrix(c(0,1,-1,0), 2)
        } else {
          R = matrix(c(0,-1,1,0), 2)
        }
        private$dir = (R %*% private$dir) %>% c()
        
        private$pos = private$pos + private$dir
        #self$print()
      }
      
      self
    },
    
    print = function() {
      usethis:::kv_line("pos", private$pos)
      usethis:::kv_line("dir", private$dir)
      #usethis:::kv_line("path", NULL)
      tibble:::print.tbl(private$path)
    },
    
    n_painted = function() {
      distinct(private$path, pos_x, pos_y)
    },
    
    path_image = function() {
      crds = private$path %>%
        mutate(
          pos_x = pos_x - min(pos_x) + 1,
          pos_y = pos_y - min(pos_y) + 1,
        )
      
      image = matrix(0, max(crds$pos_y), max(crds$pos_x))
      
      pwalk(
        crds,
        ~ {
          image[..2, ..1] <<- ..3
        }
      ) 
      
      t(image)
    }
    
  ),
  private = list(
    cpu = NULL,
    pos = NULL,
    dir = NULL,
    path = NULL,
    init_color = NULL
  )
)

program = scan(here::here("day11/input.txt"), sep=",")
r = robot$new(program, init_color=0)
r$run()$n_painted()


r2 = robot$new(program, init_color=1)
r2$run()$path_image() %>% image()

# LBJHEKLH


