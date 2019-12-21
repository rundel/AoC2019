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
          stringr::str_pad(self$n_params[opcode], pad = "0") %>% 
          stringr::str_split("") %>% .[[1]] %>%
          as.integer()
      }
      
      list(
        opcode = opcode,
        mode = mode
      )
    }
  )
)