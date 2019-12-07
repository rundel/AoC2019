library(tidyverse)

intcode = R6::R6Class(
  "intcode",
  public = list(
    valid_opcode = c(1:8,99),
    n_params = c(3,3,1,1,2,2,3,3),
    state = NA,
    output = c(),
    input = c(),
    program = c(),
    cur_pos = 0,
    
    initialize = function(program, input = c()) {
      self$program = program
      self$input  = input
      
      self$cur_pos = 0
      self$state = "initialized"
    },
    
    run = function() {
      
      repeat {
        cmd = self$program[self$cur_pos+1]
        res = private$parse_cmd(cmd)
        
        opcode = res$opcode
        mode = res$mode
        
        args = purrr::imap_dbl(
          rev(mode), # Backwards bullshit
          function(mode, i) {
            if (mode == 0) {        # position mode
              self$program[self$cur_pos+1 + i]
            } else if (mode == 1) { # immediate mode
              self$cur_pos + i
            } else {
              stop("Invalid mode.")
            }
          }
        )
        
        #cat(cmd, "->", opcode, " [", paste(mode, collapse=""), "] ", args, "\n")
        
        
        if (opcode == 1) {
          self$program[args[3]+1] = self$program[args[1]+1] + self$program[args[2]+1]
        } else if (opcode == 2) {
          self$program[args[3]+1] = self$program[args[1]+1] * self$program[args[2]+1]
        } else if (opcode == 3) {
          if (length(self$input) == 0) {
            self$state = "waiting for input"
            break
          }
          self$program[args[1]+1] = self$input[1]
          self$input = self$input[-1]
        } else if (opcode == 4) {
          #self$output = append(self$output, self$program[args[1]+1])
          self$output = self$program[args[1]+1]
        } else if (opcode == 5) {
          if (self$program[args[1] + 1] != 0) {
            self$cur_pos = self$program[args[2] + 1]
            next
          }
        } else if (opcode == 6) {
          if (self$program[args[1] + 1] == 0) {
            self$cur_pos = self$program[args[2] + 1]
            next
          }
        } else if (opcode == 7) {
          self$program[args[3]+1] = as.double(self$program[args[1]+1] < self$program[args[2]+1])
        } else if (opcode == 8) {
          self$program[args[3]+1] = as.double(self$program[args[1]+1] == self$program[args[2]+1])
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
      usethis:::kv_line("Program", self$program)
      usethis:::kv_line("cur_pos", self$cur_pos)
      usethis:::kv_line("State  ", self$state)
      usethis:::kv_line("input  ", self$input)
      usethis:::kv_line("output ", self$output)
    }
    
  ),
  private = list(
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


run_settings = function(prog, settings) {
  A = intcode$new(prog, c(settings[1], 0))$run()
  B = intcode$new(prog, c(settings[2], A$output))$run()
  C = intcode$new(prog, c(settings[3], B$output))$run()
  D = intcode$new(prog, c(settings[4], C$output))$run()
  E = intcode$new(prog, c(settings[5], D$output))$run()
  
  repeat {
    if (E$state == "finished") {
      return(E$output)
    }
    
    A$input = E$output; A$run()
    B$input = A$output; B$run()
    C$input = B$output; C$run()
    D$input = C$output; D$run()
    E$input = D$output; E$run()
  }
}

ex1_settings = c(9,8,7,6,5)
ex1 = c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)

run_settings(ex1, ex1_settings) # 139629729

ex2_settings = c(9,7,8,5,6)
ex2 = c(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)

run_settings(ex2, ex2_settings) # 18216


find_settings = function(prog, setting_range = 0:4) {
  expand_grid(
    A = setting_range,
    B = setting_range,
    C = setting_range,
    D = setting_range,
    E = setting_range
  ) %>%
    mutate(
      settings = pmap(., ~c(..1, ..2, ..3, ..4, ..5)),
      l_set = map_int(settings, ~length(unique(.x)))  
    ) %>%
    filter(l_set == length(setting_range)) %>%
    mutate(
      result = map_dbl(settings, run_settings, prog = prog)
    ) %>%
    arrange(desc(result)) %>%
    slice(1)
}

input = scan(here::here("day07/input.txt"), sep=",")
find_settings(input, 5:9)