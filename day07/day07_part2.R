library(tidyverse)

n_params = c(3,3,1,1,2,2,3,3)
valid_opcode = c(1:8,99)

parse_cmd = function(cmd) {
  cmd = as.character(cmd)
  parse = stringr::str_match(cmd, "^(\\d*?)(\\d{1,2})$") %>% c()
  
  opcode = parse[3] %>% as.integer()
  
  if (!opcode %in% valid_opcode)
    stop("Invalid opcode: ", opcode, " from command: ", cmd, call. = FALSE)
  
  mode = parse[2] %>% 
    str_pad(n_params[opcode], pad = "0") %>% 
    str_split("") %>% .[[1]] %>%
    as.integer()
  
  list(
    opcode = opcode,
    mode = mode
  )
}

run_program = function(program, input = c()) {
  cur_pos = 0
  last_print = NA
  
  while(program[cur_pos+1] != 99) {
    
    cmd = program[cur_pos+1]
    res = parse_cmd(cmd)
    
    opcode = res$opcode
    mode = res$mode
    
    args = purrr::imap_dbl(
      rev(mode), # Backwards bullshit
      function(mode, i) {
        if (mode == 0) {        # position mode
          program[cur_pos+1 + i]
        } else if (mode == 1) { # immediate mode
          cur_pos + i
        } else {
          stop("Invalid mode.")
        }
      }
    )
    
    #cat(cmd, "->", opcode, " [", paste(mode, collapse=""), "] ", args, "\n")
    
    
    if (opcode == 1) {
      program[args[3]+1] = program[args[1]+1] + program[args[2]+1]
    } else if (opcode == 2) {
      program[args[3]+1] = program[args[1]+1] * program[args[2]+1]
    } else if (opcode == 3) {
      program[args[1]+1] = input[1]
      input = input[-1]
    } else if (opcode == 4) {
      last_print = program[args[1]+1]
      print(program[args[1]+1])
    } else if (opcode == 5) {
      if (program[args[1] + 1] != 0) {
        cur_pos = program[args[2] + 1]
        next
      }
    } else if (opcode == 6) {
      if (program[args[1] + 1] == 0) {
        cur_pos = program[args[2] + 1]
        next
      }
    } else if (opcode == 7) {
      program[args[3]+1] = as.double(program[args[1]+1] < program[args[2]+1])
    } else if (opcode == 8) {
      program[args[3]+1] = as.double(program[args[1]+1] == program[args[2]+1])
    } else {
      stop("Bad opcode: ", opcode, "@", cur_pos)
    }
    
    cur_pos = cur_pos + length(args) + 1 
    
  }
  
  last_print
}

ex1 = c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
ex2 = c(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
ex3 = c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)

run_settings = function(prog, settings) {
  init = 0
  for(s in settings) {
    init = run_program(prog, c(s, init))
  }
  
  init
}

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
    filter(l_set == 5) %>%
    mutate(
      result = map_dbl(settings, run_settings, prog = prog)
    ) %>%
    arrange(desc(result)) %>%
    slice(1)
}

library(testthat)

expect_equal(run_settings(ex1, c(4,3,2,1,0)), 43210)
expect_equal(run_settings(ex2, c(0,1,2,3,4)), 54321)
expect_equal(run_settings(ex3, c(1,0,4,3,2)), 65210)

find_settings(ex1)
find_settings(ex2)
find_settings(ex3)

input = scan(here::here("day07/input.txt"), sep=",")
find_settings(input)



## Part 2

# Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6):
ex4 = c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
ex5 = c(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)

run_program(ex4, c(9,0))
