library(tidyverse)

input = scan(here::here("day05/input.txt"), sep=",")

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
  
#  program
}

library(testthat)

# consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
expect_output( run_program(c(3,9,8,9,10,9,4,9,99,-1,8), 8), "\\[1\\] 1")
expect_output( run_program(c(3,9,8,9,10,9,4,9,99,-1,8), 7), "\\[1\\] 0")

#consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
expect_output( run_program(c(3,9,7,9,10,9,4,9,99,-1,8), 8), "\\[1\\] 0")
expect_output( run_program(c(3,9,7,9,10,9,4,9,99,-1,8), 7), "\\[1\\] 1")

# Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).

expect_output( run_program(c(3,3,1108,-1,8,3,4,3,99), 8), "\\[1\\] 1")
expect_output( run_program(c(3,3,1108,-1,8,3,4,3,99), 7), "\\[1\\] 0")

# Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
expect_output( run_program(c(3,3,1107,-1,8,3,4,3,99) , 8), "\\[1\\] 0")
expect_output( run_program(c(3,3,1107,-1,8,3,4,3,99) , 7), "\\[1\\] 1")


#Here are some jump tests that take an input, then output 0 if the input was zero or 1 if the input was non-zero:
  
#(using position mode)
run_program(c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 0)
run_program(c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 1)
run_program(c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 8)

# (using immediate mode)
run_program(c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), 0)
run_program(c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), 1)
run_program(c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), 8)



# Larger example:
long_prog =  c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)

run_program(long_prog, 7)
run_program(long_prog, 8)
run_program(long_prog, 9)
 

run_program( input, 5 )

