library(tidyverse)

input = scan(here::here("day05/input.txt"), sep=",")

n_params = c(3,3,1,1,2,2,3,3)
valid_opcode = c(1,2,3,4,99)


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

parse_cmd(1)
parse_cmd(10)
parse_cmd(101)
parse_cmd(1001)


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
      #cat("Adding\n")
      program[args[3]+1] = program[args[1]+1] + program[args[2]+1]
    } else if (opcode == 2) {
      #cat("Multiplying\n")
      program[args[3]+1] = program[args[1]+1] * program[args[2]+1]
    } else if (opcode == 3) {
      #cat("Getting input\n")
      program[args[1]+1] = input[1]
      input = input[-1]
    } else if (opcode == 4) {
      #cat("Printing\n")
      print(program[args[1]+1])
    } else {
      stop("Bad opcode: ", opcode, "@", cur_pos)
    }
    
    cur_pos = cur_pos + length(args) + 1 
  }
  
  program
}

#run_program( c(1,0,0,0,99) )
#run_program( c(2,3,0,3,99) )
#run_program( c(2,4,4,5,99,0) )
#run_program( c(1,1,1,4,99,5,6,0,99) )
#run_program( c(1002, 4, 3, 4, 33) )

run_program(c(1101,100,-1,4,0))

run_program( input, 1 )

