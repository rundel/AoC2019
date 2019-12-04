

run_opcode = function(opcode) {
  cur_cmd = 0
  while(opcode[cur_cmd+1] != 99) {
    if (opcode[cur_cmd+1] == 99)
      break
    
    cmd = opcode[cur_cmd+1]
    x_pos = opcode[cur_cmd+2]
    y_pos = opcode[cur_cmd+3]
    save_pos = opcode[cur_cmd+4]
    
    if (cmd == 1) {
      opcode[save_pos+1] = opcode[x_pos+1] + opcode[y_pos+1]
    } else if (cmd == 2) {
      opcode[save_pos+1] = opcode[x_pos+1] * opcode[y_pos+1]
    } else {
      stop("Bad opcode:", cmd, "@", cur_cmd)
    }
    
    cur_cmd = cur_cmd + 4
  }
  
  opcode
}

run_opcode( c(1,0,0,0,99) )
run_opcode( c(2,3,0,3,99) )
run_opcode( c(2,4,4,5,99,0) )
run_opcode( c(1,1,1,4,99,5,6,0,99) )

input = scan(here::here("day2/input.txt"), sep=",")

input[1+1] = 12
input[2+1] = 2

run_opcode(input)


find_noun_verb = function(input, output = 19690720) {
  for(noun in 0:99) {
    for(verb in 0:99) {
      input[1+1] = noun
      input[2+1] = verb
      
      
      if (output == run_opcode(input)[1]) {
        print(glue::glue("Noun: {noun}, Verb: {verb}")) 
        return(invisible(NULL))
      }
    }
  }
}

find_noun_verb(input)
