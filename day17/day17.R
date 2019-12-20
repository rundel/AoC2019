library(tidyverse)

source(here::here("intcode.R"))

input = scan(here::here("day17/input.txt"), sep=",")

ascii = intcode$new(input)
ascii$run()

camera_to_mat = function(output) {
  chars = intToUtf8(output)
  lines = strsplit(chars, "\n")[[1]]
  lines = lines[-length(lines)]
  
  n_col = nchar(lines[1])
  n_row = length(lines)
  mat = lines %>% 
    strsplit("") %>% 
    unlist() %>%
    matrix(nrow=n_row, ncol=n_col, byrow=TRUE)
}

find_intersections = function(mat) {
  mat_xp1 = t(apply(mat, 1, lag))
  mat_xm1 = t(apply(mat, 1, lead))
  
  mat_yp1 = apply(mat, 2, lag)
  mat_ym1 = apply(mat, 2, lead)
  
  intersects = which(mat == "#" &
                       mat == mat_xp1 & mat == mat_xm1 & 
                       mat == mat_yp1 & mat == mat_ym1, 
                     arr.ind = TRUE)
  
  (intersects - 1) %>% 
    apply(1, prod) %>%
    sum()
}

mat = camera_to_mat(ascii$output)
find_intersections(mat)

orientation = list(
  "^" = c(0, -1), 
  "v" = c(0,  1), 
  "<" = c(-1, 0), 
  ">" = c( 1, 0)
)
find_start = function(mat) {
  d = dim(mat)
  sub = mat %in% c("^", "v", "<",  ">")
  dim(sub) = d
  
  rev(c(which(sub, arr.ind = TRUE)))
}

turn_left = function(dir) {
  if (dir[1] == -1 & dir[2] == 0) {
    c(0,1)
  } else if (dir[1] == 1 & dir[2] == 0) {
    c(0,-1)
  } else if (dir[1] == 0 & dir[2] == 1) {
    c(1,0)
  } else if (dir[1] == 0 & dir[2] == -1) {
    c(-1,0)
  }
}

turn_right = function(dir) {
  -turn_left(dir)
}


find_moves = function(mat) {
  pos = find_start(mat)
  robot_char = mat[pos[2],pos[1]]
  dir = orientation[[robot_char]]
  
  moves = character()
  
  repeat {
    old_dir = dir
    dir = turn_left(dir)
    new_L = pos + dir
    new_R = pos - dir
    
    #cat("pos:", pos, "old:", old_dir, ", new:", dir,"\n")
    
    if (mat[new_L[2], new_L[1]] == "#") { # Check Left
      moves = append(moves, "L")
    } else if (mat[new_R[2], new_R[1]] == "#") { # Check Left
      moves = append(moves, "R")
      dir = -dir
    } else {
      break
    }
    
    j = 0
    repeat {
      new = pos + dir
      if (new[2] > nrow(mat) || new[2] < 1 || new[1] > ncol(mat) || new[1] < 1)
        break
      if (mat[new[2], new[1]] == ".") 
        break
      
      j = j + 1
      pos = new
    }
    moves = append(moves, j)
  }
  
  moves
}



find_rep_seq = function(moves, val) {
  seq = val
  i = 1
  repeat {
    x = lag(moves,i)[moves == val] %>% unique()
    if (length(x) != 1)
      break
    seq = c(x, seq)
    i = i + 1
  }
  
  i = 1
  repeat {
    x = lead(moves,i)[moves == val] %>% unique()
    if (length(x) != 1)
      break
    seq = c(seq, x)
    i = i + 1
  }
  
  seq
}

moves = find_moves(mat)

moves2 = paste(
  moves[seq(1,length(moves),2)],
  moves[seq(2,length(moves),2)],
  sep=","
)

moves2

A = find_rep_seq(moves2, "R,10")
A = A[-length(A)]
A = A[-1]
A = paste(A, collapse=",")

B = "L,4,R,8,L,6,L,10"
C = "L,4,L,4,L,10"

main = paste(moves2, collapse=",") %>%
  str_replace_all(A, "A") %>%
  str_replace_all(B, "B") %>%
  str_replace_all(C, "C")

main


str_to_int = function(c) {
  str_split(c,"")[[1]] %>%
    append("\n") %>%
    map_int(utf8ToInt)
}

cmds = c(
  str_to_int(main),
  str_to_int(A),
  str_to_int(B),
  str_to_int(C),
  str_to_int("n")
)

part2_input = input
part2_input[1] = 2

ascii_explore = intcode$new(part2_input, cmds)$run()
tail(ascii_explore$output,1)
