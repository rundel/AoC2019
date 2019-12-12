library(tidyverse)

ex1 = 122345
ex2 = 111123
ex3 = 111111
ex4 = 223450
ex5 = 123789

check_password = function(v) {
  v = v %>% as.character() %>% strsplit("") %>% pluck(1) %>% as.integer()
  n = length(v)
  
  up = v[-1]
  low = v[-n]
  
  all(up >= low) & any(up == low)
}

check_password(ex1)
check_password(ex2)
check_password(ex3)
check_password(ex4)
check_password(ex5)

checks = map_lgl(134564:585159, check_password)


check_password2 = function(v) {
  v = v %>% as.character() %>% strsplit("") %>% pluck(1) %>% as.integer()
  n = length(v)
  
  up = v[-1]
  low = v[-n]

  pairs = paste(low[low == up], up[low == up])
  all(up >= low) & any(table(pairs) == 1)
}

check_password2("123456")
check_password2("112233")
check_password2("123444")
check_password2("111122")

checks2 = map_lgl((134564:585159)[checks], check_password2)


