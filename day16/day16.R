library(tidyverse)

place_pattern = function(p, n_digits) {
  base_pattern = c(0, 1, 0, -1)
  pattern = rep(base_pattern, rep(p, 4))
  
  pattern = rep(pattern, ceiling(n_digits / (4*p - 1)))
  pattern[2:(n_digits+1)]
}

split_num = function(i) {
  i %>%
    as.character() %>%
    str_split(pattern = "") %>%
    .[[1]] %>%
    as.integer()
}

place_pattern(1, 8)
place_pattern(2, 8)

calc_pats = function(n) {
  map(1:n, place_pattern, n_digits = n)
}

mem_calc_pats = memoise::memoise(calc_pats)



fft = function(input) {
  if (length(input) == 1)
    input = split_num(input)
  
  n = length(input)
  
  pats = mem_calc_pats(n)
  map_dbl(
    pats,
    function(pat, input) {
      sum(pat * input) %>%
        abs() %>%
        {. %% 10}
    },
    input = input
  )
}

fft_rep = function(input, n=1) {
  for(i in seq_len(n)) {
    input = fft(input)
    #print(input)
  }
  input
}

fft_rep(12345678, 4)

fft_rep("80871224585914546619083218645595", 100) # 24176176.
fft_rep("19617804207202209144916044189917", 100) # 73745418.
fft_rep("69317163492948606335995924319873", 100) # 52432133.

input = readLines(here::here("day16/input.txt"))
res = fft_rep(input, 100)
res[1:8] %>% paste(collapse = "")
