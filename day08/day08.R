library(tidyverse)

ex1 = "123456789012"

to_image = function(data, rows, cols) {
  vals = str_split(data, "")[[1]]
  slices = length(vals) / rows / cols
  array(vals, dim = c(cols, rows, slices))
}

find_layer = function(img) {
  zeros = apply(img, 3, function(x) sum(x == 0))
  img[,,which.min(zeros)]
}

calc = function(layer) {
  sum(layer == 1) * sum(layer == 2)
}

ex1 %>% to_image(2,3) %>% find_layer() %>% calc()

input = readLines(here::here("day08/input.txt"))

input %>% to_image(6,25) %>% find_layer() %>% calc()

get_color = function(x) {
  for(i in x) {
    if (i %in% 0:1)
      return(as.integer(i))
  }
  
  return(NA)
}


input %>% to_image(6,25) %>% apply(1:2, get_color) %>% image()
