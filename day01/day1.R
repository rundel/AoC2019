d = readr::read_csv(here::here("day1/input.txt"), col_names = "mass")

fuel = function(mass) {
  fuel = floor(mass / 3) - 2
  ifelse(fuel <= 0, 0, fuel)
}

sum(fuel(d$mass))



fuel2 = function(mass) {
  cur_fuel = fuel(mass)
  total_fuel = cur_fuel
  
  while(any(cur_fuel > 0)) {
    #print(cur_fuel)
    cur_fuel = fuel(cur_fuel)
    total_fuel = total_fuel + cur_fuel
  }
  
  total_fuel
}

sum(fuel2(d$mass))

