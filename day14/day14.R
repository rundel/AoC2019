library(tidyverse)

amt_to_df = function(amt) {
  str_split(amt, " ", simplify = TRUE) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("value", "name")) %>%
    mutate(value = as.numeric(value)) %>%
    select(name, value)
}


rxn_to_list = function(rxn) {
  "(?:(\\d+) ([A-Z]+),?)+ => (\\d+) ([A-Z]+)"
  
  
  m = str_split(rxn, " => ", simplify = TRUE)
  input  = m[,1] %>% str_split(", ")
  output = m[,2]
  
  m = str_split(output, " ", simplify = TRUE)
  output_val  = m[,1]
  output_name = m[,2]
  
  map2(
    output_val, input,
    function(val, input) {
        list(
          value  = as.numeric(val),
          input = amt_to_df(input)
        )
    }
  ) %>% 
    setNames(output_name)
}

get_needed_input = function(name, value, tbl) {
  
  #cat("get:",name, value, "\n")
  
  if (name == "ORE") {
    tribble(
      ~name, ~value,
      name, value
    )
  } else {
    mult = value / tbl[[name]]$value
      
    tbl[[name]]$input %>%
      mutate(value = value * ceiling(mult))
  }
}

calc_ore = function(rxn, init_fuel = 1) {
  table = rxn_to_list(rxn)
  
  needed = tribble(
    ~name, ~value,
    "FUEL", init_fuel
  )
  changed = TRUE
  
  repeat {
    prev = needed
    
    possible_inputs = map(table, c("input","name")) %>% unlist() %>% unique()
    
    sub = which(!needed$name %in% possible_inputs)[1]
    
    if (length(sub) == 0)
      break
    
    needed = bind_rows(
      needed[-sub,],
      get_needed_input(needed$name[sub], needed$value[sub], table)
    ) %>%
      group_by(name) %>%
      summarize(value = sum(value))
    
    table[[ prev$name[sub] ]] = NULL
    
    changed = !identical(needed, prev)
    
    if (nrow(needed) == 1 & needed$name[1] == "ORE")
      break
  }
  
  needed
}

ex1 = c(
  "10 ORE => 10 A",
  "1 ORE => 1 B",
  "7 A, 1 B => 1 C",
  "7 A, 1 C => 1 D",
  "7 A, 1 D => 1 E",
  "7 A, 1 E => 1 FUEL"
)
calc_ore(ex1) # 31


ex2 = c(
  "9 ORE => 2 A",
  "8 ORE => 3 B",
  "7 ORE => 5 C",
  "3 A, 4 B => 1 AB",
  "5 B, 7 C => 1 BC",
  "4 C, 1 A => 1 CA",
  "2 AB, 3 BC, 4 CA => 1 FUEL"
)
calc_ore(ex2) # 165


# 13312 ORE for 1 FUEL:
ex3 = c(
  "157 ORE => 5 NZVS",
  "165 ORE => 6 DCFZ",
  "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL",
  "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ",
  "179 ORE => 7 PSHF",
  "177 ORE => 5 HKGWZ",
  "7 DCFZ, 7 PSHF => 2 XJWVT",
  "165 ORE => 2 GPVTF",
  "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
)
calc_ore(ex3)

#180697 ORE for 1 FUEL:
ex4 = c(
  "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG",
  "17 NVRVD, 3 JNWZP => 8 VPVL",
  "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL",
  "22 VJHF, 37 MNCFX => 5 FWMGM",
  "139 ORE => 4 NVRVD",
  "144 ORE => 7 JNWZP",
  "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC",
  "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV",
  "145 ORE => 6 MNCFX",
  "1 NVRVD => 8 CXFTF",
  "1 VJHF, 6 MNCFX => 4 RFSQX",
  "176 ORE => 6 VJHF"
)
calc_ore(ex4)

# 2210736 ORE for 1 FUEL:
ex5 = c(
  "171 ORE => 8 CNZTR",
  "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL",
  "114 ORE => 4 BHXH",
  "14 VRPVC => 6 BMBT",
  "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL",
  "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT",
  "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW",
  "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW",
  "5 BMBT => 4 WPTQ",
  "189 ORE => 9 KTJDG",
  "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP",
  "12 VRPVC, 27 CNZTR => 2 XDBXC",
  "15 KTJDG, 12 BHXH => 5 XCVML",
  "3 BHXH, 2 VRPVC => 7 MZWV",
  "121 ORE => 7 VRPVC",
  "7 XCVML => 6 RJRHP",
  "5 BHXH, 4 VRPVC => 5 LTCX"
)
calc_ore(ex5)

input = readLines(here::here("day14/input.txt"))
calc_ore(input)


find_fuel = function(rxn, lb = 1e5, ub = 1e8, ore_budget = 1000000000000) {
  repeat {
    guess = mean(c(lb,ub)) %>% floor()
    ore = calc_ore(rxn, guess)$value
    if (ore < ore_budget) {
      cat("Guess: ", guess, "too small\n")
      lb = guess
    } else {
      cat("Guess: ", guess, "too big\n")
      ub = guess
    }
    
    if (abs(lb-ub) == 1)
      break
  }
}

find_fuel(ex3)
