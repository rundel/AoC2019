library(tidyverse)

moon = R6::R6Class(
  "moon",
  public = list(
    pos = c(),
    vel = c(),
    
    initialize = function(pos) {
      if (is.numeric(pos) & length(pos) == 3)
        self$pos = pos
      else if (is.character(pos) & length(pos) == 1)
        self$pos = unglue::unglue(pos, "<x={x}, y={y}, z={z}>") %>%
          unlist() %>%
          setNames(NULL) %>%
          as.numeric()
      else
        stop("bad input")
      
      self$vel = c(0,0,0)
    },
    
    print = function() {
      vel = self$vel
      pos = self$pos
      cat(
        glue::glue("pos=< x={pos[1]}, y={pos[2]}, z={pos[3]} >, vel=< x={vel[1]}, y={vel[2]}, z={vel[3]} >"), 
        "\n"
      )
    },
    
    update_pos = function() {
      self$pos = self$pos + self$vel
    },
    
    update_vel = function(delta) {
      self$vel = self$vel + delta
    }
  ),
  active = list(
    potential = function() {
      sum(abs(self$pos))
    },
    kinetic = function() {
      sum(abs(self$vel))
    },
    total = function() {
      self$potential * self$kinetic
    }
  )
)

apply_gravity = function(m1, m2) {
  delta = 2*(m1$pos > m2$pos) + (m1$pos == m2$pos) - 1
  m1$update_vel(-delta)
  m2$update_vel(delta)
}

simulate_orbits = function(start, steps = 1000) {
  n = length(start)
  moons = map(start, moon$new)
  
  for(step in seq_len(steps)) {
    for(i in 1:(n-1)) {
     for(j in i:n) {
       apply_gravity(moons[[i]], moons[[j]])
     }
    }
    
    walk(moons, ~ .$update_pos())
  }
  
  cat("Step: ", step, ", Energy: ", sum(map_dbl(moons, ~.$total)), "\n")
  moons
}


ex1 = c("<x=-1, y=0, z=2>",
        "<x=2, y=-10, z=-7>",
        "<x=4, y=-8, z=8>",
        "<x=3, y=5, z=-1>")

simulate_orbits(ex1, 10)

ex2 = c("<x=-8, y=-10, z=0>",
        "<x=5, y=5, z=10>",
        "<x=2, y=-7, z=3>",
        "<x=9, y=-8, z=-3>")

simulate_orbits(ex2, 100)

input = readLines(here::here("day12/input.txt"))
z = simulate_orbits(input, 1000)



## Part 2

sim_orbit_dim = function(init_pos) {
  n = length(init_pos)
  init_vel = rep(0, n)
  
  pos = init_pos
  vel = init_vel
  step = 0
  
  repeat {
    # Apply gravity
    for(i in seq_len(n)) {
      x = pos[i]
      y = pos[-i]
      
      vel[i] = vel[i] + sum(2*(x < y) + (x == y) - 1)
    }
    
    pos = pos + vel
    step = step + 1
    
    if (all(pos == init_pos) & all(vel == init_vel))
      break
  }
  
  step
}

orbit_repeat = function(start) {
  n = length(start)
  moons = map(start, moon$new)
  
  dims = map(
    1:3,
    function(dim)
      map_dbl(moons, ~ .$pos[dim])
  )
  
  n_steps = map_dbl(
    dims, sim_orbit_dim
  )
  
  list(
    n_steps,
    numbers::mLCM(n_steps)
  )
}
  
  
