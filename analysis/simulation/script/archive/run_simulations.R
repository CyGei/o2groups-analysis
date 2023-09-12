library(dplyr)
library(furrr)
library(o2groups)

# Description ---------------------------------------------------------------------------------
# Reads in the scenarios and run the simulations based on scenarios

# Run simulations -----------------------------------------------------------------------------
run_simulations <- function(scenarios) {
  simulations <- lapply(scenarios, function(scenario) {
    simulation <- o2groups::simulate_groups_furrr(
      n_simulations = 100,
      duration = 100,
      n_groups = scenario$n_groups,
      size = scenario$size,
      name = scenario$name,
      delta = scenario$delta,
      intro_n = scenario$intro_n,
      r0 = scenario$r0,
      generation_time = scenario$generation_time$d(0:50),
      incubation_period = scenario$incubation_period$r(500)
    )$data
    simulation$scenario <- scenario$scenario
    return(simulation)
  })
  return(simulations)
}


# Run simulations for db -----------------------------------------------------------------------------
run_simulations_db <- function(scenario) {
  simulation <- o2groups::simulate_groups_furrr(
    n_simulations = 100,
    duration = 100,
    n_groups = scenario$n_groups,
    size = scenario$size,
    name = scenario$name,
    delta = scenario$delta,
    intro_n = scenario$intro_n,
    r0 = scenario$r0,
    generation_time = scenario$generation_time$d(0:50),
    incubation_period = scenario$incubation_period$r(500)
  )$data
  simulation$scenario <- scenario$scenario
  return(simulation)
}
