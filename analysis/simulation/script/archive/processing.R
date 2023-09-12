library(dplyr)
library(furrr)

# Description ---------------------------------------------------------------------------------
# Reads in the scenarios
# Run the simulations based on scenarios
# Process the output of simulations for each peak_coeff

# Read scenarios -----------------------------------------------------------
# scenarios <- readRDS("analysis/simulation/data/scenarios.rds")


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

# Process simulations for each peak_coeff --------------------------------------------------------------------

process_simulations <- function(scenarios,simulations, peak_coeffs) {

  remove_obj <- function(scenario) {
  scenario$generation_time <- NULL
  scenario$incubation_period <- NULL
  return(scenario)
}

  scenarios_grid <- lapply(scenarios, remove_obj) %>%
    tibble(scenarios = .) %>%
    tidyr::unnest_wider(col = scenarios) %>%
    select(scenario, size, name) %>%
    tidyr::expand_grid(., peak_coeff = peak_coeffs)

  future::plan(multisession, workers = future::availableCores()[[1]] - 2)

  results <- furrr::future_map(seq_len(nrow(scenarios_grid)), function(i) {
    scenario <- scenarios_grid$scenario[[i]]
    size <- scenarios_grid$size[[i]]
    name <- scenarios_grid$name[[i]]
    peak_coeff <- scenarios_grid$peak_coeff[i]
    sim <- simulations[[scenario]]
    peak_results <-
      o2groups::process_simulation(sim, peak_coeff, size = size, name = name) %>%
      mutate(scenario = scenario, peak_coeff = peak_coeff)
    return(peak_results)
  }, .options = furrr::furrr_options(seed = NULL))

  return(results)
}


# Process simulations old ---------------------------------------------------------
# simulations <- readRDS("analysis/simulation/data/simulations.rds")
# system.time({
#   library(furrr)
#   plan(multisession, workers = availableCores() - 2)
#   peak_coeffs <- c(0.7, 0.8, 0.9, 1)
#   results <-
#     purrr::map2_df(simulations, scenarios, function(simulation, scenario) {
#       furrr::future_map_dfr(peak_coeffs, function(peak_coeff) {
#         o2groups::process_simulation(simulation,
#                                      peak_coeff,
#                                      size = scenario$size,
#                                      name = scenario$name) %>%
#           dplyr::mutate(scenario = scenario$scenario, peak_coeff = peak_coeff)
#       }, .options = furrr::furrr_options(seed = TRUE))
#     })
#
#   saveRDS(results, file = "analysis/simulation/data/results.rds")
# })
