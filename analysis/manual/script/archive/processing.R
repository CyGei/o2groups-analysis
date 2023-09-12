scenarios <- readRDS("analysis/data/scenarios.rds")

# Process scenarios -----------------------------------------------------------
simulations <- lapply(scenarios, function(scenario) {
  simulation <- o2groups::simulate_groups_furrr(
    n_simulations = 100,
    duration = 100,
    n_groups = scenario$n_groups,
    size = scenario$size,
    name = scenario$name,
    delta = scenario$delta,
    intro_group = scenario$intro_group,
    intro_n = scenario$intro_n,
    r0 = scenario$r0,
    generation_time = scenario$generation_time$d(0:50),
    incubation_period = scenario$incubation_period$r(500)
  )$data
  simulation$scenario <- scenario$scenario
  return(simulation)
})
saveRDS(simulations, file = "analysis/data/simulations.rds")

# Process simulations ---------------------------------------------------------
simulations <- readRDS("analysis/data/simulations.rds")

library(furrr)
plan(multisession, workers = availableCores() - 2)
peak_coeffs <- c(0.7, 0.8, 0.9, 1)
results <-
  purrr::map2_df(simulations, scenarios, function(simulation, scenario) {
    furrr::future_map_dfr(peak_coeffs, function(peak_coeff) {
      o2groups::process_simulation(simulation,
                                   peak_coeff,
                                   size = scenario$size,
                                   name = scenario$name) %>%
        dplyr::mutate(scenario = scenario$scenario, peak_coeff = peak_coeff)
    }, .options = furrr::furrr_options(seed = TRUE))
  })

saveRDS(results, file = "analysis/data/results.rds")

