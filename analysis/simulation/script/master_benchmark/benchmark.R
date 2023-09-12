source("analysis/simulation/script/generate_scenarios.R")

n_scenarios = 10
n_simulations = 100
scenarios <- generate_scenarios(n_scenarios)
peak_coeffs <- seq(0.7, 1, 0.1)
n_workers <- future::availableCores() - 1
plan(multisession, workers = n_workers)


source(here("analysis", "simulation", "script", "master_test", "furrr_inner.R"))
source(here("analysis", "simulation", "script", "master_test", "furrr_outer.R"))
source(here("analysis", "simulation", "script", "master_test", "foreach.R"))



library(microbenchmark)
bench <- microbenchmark(
  "furrr_inner" = run_furrr_inner(),
  "furrr_outer" = run_furrr_outer(),
  "foreach" = run_foreach(),
  times = 10
)

bench
