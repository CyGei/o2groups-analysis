library(here)
library(furrr)
library(purrr)
library(o2groups)
library(dplyr)

subdirectories <- c("scenarios", "simulations", "results", "logs")
purrr::walk(subdirectories, ~ dir.create(here::here("analysis/simulation/data", .x), recursive = TRUE))

# 0. Parameters --------------------------------------------------------------------------------------------
n_scenarios <- 10000
n_simulations <- 100
peak_coeffs <- seq(0.7, 1.6, 0.1)

# Seed for scenarios
log_files <- list.files(here("analysis/simulation/data"), pattern = "log_", recursive = TRUE)
if (length(log_files) > 0) {
  log_files <-
    purrr::map(log_files, ~ readRDS(here("analysis/simulation/data", .x)))
  rseeds <- purrr::map(log_files, ~ .x$rseed)
  rseed <- max(unlist(rseeds)) + 1
  rm(list = c("rseeds", "log_files"))
} else {
  rseed <- 1
}

n_workers <- future::availableCores() - 1
plan(multisession, workers = n_workers)

time_start <- format(Sys.time(), "%Y-%m-%d-%H:%M")


# 1. Generate Scenarios --------------------------------------------------------------------------------------------
source("analysis/simulation/script/generate_scenarios.R")
scenarios <- generate_scenarios(n_scenarios, seed = rseed)
for (i in 1:n_scenarios) {
  saveRDS(scenarios[[i]], file = paste0(
    here("analysis/simulation/data", "scenarios"),
    "/",
    scenarios[[i]]$scenario,
    ".rds"
  ))
}


# 2. Run & Process Simulations --------------------------------------------------------------------------------------------
source(here("analysis/simulation/script/process_simulations.R"))

process_time <- system.time({
  furrr::future_walk(scenarios, function(scenario) {
    # Simulations
    simulations <- purrr::map(1:n_simulations, function(j) {
      sim <- simulate_groups(
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
      sim$scenario <- scenario$scenario
      sim$simulation <- j
      return(sim)
    })

    # Delta Estimates
    results <-
      process_simulations(simulations, peak_coeffs, scenario)

    # Write RDS files to respective folders
    saveRDS(simulations, file.path(
      here("analysis/simulation/data"),
      "simulations",
      paste0(scenario$scenario, ".rds")
    ))
    saveRDS(results, file.path(
      here("analysis/simulation/data"),
      "results",
      paste0(scenario$scenario, ".rds")
    ))

  }, .options = furrr_options(seed = NULL))

})

# 3. Save the run details --------------------------------------------------------------------------------------------
time_end <- format(Sys.time(), "%Y-%m-%d-%H:%M")
time_start <- as.POSIXct(time_start, format = "%Y-%m-%d-%H:%M")
time_end <- as.POSIXct(time_end, format = "%Y-%m-%d-%H:%M")
total_time <- time_end - time_start
scenarios_names <- lapply(scenarios, function(x)
  x$scenario)

log <- list(
  time_start = as.character(time_start),
  time_end = as.character(time_end),
  total_time = as.numeric(total_time),
  process_time = process_time[[3]],
  rseed = rseed,
  workers = n_workers,
  n_simulations = n_simulations,
  n_scenarios = n_scenarios,
  peak_coeffs = paste(peak_coeffs, collapse = ", "),
  scenario_names = paste(scenarios_names, collapse = ", ")
)

time_start <- format(time_start, "%Y-%m-%d-%H_%M")
saveRDS(log, file = paste0(
  here("analysis/simulation/data", "logs"),
  "/log_",
  time_start,
  ".rds"
))

rm(list = ls())
gc()


# Prepare data for modelling ----------------------------------------------
source(here("analysis/simulation/script/model_data.R"))
