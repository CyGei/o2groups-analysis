library(here)
library(furrr)
library(purrr)
library(o2groups)
library(dplyr)

subdirectories <-
  c("scenarios",
    "simulations",
    "results",
    "estimates",
    "summary",
    "logs")
purrr::walk(subdirectories, ~ dir.create(here::here("analysis/simulation/data", .x), recursive = TRUE))

# 0. Parameters --------------------------------------------------------------------------------------------
n_scenarios <- 10000
n_simulations <- 100
peak_coeffs <- sort(c(1, seq(0.5, 1.7, 0.2), 2, 3, 5))
alpha <- 1 - c(0.50, 0.75, 0.90, 0.95)

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
source("analysis/simulation/script/simulations/generate_scenarios.R")
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
source(here("analysis/simulation/script/simulations/process_helpers.R"))


process_time <- system.time({
  furrr::future_walk(scenarios, function(scenario) {
    # Simulations
    simulations <- purrr::map(1:n_simulations, function(j) {
      sim <- simulate_groups(
        duration = scenrio$duration,
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


    # Transmissions by Peak cutoffs
    results <-
      process_simulations(simulations, peak_coeffs, scenario)

    # Scenario Dataframe
    scenario_df <- scenario_to_df(scenario)

    # Beta & Delta Estimates
    estimates <- purrr::map_dfr(.x = alpha,
                                .f = ~compute_estimates(
                                  alpha = .x,
                                  scenario_df = scenario_df,
                                  results = results
                                ))

    # Summarise estimates
    summary <- estimates %>%
      group_by(peak_coeff, name, alpha) %>%
      summarise(
        peak_date = mean(peak_date, na.rm = TRUE),
        coverage = sum(is_within_ci, na.rm = TRUE) / n(),
        bias = mean(bias, na.rm = TRUE),
        sensitivity = sum(true_positive, na.rm = TRUE) / sum(significant_delta, na.rm = FALSE),
        specificity = sum(true_negative, na.rm = TRUE) / sum(significant_delta == FALSE, na.rm = FALSE),
        ppv = sum(true_positive, na.rm = TRUE) / sum(significant_est, na.rm = FALSE),
        npv = sum(true_negative, na.rm = TRUE) / sum(significant_est == FALSE, na.rm = FALSE),
        trials = mean(trials, na.rm = TRUE),
        successes = mean(successes, na.rm = TRUE),
        n_cases = mean(n_cases, na.rm = TRUE),
        total_cases = mean(total_cases, na.rm = TRUE),
        beta_est = mean(beta_est, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      left_join(scenario_df, by = c("name")) %>%
      mutate(group_susceptibles = 1 - (n_cases / size)) %>%
      group_by(peak_coeff, alpha) %>%
      mutate(
        sd_peak_date = sd(peak_date, na.rm = TRUE),
        sd_n_cases = sd(n_cases, na.rm = TRUE),
        sd_size = sd(size, na.rm = TRUE),
        sd_size_freq = sd(size_freq, na.rm = TRUE),
        sd_delta = sd(delta, na.rm = TRUE),
        sd_r0 = sd(r0, na.rm = TRUE),
        sd_intro_n = sd(intro_n, na.rm = TRUE),
        sd_intro_prop = sd(intro_prop, na.rm = TRUE),
        sd_group_susceptibles = sd(group_susceptibles, na.rm = TRUE),
        total_susceptibles = 1 - (sum(n_cases) / sum(size)),
        sd_beta_est = sd(beta_est, na.rm = TRUE)
        ) %>%
      ungroup()


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
    saveRDS(estimates, file.path(
      here("analysis/simulation/data"),
      "estimates",
      paste0(scenario$scenario, ".rds")
    ))
    saveRDS(summary, file.path(
      here("analysis/simulation/data"),
      "summary",
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

#Move to visualisations...
