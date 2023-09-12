# master file for MANUAL analysis
library(tidyverse)
library(here)

subdirectories <- c("scenarios", "simulations", "results", "logs")
purrr::walk(subdirectories, ~ dir.create(here::here("analysis/manual/data", .x), recursive = TRUE))

# source scenarios
source(here("analysis/manual/script/scenarios.R"))

# generate table
source(here("analysis/manual/script/scenarios_kable.R"))
kbl
scenarios

# generate simulations
source(here("analysis/simulation/script/process_simulations.R"))
n_workers <- future::availableCores() - 1
plan(multisession, workers = n_workers)
n_simulations <- 100
peak_coeffs <- seq(0.7, 1, 0.1)


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

    # Save simulations
    simulation_file <- paste0(here("analysis/manual/data/simulations"), "/",janitor::make_clean_names(scenario$scenario), ".rds")

    if (file.exists(simulation_file)) {
        stop(paste0("File already exists: ", simulation_file))
    } else {
        saveRDS(simulations, simulation_file)
    }

    # Save simulations
    results_file <- paste0(here("analysis/manual/data/results"), "/",janitor::make_clean_names(scenario$scenario), ".rds")
    if (file.exists(results_file)) {
        stop(paste0("File already exists: ", results_file))
    } else {
        saveRDS(results, results_file)
    }
}, .options = furrr_options(seed = NULL))


