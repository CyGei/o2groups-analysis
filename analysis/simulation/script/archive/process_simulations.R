library(foreach)
library(doFuture)
library(o2groups)
library(dplyr)

process_simulations <-
  function(scenarios, simulations, peak_coeffs) {
    remove_obj <- function(scenario) {
      scenario$generation_time <- NULL
      scenario$incubation_period <- NULL
      return(scenario)
    }
    scenarios <- lapply(scenarios, remove_obj)

    num_scenarios <- length(scenarios)
    num_workers <- future::availableCores()[[1]] - 1
    num_batches <- ceiling(num_scenarios / num_workers)
    future::plan(multisession, workers = num_workers)

    # Simulations are split into batches
    sim_batches <- split(simulations,
                         rep(1:num_batches,
                             each = num_workers,
                             length.out = num_scenarios))

    scenario_batches <- split(scenarios,
                              rep(1:num_batches,
                                  each = num_workers,
                                  length.out = num_scenarios))

    batch_results <- list()
    for (batch in 1:num_batches) {
      sim_batch <- sim_batches[[batch]]
      scenario_batch <- scenario_batches[[batch]]

      # Each worker processes one scenario-simulation pair in parallel
      batch_results[[batch]] <-
        foreach(i = 1:length(scenario_batch),
                .combine = rbind) %dofuture% {
                  scenario <- scenario_batch[[i]]
                  scenario_id <- scenario$scenario
                  sim <- sim_batch[[i]]
                  size <- scenario$size
                  name <- scenario$name
                  peak_results <-
                    lapply(peak_coeffs, function(peak_coeff) {
                      peak_est <-
                        o2groups::process_simulation(sim, peak_coeff, size, name) %>%
                        mutate(scenario = scenario_id, peak_coeff = peak_coeff)
                      return(peak_est)
                    })
                  return(peak_results)
                }

    }

    return(bind_rows(batch_results))
  }
