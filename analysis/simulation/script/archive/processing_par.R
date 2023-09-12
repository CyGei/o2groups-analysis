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



process_simulations <-
  function(scenarios,
           simulations,
           peak_coeffs,
           batch_size = 10) {

    remove_obj <- function(scenario) {
      scenario$generation_time <- NULL
      scenario$incubation_period <- NULL
      return(scenario)
    }

    num_scenarios <- length(scenarios)
    num_workers <- future::availableCores()[[1]] - 2
    num_batches <- ceiling(num_scenarios / batch_size)

    # Split simulations into subsets for workers
    simulations_subsets <-
      split(simulations,
            rep(
              1:num_batches,
              each = batch_size,
              length.out = length(simulations)
            ))

    future::plan(multisession, workers = num_workers)
    options(future.globals.maxSize = 5500 * 1024 ^ 2)

    # Function to process a batch of scenarios
    process_batch <- function(batch_id) {
      start_index <- (batch_id - 1) * batch_size + 1
      end_index <- min(start_index + batch_size - 1, num_scenarios)
      batch_results <- list()

      for (scenario_id in start_index:end_index) {
        scenario <- scenarios[[scenario_id]]
        size <- scenario$size
        name <- scenario$name
        peak_coeff <- peak_coeffs

        sim <- simulations_subsets[[batch_id]][[scenario_id - start_index + 1]]

        peak_results <- lapply(peak_coeff, function(peak) {
          out <-
            o2groups::process_simulation(sim, peak_coeff, size = size, name = name) %>%
            mutate(scenario = scenario_id, peak_coeff = peak)
          return(out)
        }) %>% bind_rows()

        batch_results[[scenario_id]] <- peak_results
      }

      return(batch_results)
    }


    # Process scenarios in batches
    all_results <- furrr::future_map(
      seq(1, num_scenarios, by = batch_size), #batch_id
      .f = process_batch,
      .options = furrr::furrr_options(seed = NULL)
    )

    # Combine results
    results <- unlist(all_results, recursive = FALSE)
    return(results)
  }







# foreach -----------------------------------------------------------------
library(foreach)

process_simulations <- function(scenarios, simulations, peak_coeffs) {
  remove_obj <- function(scenario) {
    scenario$generation_time <- NULL
    scenario$incubation_period <- NULL
    return(scenario)
  }

  num_scenarios <- length(scenarios)
  num_workers <- future::availableCores()[[1]] - 2
  batch_size <- ceiling(num_scenarios / num_workers)
  num_batches <- ceiling(num_scenarios / batch_size)

  # Split scenarios into batches
  scenario_batches <-
    split(1:num_scenarios,
          rep(1:num_batches, each = batch_size, length.out = num_scenarios))

  # Prepare simulations subsets for each batch
  simulations_subsets <- lapply(scenario_batches, function(batch_indices) {
    scenario_subset <- scenarios[batch_indices]
    sim_subset <- simulations[batch_indices]
    return(list(scenario_subset = scenario_subset, sim_subset = sim_subset))
  })

  batch_results <- list()

  for (batch_id in 1:num_batches[1:3]) {
    print(paste0("batch_id: ", batch_id, ": ", Sys.time()))
    Sys.sleep(3)
    batch_indices <- scenario_batches[[batch_id]]
    batch_data <- simulations_subsets[[batch_id]]

    batch_results[[batch_id]] <- foreach(i = batch_indices,
                                         .combine = c,
                                         .inorder=FALSE) %dopar% {

      scenario <- batch_data$scenario_subset[[i]]
      sim <- batch_data$sim_subset[[i]]

      size <- scenario$size
      name <- scenario$name
      peak_coeff <- peak_coeffs

      scenario_results <- lapply(peak_coeff, function(peak) {
        out <- o2groups::process_simulation(sim, peak, size = size, name = name) %>%
          mutate(scenario = i, peak_coeff = peak)
        return(out)
      })

      return(scenario_results)
    }
  }

  # Combine batch results
  results <- unlist(batch_results, recursive = FALSE)
  return(results)
}
