library(o2groups)
library(dplyr)
library(furrr)
library(purrr)
library(here)
# Function to process a single simulation
process_scenario <- function(sim, peak_coeffs, scenario) {
  peaks <- o2groups:::get_peak(sim)

  result_list <- list()

  for (peak_coeff in peak_coeffs) {
    processed_data <- map(names(peaks), function(group) {
      d_group <- o2groups::early_delta(
        data = sim,
        min_t = 0,
        max_t = peak_coeff * peaks[[group]],
        size = scenario$size,
        name = scenario$name
      )

      if (group %in% rownames(d_group)) {
        d_group <- d_group[group, ]
        d_group <- as.data.frame(as.list(d_group))
        d_group$group <- group
        d_group$peak_coeff <- peak_coeff
        d_group$scenario <- scenario$scenario
        return(d_group)
      }
      return(NULL)
    })

    result_list <- append(result_list, processed_data)
  }

  result_df <- bind_rows(result_list)
  return(result_df)
}

run_furrr_outer <- function(){
  # Process each scenario
  scenarios_results <- future_map(scenarios, function(scenario) {
    scenario_folder <- here("analysis/simulation/data")

    # Simulations
    simulations <- map(1:n_simulations, function(j) {
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

    # Results
    results <- map(simulations, ~process_scenario(sim = .x, peak_coeffs, scenario))

    # Write RDS files to respective folders
    saveRDS(simulations, file.path(scenario_folder, "simulations", paste0(scenario$scenario, ".rds")))
    saveRDS(results, file.path(scenario_folder, "results", paste0(scenario$scenario, ".rds")))

    #return(list(simulations = simulations, results = results))
  }, .options = furrr_options(seed = NULL))

  # If needed, you can access the simulations and results for each scenario like this:
  # scenario_1_simulations <- scenarios_results[[1]]$simulations
  # scenario_1_results <- scenarios_results[[1]]$results
}

