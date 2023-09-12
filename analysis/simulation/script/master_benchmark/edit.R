library(o2groups)
library(dplyr)
library(furrr)
library(purrr)
library(here)

plan(multisession, workers = future::availableCores()[[1]] - 1)
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

    # Delta Estimates
    results <- purrr::map(simulations, ~ process_simulation(.x, peak_coeffs, scenario))

    # Write RDS files to respective folders
    saveRDS(simulations, file.path(scenario_folder, "simulations", paste0(scenario$scenario, ".rds")))
    saveRDS(results, file.path(scenario_folder, "results", paste0(scenario$scenario, ".rds")))
}, .options = furrr_options(seed = NULL), .env_globals = list(scenarios = scenarios, n_simulations = n_simulations, peak_coeffs = peak_coeffs, process_simulation = process_simulation))


process_simulation <- function(sim, peak_coeffs, scenario) {
    peaks <- o2groups:::get_peak(sim)

    # iterate over each peak coefficient
    simulation_results <- purrr::map(peak_coeffs, function(peak_coeff) {
        # iterate over each group
        group_result <- purrr::map(names(peaks), function(group) {
            d_group <- o2groups::early_delta(
                data = sim,
                min_t = 0,
                max_t = peak_coeff * peaks[[group]],
                size = scenario$size,
                name = scenario$name
            )

            if (group %in% rownames(d_group)) {
                d_group <- d_group[group, ]
                d_group <- data.frame(
                    group = group,
                    peak_coeff = peak_coeff,
                    t(d_group)
                )
                return(d_group)
            }
            return(NULL)
        }) %>% purrr::list_rbind()
    }) %>% purrr::list_rbind()
    # create a scenario column if the dataframe isn't empty
    if (nrow(simulation_results) > 0){
        simulation_results$scenario <- scenario$scenario
    }
    return(simulation_results)
}
