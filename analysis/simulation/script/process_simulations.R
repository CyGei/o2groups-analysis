

process_simulations <-
  function(simulations, peak_coeffs, scenario) {
    n_rows <- n_simulations * scenario$n_groups * length(peak_coeffs)
    simulation_results <- data.frame(
      name = character(n_rows),
      peak_coeff = numeric(n_rows),
      est = numeric(n_rows),
      lower_ci = numeric(n_rows),
      upper_ci = numeric(n_rows),
      successes = integer(n_rows),
      trials = integer(n_rows),
      scenario = character(n_rows),
      simulation = integer(n_rows)
    )

    idx <- 1

    for (simulation_idx in 1:n_simulations) {
      sim <- simulations[[simulation_idx]]

      peaks <- o2groups:::get_peak(sim)

      for (peak_coeff in peak_coeffs) {
        for (group in scenario$name) {
          d_group <- o2groups::early_delta(
            data = sim,
            min_t = 0,
            max_t = peak_coeff * peaks[[group]],
            size = scenario$size,
            name = scenario$name
          )

          if (group %in% rownames(d_group)) {
            simulation_results[idx,] <- data.frame(
              name = group,
              peak_coeff = as.numeric(peak_coeff),
              est = as.numeric(d_group[group, "est"]),
              lower_ci = as.numeric(d_group[group, "lower_ci"]),
              upper_ci = as.numeric(d_group[group, "upper_ci"]),
              successes = as.integer(d_group[group, "successes"]),
              trials = as.integer(d_group[group, "trials"]),
              scenario = scenario$scenario,
              simulation = as.integer(simulation_idx)
            )
          } else {
            simulation_results[idx,] <- data.frame(
              name = group,
              peak_coeff = as.numeric(peak_coeff),
              est = NA_real_,
              lower_ci = NA_real_,
              upper_ci = NA_real_,
              successes = NA_integer_,
              trials = NA_integer_,
              scenario = scenario$scenario,
              simulation = as.integer(simulation_idx)
            )
          }


          idx <- idx + 1
        }
      }
    }

    return(simulation_results)
  }





# OLD ---------------------------------------------------------------------
#
# process_simulation <- function(sim, peak_coeffs, scenario) {
#   peaks <- o2groups:::get_peak(sim)
#
#   # iterate over each peak coefficient
#   simulation_results <-
#     purrr::map(peak_coeffs, function(peak_coeff) {
#       # iterate over each group
#       group_result <- purrr::map(names(peaks), function(group) {
#         d_group <- o2groups::early_delta(
#           data = sim,
#           min_t = 0,
#           max_t = peak_coeff * peaks[[group]],
#           size = scenario$size,
#           name = scenario$name
#         )
#
#         if (group %in% rownames(d_group)) {
#           d_group <- d_group[group,]
#           d_group <- data.frame(group = group,
#                                 peak_coeff = peak_coeff,
#                                 t(d_group))
#           return(d_group)
#         }
#         d_group <- data.frame(
#           group = group,
#           peak_coeff = peak_coeff,
#           est = NA,
#           lower_ci = NA,
#           upper_ci = NA,
#           successes = NA,
#           trials = NA
#         )
#       }) %>% purrr::list_rbind()
#     }) %>% purrr::list_rbind()
#   if (nrow(simulation_results) > 0) {
#     simulation_results$scenario <- scenario$scenario
#     simulation_results$simulation <- unique(sim$simulation)
#   }
#   return(simulation_results)
# }
#


# benchmark_results <- microbenchmark::microbenchmark(
#   map =   purrr::map(simulations,
#                            ~ process_simulation(.x, peak_coeffs, scenario)),
#   no_map = process_simulation(simulations, peak_coeffs, scenario),
#   times = 5
# )
# benchmark_results
