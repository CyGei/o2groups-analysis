process_simulations <-
  function(simulations, peak_coeffs, scenario) {
    n_rows <- n_simulations * scenario$n_groups * length(peak_coeffs)
    simulation_results <- data.frame(
      name = character(n_rows),
      peak_coeff = numeric(n_rows),
      peak_date = numeric(n_rows),
      successes = integer(n_rows),
      trials = integer(n_rows),
      n_cases = integer(n_rows),
      total_cases = integer(n_rows),
      scenario = character(n_rows),
      simulation = integer(n_rows)
    )

    idx <- 1

    for (simulation_idx in 1:n_simulations) {
      sim <- simulations[[simulation_idx]]
      peaks <- o2groups:::get_peak(sim)
      for (peak_coeff in peak_coeffs) {
        for (group_i in scenario$name) {
          # cat("Simulation",
          #     simulation_idx,
          #     "Peak coeff",
          #     peak_coeff,
          #     "Group",
          #     group,
          #     "\n")

          peak_date <- as.numeric(peaks[[group_i]])

          # No peak date (epidemic never takes off)
          if (length(peak_date) == 0) {
            simulation_results[idx, ] <- data.frame(
              name = group_i,
              peak_coeff = as.numeric(peak_coeff),
              peak_date = NA_real_,
              successes = NA_real_,
              trials = NA_real_,
              n_cases = NA_real_,
              total_cases = NA_real_,
              scenario = scenario$scenario,
              simulation = as.integer(simulation_idx)
            )
          }
          else {
            cutoff_date <- round(peak_coeff * peak_date)
            # Only analyse chains up to the cutoff_date
            sim_cutoff  <-
              sim[sim$date_onset  <= cutoff_date , ]
            n_cases <-
              nrow(sim_cutoff[sim_cutoff$group == group_i,])
            total_cases <-
              nrow(sim_cutoff)

            group_df <-
              subset(sim_cutoff, source_group == as.character(group_i))

            transmissions <- if (nrow(group_df) > 0) {
              with(group_df,
                   data.frame(
                     successes = sum(group == group_i),
                     trials = length(source_group)
                   ))
            } else {
              data.frame(successes = NA, trials = NA)
            }


            simulation_results[idx,] <- data.frame(
              name = group_i,
              peak_coeff = as.numeric(peak_coeff),
              peak_date = as.numeric(peak_date),
              successes = as.integer(transmissions["successes"]),
              trials = as.integer(transmissions["trials"]),
              n_cases = as.integer(n_cases),
              total_cases = as.integer(total_cases),
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
