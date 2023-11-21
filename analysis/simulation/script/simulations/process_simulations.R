process_simulations <-
  function(simulations, peak_coeffs, scenario) {
    n_rows <- n_simulations * scenario$n_groups * length(peak_coeffs)
    simulation_results <- data.frame(
      name = character(n_rows),
      peak_coeff = numeric(n_rows),
      peak_date = numeric(n_rows),
      est = numeric(n_rows),
      lower_ci = numeric(n_rows),
      upper_ci = numeric(n_rows),
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
      size_freq <- prop.table(scenario$size)
      names(size_freq) <- scenario$name

      for (peak_coeff in peak_coeffs) {
        for (group in scenario$name) {
          # cat("Simulation",
          #     simulation_idx,
          #     "Peak coeff",
          #     peak_coeff,
          #     "Group",
          #     group,
          #     "\n")

          peak_date <- as.numeric(peaks[[group]])

          # No peak date (epidemic never takes off)
          if (length(peak_date) == 0) {
            simulation_results[idx,] <- data.frame(
              name = group,
              peak_coeff = as.numeric(peak_coeff),
              peak_date = NA_real_,
              est = NA_real_,
              lower_ci = NA_real_,
              upper_ci = NA_real_,
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
            # Only look at cases before the cutoff_date
            sim_cutoff  <-
              sim[sim$date_onset  <= cutoff_date ,]
            n_cases <-
              nrow(sim_cutoff[sim_cutoff$group == group, ])
            total_cases <-
              nrow(sim_cutoff)

            beta <- o2groups::binom_mix(data = sim_cutoff,
                                        min_t = 0,
                                        max_t = cutoff_date)


            # if we have beta values for this group
            if (group %in% rownames(beta)) {
              beta <- beta[group,]
              gamma <-
                o2groups::get_gamma(beta = beta[1:3], f = size_freq[[group]])

              simulation_results[idx,] <- data.frame(
                name = group,
                peak_coeff = as.numeric(peak_coeff),
                peak_date = as.numeric(peak_date),
                est = as.numeric(gamma[[1]]),
                lower_ci = as.numeric(gamma[[2]]),
                upper_ci = as.numeric(gamma[[3]]),
                successes = as.integer(beta["successes"]),
                trials = as.integer(beta["trials"]),
                n_cases = as.integer(n_cases),
                total_cases = as.integer(total_cases),
                scenario = scenario$scenario,
                simulation = as.integer(simulation_idx)
              )

            } else {
              simulation_results[idx,] <- data.frame(
                name = group,
                peak_coeff = as.numeric(peak_coeff),
                peak_date = as.numeric(peak_date),
                est = NA_real_,
                lower_ci = NA_real_,
                upper_ci = NA_real_,
                successes = 0L,
                trials = 0L,
                n_cases = as.integer(n_cases),
                total_cases = as.integer(total_cases),
                scenario = scenario$scenario,
                simulation = as.integer(simulation_idx)
              )
            }
          }

          idx <- idx + 1
        }
      }
    }

    return(simulation_results)
  }
