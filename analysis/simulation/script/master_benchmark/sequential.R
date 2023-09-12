for (i in seq_along(scenarios)) {
    scenario <- scenarios[[i]]

    # Simulations
    simulations <- for( j in seq_len(n_simulations)) {
      sim <- o2groups::simulate_groups(
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
    }

    # Results
    results <-
      for( k in seq_along(simulations)) {
        sim <- simulations[[k]]
        peaks <- o2groups:::get_peak(sim)

        result <- data.frame(
          scenario = character(),
          simulation = integer(),
          peak_coeff = double(),
          group = character(),
          est = numeric(),
          lower_ci = numeric(),
          upper_ci = numeric(),
          successes = numeric(),
          trials = numeric(),
          stringsAsFactors = FALSE
        )

        for (peak_coeff in peak_coeffs) {
          for (group in sort(names(peaks))) {
            d_group <- o2groups::early_delta(
              data = sim,
              min_t = 0,
              max_t = peak_coeff * peaks[[group]],
              size = scenario$size,
              name = scenario$name
            )

            if (!group %in% rownames(d_group)) {
              next
            }

            d_group <- d_group[group, ]
            d_group <- as.data.frame(as.list(d_group))
            d_group$group <- group
            d_group$peak_coeff <- peak_coeff
            d_group$simulation <- k
            d_group$scenario <- scenario$scenario
            result <- rbind(result, d_group)
          }
        }
        return(result)
      }

    # Write RDS files to respective folders
    saveRDS(simulations, file = paste0(here("analysis/simulation/data", "simulations"), "/", scenario$scenario, ".rds"))
    saveRDS(results, file = paste0(here("analysis/simulation/data", "results"), "/", scenario$scenario, ".rds"))
  }