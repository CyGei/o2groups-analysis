library(dplyr)
library(o2groups)

############################################################
# process_simulations: retrieve peak dates & transmissions
############################################################

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


############################################################
# compute_estimates: calculate estimates of beta/delta for different alpha levels
############################################################

compute_estimates <- function(alpha, scenario_df, results) {
  #beta: within group mixing
  beta <- b_est(x = results$successes,
                n = results$trials,
                alpha = alpha)
  beta <- as.data.frame(beta)
  names(beta) <-
    c("beta_est", "beta_lower_ci", "beta_upper_ci")
  beta$alpha <- alpha

  data <-
    left_join(scenario_df, bind_cols(results, beta), by = c("name"))

  est <- mutate(
    .data = data,
    across(
      .cols = starts_with("beta_"),
      .fns = function(x) {
        gamma <- o2groups::get_gamma(beta = x, f = .data[["size_freq"]])
        delta <- o2groups::scale(gamma)
        return(delta)
      },
      .names = "{gsub('beta_', '', .col)}"
    )
  )

  est <- mutate(
    .data = est,
    intro_prop = intro_n / size,
    bias = delta - est,
    is_within_ci = ifelse(delta >= lower_ci &
                            delta <= upper_ci,
                          TRUE,
                          FALSE),
    significant_delta = ifelse(delta != 0,
                               TRUE,
                               FALSE),
    significant_est = ifelse(lower_ci > 0 |
                               upper_ci < 0,
                             TRUE,
                             FALSE),
    true_positive = ifelse(significant_est == TRUE &
                             significant_delta == TRUE,
                           1,
                           0),
    true_negative = ifelse(significant_est == FALSE &
                             significant_delta == FALSE,
                           1,
                           0),
    false_positive = ifelse(significant_est == TRUE &
                              significant_delta == FALSE,
                            1,
                            0),
    false_negative = ifelse(significant_est == FALSE &
                              significant_delta == TRUE,
                            1,
                            0)
  )

  return(est)
}




# other helpers -----------------------------------------------------------

# beta estimates
b_est <- function(x, n, alpha = 0.05) {
  est <- x/n
  lower_ci <- qbeta(alpha/2, x, n-x+1)
  zvals <- !is.na(x) & x == 0
  nvals <- !is.na(x) & x == n
  lower_ci[zvals] <- 0
  lower_ci[nvals] <- (alpha/2)^(1/n[nvals])
  upper_ci <- qbeta(1-alpha/2, x+1, n-x)
  upper_ci[zvals] <- 1-(alpha/2)^(1/n[zvals])
  upper_ci[nvals] <- 1
  cbind(est, lower_ci, upper_ci)
}

#remove object from list (scenario)
remove_object <- function(list, object_name) {
  list <- list[!names(list) %in% object_name]
  return(list)
}

scenario_to_df <- function(scenario) {
  scenario_df <- remove_object(
    scenario,
    c(
      "scenario",
      "generation_time",
      "incubation_period",
      "duration",
      "delta"
    )
  )
  scenario_df <-
    rename(.data = as.data.frame(scenario_df), delta = scaled_delta)
  scenario_df$size_freq <-
    scenario_df$size / sum(scenario_df$size)
  scenario_df$intro_prop <- scenario_df$intro_n / scenario_df$size
  return(scenario_df)
}

