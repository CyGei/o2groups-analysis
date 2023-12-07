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


binom_conf <- function(x, n, alpha) {
  na_rows <- which(is.na(n))

  if (length(na_rows) > 0) {
    out <- data.frame(
      beta_est = rep(NA, length(x)),
      beta_lower_ci = rep(NA, length(x)),
      beta_upper_ci = rep(NA, length(x)),
      conf_level = 1 - alpha
    )

    non_na_rows <- setdiff(seq_along(x), na_rows)

    if (length(non_na_rows) > 0) {
      non_na_out <- Hmisc::binconf(
        x = x[non_na_rows],
        n = n[non_na_rows],
        alpha = alpha,
        method = "exact",
        return.df = TRUE
      )

      conf_level <- 1 - alpha
      non_na_out$conf_level <- conf_level
      rownames(non_na_out) <- NULL
      names(non_na_out) <- c("beta_est", "beta_lower_ci", "beta_upper_ci", "conf_level")

      out[non_na_rows, ] <- non_na_out
    }

    return(out)
  }

  out <- Hmisc::binconf(
    x = x,
    n = n,
    alpha = alpha,
    method = "exact",
    return.df = TRUE
  )

  conf_level <- 1 - alpha
  out$conf_level <- conf_level
  rownames(out) <- NULL
  names(out) <- c("beta_est", "beta_lower_ci", "beta_upper_ci", "conf_level")
  return(out)
}

library(tidyverse)
library(here)
# READ FILES --------------------------------------------------------------

results_df <- readRDS(here("analysis/simulation/data/model", "results_df.rds")) %>%
  select(-c("est", "lower_ci", "upper_ci"))
scenarios_df <- readRDS(here("analysis/simulation/data/model", "scenarios_df.rds"))

b1_time <- system.time({
  b1 <- b_est(x = test$successes,
              n = test$trials,
              alpha = 0.05)
})
b2time <- system.time({
  b2 <- binom_conf(x = test$successes,
                   n = test$trials,
                   alpha = 0.05)
})



b1_df <-  bind_cols(results_df, as.data.frame(b1)) %>%
  mutate(across(.cols = c("est", "lower_ci", "upper_ci"),
                ~ o2groups::get_gamma(beta = .x, f = size_freq)
                  ))

b3time <- system.time({

})

alpha <- 1 - c(0.50, 0.75, 0.90, 0.95)
model_df <-
  purrr::map_dfr(.x = alpha,
                 ~ bind_cols(
                   results_df,
                   binom_conf(
                     x = results_df$successes,
                     n = results_df$trials,
                     alpha = .x
                   )
                 ))
