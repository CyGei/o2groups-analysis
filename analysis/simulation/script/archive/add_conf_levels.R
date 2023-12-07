

# data
df <- tibble(
  successes = c(round(runif(1000, 0, 100)), rep(NA, 10)),
  trials = c(round(runif(1000, 0, 100)) + 100, rep(NA, 10))
)

# VERSION 1 ---------------------------------------------------------------
#function
binom_test <- function(x, n, conf.levels = c(0.50, 0.75, 0.90, 0.95)) {
  if (is.na(n) | n < 1) {
    return(data.frame(
      conf_level = conf.levels,
      est = rep(NA, length(conf.levels)),
      x = rep(NA, length(conf.levels)),
      n = rep(NA, length(conf.levels)),
      lower_ci = rep(NA, length(conf.levels)),
      upper_ci = rep(NA, length(conf.levels))
    ))
  } else {
    results <- lapply(conf.levels, function(conf.level) {
      test <- binom.test(x = x,
                         n = n,
                         conf.level = conf.level)
      c(
        conf_level = conf.level,
        x = x,
        n = n,
        est = test$estimate[[1]],
        lower_ci = test$conf.int[1],
        upper_ci = test$conf.int[2]
      )
    })

    results_df <- as.data.frame(do.call(rbind, results))
    return(results_df)
  }
}

# requires rowwise (slower)
df %>%
  rowwise() %>%
  mutate(beta = list(binom_test(x = successes, n = trials))) %>%
  unnest(beta, names_sep = "_")

# avoids rowwise but still slow
df %>%
  mutate(beta = pmap(list(successes, trials), binom_test)) %>%
  unnest(beta, names_sep = "_")


# VERSION 2 ---------------------------------------------------------------
binom_conf <- function(x, n, alpha) {
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

alpha <- 1 - c(0.50, 0.75, 0.90, 0.95)

out_df <-
  map_dfr(.x = alpha, ~ bind_cols(df, binom_conf(
    x = df$successes,
    n = df$trials,
    alpha = .x
  )))


library(furrr)
plan(multisession, workers = 5)

out_df <- future_map_dfr(.x = alpha,
               ~ bind_cols(df, binom_conf(
                 x = df$successes,
                 n = df$trials,
                 alpha = .x
               ))) %>%
  mutate(across(
    .cols = starts_with("beta_"),
    .fns = function(x) {
      o2groups::get_gamma(beta = x, f = size_freq) %>%
        o2groups::scale(.)
    },
    .names = "{gsub('beta_', '', .col)}"
  ))

# VERSION 3 ---------------------------------------------------------------

b_est <- function(x, n, alpha = 0.05) {
  est <- x/n
  lwr <- qbeta(alpha/2, x, n-x+1)
  zvals <- !is.na(x) & x == 0
  nvals <- !is.na(x) & x == n
  lwr[zvals] <- 0
  lwr[nvals] <- (alpha/2)^(1/n[nvals])
  upr <- qbeta(1-alpha/2, x+1, n-x)
  upr[zvals] <- 1-(alpha/2)^(1/n[zvals])
  upr[nvals] <- 1
  cbind(est, lwr, upr)
}












# Delta estimates & CIs
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
                 )) %>%
  mutate(across(
    .cols = starts_with("beta_"),
    .fns = function(x) {
      o2groups::get_gamma(beta = x, f = size_freq) %>%
        o2groups::scale(.)
    },
    .names = "{gsub('beta_', '', .col)}"
  ))



model_df <-
  model_df %>%
  mutate(
    bias = delta - est,
    is_within_ci = ifelse(delta >= lower_ci &
                            delta <= upper_ci,
                          TRUE,
                          FALSE),
    significant_delta = ifelse(delta != 0,
                               TRUE,
                               FALSE),
    significant_est = ifelse(lower_ci > 0 | upper_ci < 0,
                             TRUE,
                             FALSE),
    true_positive = ifelse(significant_est == TRUE & significant_delta == TRUE,
                           1,
                           0),
    true_negative = ifelse(significant_est == FALSE & significant_delta == FALSE,
                           1,
                           0),
    false_positive = ifelse(significant_est == TRUE & significant_delta == FALSE,
                            1,
                            0),
    false_negative = ifelse(significant_est == FALSE & significant_delta == TRUE,
                            1,
                            0)
  )
