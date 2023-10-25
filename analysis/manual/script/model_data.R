library(tidyverse)
library(here)
library(future)
library(furrr)
library(purrr)
library(skimr)
plan(multisession, workers = future::availableCores()[[1]] - 2)
source(here("analysis/simulation/script/test.R"))

# Helpers --------------------------------------------------------------------

standardise <- function(x) {
  x <- as.numeric(x)
  d <- ifelse(is.finite(x),
              (x - 1) / (x + 1),
              1.0)
  return(d)
}

reverse_standardise <- function(d) {
  x <- (1 + d) / (1 - d)
  return(x)
}

normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

read_files <- function(path) {
  files <- list.files(path = path, pattern = "*.rds", full.names = TRUE)
  files <- furrr::future_map(files, readRDS, .options = furrr_options(seed = NULL))
  return(files)
}

remove_object <- function(list, object_name) {
  list <- list[!names(list) %in% object_name]
  return(list)
}


# Data ----------------------------------------------------------------------
scenarios <- read_files(here("analysis/manual/data", "scenarios"))
results <- read_files(here("analysis/manual/data", "results"))
test_n_groups(scenarios, results, n_sample = length(scenarios))
test_skim_output(scenarios, results, n_sample = length(scenarios))

scenarios_df <-
  map(.x = scenarios,
      ~ remove_object(.x,
                      c("generation_time", "incubation_period", "duration"))) %>%
  bind_rows() %>%
  mutate(delta = o2groups::scale(delta))

results_df <- bind_rows(results) %>%
  mutate(across(
    .cols = c("est", "lower_ci", "upper_ci"),
    .names = "{.col}",
    .fns = o2groups::scale
  ))

model_df <-
  left_join(results_df,
            scenarios_df,
            by = c("scenario", "name")) %>%
  mutate(
    beta = successes / trials,
    intro_prop = intro_n / size,
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
  ) %>%
  select(
    param,
    scenario,
    simulation,
    peak_coeff,
    name,
    n_groups,
    size,
    intro_n,
    intro_prop,
    r0,
    delta,
    est,
    lower_ci,
    upper_ci,
    bias,
    is_within_ci,
    significant_delta,
    significant_est,
    true_positive,
    true_negative,
    false_positive,
    false_negative,
    successes,
    trials,
    beta
  )




# Calculate coverage, bias, and significance aggregated over simulations
outcomes_df <- model_df %>%
  group_by(scenario, peak_coeff, name) %>%
  summarise(
    coverage = sum(is_within_ci, na.rm = TRUE) / n(),
    bias = mean(bias, na.rm = TRUE),
    significance = sum(significant_est == significant_delta, na.rm = TRUE) / n(),
    sensitivity = sum(true_positive, na.rm = TRUE) / sum(significant_delta, na.rm = FALSE),
    specificity = sum(true_negative, na.rm = TRUE) / sum(significant_delta == FALSE, na.rm = FALSE),
    ppv = sum(true_positive, na.rm = TRUE) / sum(significant_est, na.rm = FALSE),
    npv = sum(true_negative, na.rm = TRUE) / sum(significant_est == FALSE, na.rm = FALSE),
    trials = mean(trials, na.rm = TRUE),
    successes = mean(successes, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(scenarios_df, by = c("scenario", "name")) %>%
  mutate(intro_prop = intro_n / size,
         beta = successes / trials)

dir.create(here("analysis/manual/data", "model"))
library(data.table)
fwrite(setDT(scenarios_df), here("analysis/manual/data/model", "scenarios_df.csv"))
fwrite(setDT(results_df), here("analysis/manual/data/model", "results_df.csv"))
fwrite(setDT(model_df), here("analysis/manual/data/model", "model_df.csv"))
fwrite(setDT(outcomes_df), here("analysis/manual/data/model", "outcomes_df.csv"))
