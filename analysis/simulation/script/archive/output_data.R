library(tidyverse)
library(here)
library(future)
library(furrr)
library(purrr)
library(skimr)
plan(multisession, workers = future::availableCores()[[1]] - 2)
source(here("analysis/simulation/script/simulations/test.R"))

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
scenarios <- read_files(here("analysis/simulation/data", "scenarios"))
results <- read_files(here("analysis/simulation/data", "results"))
test_n_groups(scenarios, results, n_sample = 100)
test_skim_output(scenarios, results, n_sample = 100)

scenarios_df <-
  map(.x = scenarios,
      ~ remove_object(.x,
                      c("generation_time", "incubation_period", "duration", "delta"))) %>%
  bind_rows() %>%
  rename(delta = scaled_delta)


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
    scenario,
    simulation,
    name,
    peak_coeff,
    peak_date,
    delta,
    est,
    lower_ci,
    upper_ci,
    bias,
    successes,
    trials,
    n_cases,
    total_cases,
    is_within_ci,
    significant_delta,
    significant_est,
    true_positive,
    true_negative,
    false_positive,
    false_negative,
    n_groups,
    size,
    intro_n,
    intro_prop,
    r0,
    GT_mean,
    GT_sd,
    INCUB_mean,
    INCUB_sd
  )




# Metrics over all simulations
outcomes_df <- model_df %>%
  group_by(scenario, peak_coeff, name) %>%
  summarise(
    peak_date = mean(peak_date, na.rm = TRUE),
    coverage = sum(is_within_ci, na.rm = TRUE) / n(),
    bias = mean(bias, na.rm = TRUE),
    # significance = sum(significant_est == significant_delta, na.rm = TRUE) / n(),
    sensitivity = sum(true_positive, na.rm = TRUE) / sum(significant_delta, na.rm = FALSE),
    specificity = sum(true_negative, na.rm = TRUE) / sum(significant_delta == FALSE, na.rm = FALSE),
    ppv = sum(true_positive, na.rm = TRUE) / sum(significant_est, na.rm = FALSE),
    npv = sum(true_negative, na.rm = TRUE) / sum(significant_est == FALSE, na.rm = FALSE),
    trials = mean(trials, na.rm = TRUE),
    successes = mean(successes, na.rm = TRUE),
    n_cases = mean(n_cases, na.rm = TRUE),
    total_cases = mean(total_cases, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(scenarios_df, by = c("scenario", "name")) %>%
  ungroup() %>%
  mutate(intro_prop = intro_n / size,
         group_susceptibles = 1 - (n_cases / size)) %>%
  group_by(scenario, peak_coeff) %>%
  mutate(
    size_freq = size / sum(size, na.rm = TRUE),
    sd_peak_date = sd(peak_date, na.rm = TRUE),
    sd_n_cases = sd(n_cases, na.rm = TRUE),
    sd_size = sd(size, na.rm = TRUE),
    sd_size_freq = sd(size_freq, na.rm = TRUE),
    sd_delta = sd(delta, na.rm = TRUE),
    sd_r0 = sd(r0, na.rm = TRUE),
    sd_intro_n = sd(intro_n, na.rm = TRUE),
    sd_intro_prop = sd(intro_prop, na.rm = TRUE),
    sd_group_susceptibles = sd(group_susceptibles, na.rm = TRUE),
    total_susceptibles = 1 - (sum(n_cases) / sum(size))
  ) %>%
  ungroup()

# mutate(significance = ifelse(delta == 0, 1 - significance, significance))


test_outcomes_df_conditions(outcomes_df)

# Save -----------------------------------------------------------------------
dir.create(here("analysis/simulation/data", "model"))
saveRDS(scenarios_df, here("analysis/simulation/data/model", "scenarios_df.rds"))
saveRDS(results_df, here("analysis/simulation/data/model", "results_df.rds"))
saveRDS(model_df, here("analysis/simulation/data/model", "model_df.rds"))
saveRDS(outcomes_df, here("analysis/simulation/data/model", "outcomes_df.rds"))



#library(data.table)
# fwrite(setDT(scenarios_df), here("analysis/simulation/data/model", "scenarios_df.csv"))
# fwrite(setDT(results_df), here("analysis/simulation/data/model", "results_df.csv"))
# fwrite(setDT(model_df), here("analysis/simulation/data/model", "model_df.csv"))
# # VISU ----------------------------------------------------------------------------------------
# #model_df <- readRDS("analysis/simulation/data/model_df.rds")
#
# predictors <- c("delta", "n_groups", "size", "intro_n", "r0", "GT_mean", "GT_sd")
# plot_relationship <- function(predictor) {
#   # Aggregate data by the dependent variable and calculate mean bias
#   aggregated_data <- model_df %>%
#     group_by(peak_coeff, .data[[predictor]]) %>%
#     summarize(mean_bias = mean(bias))
#
#   # Create a scatter plot
#   ggplot(aggregated_data, aes(x = .data[[predictor]], y = mean_bias)) +
#     geom_point(aes(col = peak_coeff)) +
#     geom_hline(aes(yintercept = 0), col = "steelblue")+
#     labs(x = predictor, y = "Mean Bias") +
#     ggtitle(paste("Relationship between", predictor, "and Mean Bias"))
# }
#
# # plot all relationships using lapply
# lapply(predictors, plot_relationship)
# plot_relationship("r0")
#
#
# library(corrplot)
#
# correlation_matrix <- cor(model_df[, c("bias", predictors)])
# corrplot(correlation_matrix, method = "color")
#
#
#
#
# # Modelling -----------------------------------------------------------------------------------
#
# predictors <- c("delta", "n_groups", "size", "intro_n", "r0", "GT_mean", "GT_sd")
# outcome <- "bias"
# summary_df <- model_df %>%
#   filter(as.numeric(scenario) <= 400) %>%
#   group_by(scenario, simulation, peak_coeff) %>%
#   summarise(across(all_of(c(predictors, outcome)), list(mean = ~mean(.), sd = ~sd(.)),
#                    .names = "{.col}_{.fn}"))
#
# # plot th relationship between bias and delta using density plots
# ggplot(summary_df, aes(x = delta_mean, y = bias_mean)) +
#   geom_point(aes(col = peak_coeff)) +
#   geom_hline(aes(yintercept = 0), col = "steelblue")+
#   labs(x = "Delta", y = "Bias") +
#   ggtitle("Relationship between Delta and Bias")
#
#
# ggplot(summary_df, aes(x = delta_mean, y = bias_mean)) +
#   geom_density_2d(aes(fill = ..level..), alpha = 0.6) +
#   geom_hline(yintercept = 0, col = "steelblue") +
#   scale_fill_gradient(low = "lightblue", high = "darkblue") +
#   labs(x = "Delta", y = "Bias") +
#   ggtitle("Relationship between Delta and Bias using Density Plots")
#
#
# # Melting the summary_df for easier plotting
# melted_summary <- summary_df %>%
#   pivot_longer(cols = starts_with(predictors), names_to = "predictor", values_to = "value")
#
# ggplot(melted_summary, aes(x = value, y = bias_mean )) +
#   geom_violin() +
#   facet_wrap(~predictor, scales = "free_x")+
#   labs(x = "Predictor", y = "Bias") +
#   ggtitle("Distribution of Bias by Predictor")
#
#
# # Creating density distribution plots
# ggplot(melted_summary, aes(x = value, y = ..density.., fill = predictor)) +
#   geom_density(alpha = 0.5) +
#   facet_wrap(~ predictor, scales = "free") +
#   labs(x = "Value", y = "Density", fill = "Predictor") +
#   ggtitle("Density Distribution of Bias by Predictor")
#
#
#
# # MODELLING -------------------------------------------------------------------
# #model_df <- readRDS("analysis/simulation/data/model_df.rds")
# library(lme4)
#
# model_formula <- "bias ~ size + delta + n_groups + (1 | scenario) + (1 | simulation)"
# model <- lmer(model_formula, data = model_df)
# model <- lmer(bias ~  delta  + n_groups + size + intro_n + r0 + GT_mean + GT_sd
#               + (1 | scenario/simulation/peak_coeff),
#               data = model_df)
#
#
#
#
# # Fit the model
# fit <- summary(model)
#
# # View the model summary
# print(fit)
#
#
#
# library(randomForest)
# # Convert factors to numeric for random forest
# model_df$scenario <- as.numeric(model_df$scenario)
# model_df$simulation <- as.numeric(model_df$simulation)
# model_df$peak_coeff <- as.numeric(model_df$peak_coeff)
# model_df$group <- as.numeric(model_df$group)
#
# # Define the Random Forest model
# rf_model <- randomForest(bias ~ delta + n_groups + size + intro_n + r0 + GT_mean + GT_sd, data = model_df)
#
# # View the model summary
# print(rf_model)
# summary(rf_model)
#
# # Feature importance plot
# varImpPlot(rf_model, main = "Variable Importance Plot")
#

