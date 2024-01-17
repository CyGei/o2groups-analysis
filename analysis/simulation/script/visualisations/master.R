extrafont::loadfonts(device="win")
library(tidyverse)
library(here)
library(furrr)
library(patchwork)
library(santoku)
library(latex2exp)
##################################
# Check that epidemics reached herd immunity
##################################
# source(here("analysis/simulation/script/visualisations","1_AR_last_date.R"))


# Helpers -----------------------------------------------------------------
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))

# Variables ---------------------------------------------------------------
predictors <-
  c(
    "delta",
    "size",
    "size_freq",
    "intro_n",
    "intro_prop",
    "r0",
    "n_cases",
    "group_susceptibles",
    "successes",
    "trials",
    "n_groups",
    "total_cases",
    "total_susceptibles",
    "sd_peak_date",
    "sd_n_cases",
    "sd_group_susceptibles",
    "sd_size",
    "sd_size_freq",
    "sd_delta",
    "sd_r0",
    "sd_intro_n",
    "sd_intro_prop",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd"
  )

metrics <-
  c("sensitivity",
    "specificity",
    "bias",
    "coverage")
extra_metrics <- c("ppv", "npv")
id_vars <- c("scenario", "peak_coeff", "alpha",  "name")
peak_coeff_breaks <- sort(c(1, seq(0.5, 1.7, 0.2), 2, 3, 5))
alpha_breaks <- 1 - c(0.50, 0.75, 0.90, 0.95)

# Data --------------------------------------------------------------------
summary_df <-
  read_files(path = here("analysis/simulation/data", "summary")) %>%
  bind_rows() %>%
  mutate(across(c(peak_coeff, alpha), as.factor))

summary_long <- summary_df %>%
  select(-c(peak_date, all_of(extra_metrics))) %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value")

hline_df = data.frame(outcome_name =  metrics,
                      target = c(1, 1, 0, 0.95))

ls_snapshot <- c(ls(), "ls_snapshot")


# Plots --------------------------------------------------------------------

##################################
# Histogram of scenario parameters
##################################
source(here("analysis/simulation/script/visualisations", "2_scenario_histograms.R"))

##################################
# Violin & ROC results
##################################
source(here("analysis/simulation/script/visualisations", "3_violin_ROC.R"))

##################################
# Hexbin:
# univariate relationship between
# performance metrics and parameters
##################################
source(here("analysis/simulation/script/visualisations", "4_hexbin.R"))

#Bias
summary_df %>%
  filter(alpha == 0.05 & peak_coeff == 1) %>%
  mutate(n_cases_chop =  santoku::chop_evenly(n_cases, intervals = 20)) %>%
  group_by(n_cases_chop) %>%
  summarise(across(all_of(metrics), list(mean = ~mean(.x, na.rm = TRUE),
                                         sd = ~sd(.x, na.rm = TRUE))))
summary_df %>%
  filter(alpha == 0.05 & peak_coeff == 1) %>%
  mutate(size_freq_chop =  santoku::chop_evenly(size_freq, intervals = 20)) %>%
  group_by(size_freq_chop) %>%
  summarise(across(all_of(metrics), list(mean = ~mean(.x, na.rm = TRUE),
                                         sd = ~sd(.x, na.rm = TRUE))))


source(here("analysis/simulation/script/visualisations", "5_hexbin_all.R"))

##################################
# Estimator Grid Diagrams
##################################
source(here("analysis/simulation/script/visualisations", "6_estimator_grid.R"))

##################################
# Delta & Gamma & Peak coeff
# Diagrams
##################################
source(here("analysis/simulation/script/visualisations", "7_delta_gamma_peak_coeff.R"))


##################################
# Relationship between assorativity
# & saturation
##################################
source(here("analysis/simulation/script/visualisations", "8_delta_saturation.R"))

