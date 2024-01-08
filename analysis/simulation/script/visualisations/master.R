extrafont::loadfonts(device="win")
library(tidyverse)
library(here)
library(furrr)
library(patchwork)
library(santoku)
#library(dtplyr)
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
  mutate(across(c(peak_coeff, alpha), as.factor)) %>%
  select(-.groups)

summary_long <- summary_df %>%
  select(-c(peak_date, all_of(extra_metrics))) %>% #predictors <- names(summary_long)[!( names(summary_long) %in% c(metrics, id_vars, extra_metrics))]
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value")

# summary_long$predictor_name <-
#   factor(summary_long$predictor_name,
#          levels = predictors,
#          labels = label)
# summary_long$outcome_name <-
#   factor(summary_long$outcome_name, levels = c(metrics))


hline_df = data.frame(outcome_name =  metrics,
                      target = c(1, 1, 0, 0.95))

ls_snapshot <- c(ls(), "ls_snapshot")


# Plots --------------------------------------------------------------------

##################################
# Histogram of predictor values
# p_hist
##################################
source(here("analysis/simulation/script/visualisations", "scenario_histograms.R"))

##################################
# Violin & ROC
##################################
source(here("analysis/simulation/script/visualisations", "violin_ROC.R"))


##################################
# Hexbin
# p_hexbin
##################################
source(here("analysis/simulation/script/visualisations", "hexbin.R"))

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

##################################
# Estimator Heatmap
##################################
source(here("analysis/simulation/script/visualisations", "estimator_heatmap.R"))




# OTHER ANALYSES ----------------------------------------------------------

##################################
# Delta & Gamma
##################################
source(here("analysis/simulation/script/visualisations", "delta_gamma.R"))


##################################
# Peak Coefficient
##################################

alpha <- 1
set.seed(123)
p_peak_coeff <- data.frame(values = rnorm(20000, mean = 10, sd = 2.9)) %>%
  ggplot() +
  geom_histogram(aes(x = values, fill = after_stat(x)),
                 binwidth = 1,
                 alpha = alpha) +
  scale_fill_gradientn(
    "Peak Coefficient",
    colours = c(
      alpha("#3679c0", alpha),
      alpha("black", alpha),
      alpha("#ff007f", alpha)
    ),
    values = scales::rescale(c(-1, -0.2, -0.05, 0, 0.05, 0.2, 1)),
    labels = c(0, 0.5, 1, 1.5, 2)
  ) +
  theme_publication() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = "date of symptom onset", y = "number of cases") +
  guides(color = guide_colorbar(size = 5)) +
  scale_y_continuous(
    labels = function(x)
      x / 100
  ) +
  scale_x_continuous(limits = c(0, 20))
p_peak_coeff


ggsave(
  here("analysis/simulation/plots", "peak_coeff.png"),
  plot = p_peak_coeff,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)





##################################
# Relationship between assorativity & saturation
##################################
source(here("analysis/simulation/script/visualisations", "delta_saturation.R"))

