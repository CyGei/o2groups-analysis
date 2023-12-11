library(tidyverse)
library(here)
library(furrr)
library(patchwork)
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

label <- c(
  "delta[G]",
  "s[G]",
  "p[s[G]]",
  "n[intro[G]]",
  "p[intro[G]]",
  "R[0[G]]",
  "n[cases[G]]",
  "p[susceptibles[G]]",
  "n[G %<-% G]",
  "n[. %<-% G]",
  "n[groups]",
  "sum(n[cases[G]])",
  "p[susceptibles]",
  "sigma[peak]",
  "sigma[n[cases[G]]]",
  "sigma[p[susceptibles[G]]]",
  "sigma[s[G]]",
  "sigma[p[s[G]]]",
  "sigma[delta[G]]",
  "sigma[R[0[G]]]",
  "sigma[n[intro[G]]]",
  "sigma[p[intro[G]]]",
  "mu[GT]",
  "sigma[GT]",
  "mu[incub]",
  "sigma[incub]"
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

summary <- summary_long %>%
  group_by(predictor_name) %>%
  summarise(
    mean = mean(predictor_value, na.rm = TRUE),
    sd = sd(predictor_value, na.rm = TRUE),
    median = median(predictor_value, na.rm = TRUE),
    upper_quantile = quantile(predictor_value, 0.975, na.rm = TRUE),
    lower_quantile = quantile(predictor_value, 0.025, na.rm = TRUE)
  )

p_hist <- summary_long %>%
  ggplot() +
  geom_histogram(aes(x = predictor_value),
                 bins = 80) +
  geom_point(data = summary,
             aes(x = mean, y = 0, shape = "mean"),
             size = 2) +
  geom_point(data = summary,
             aes(x = median, y = 0, shape = "median"),
             size = 2) +
  geom_errorbarh(
    data = summary,
    aes(
      xmin = lower_quantile,
      xmax = upper_quantile,
      y = 0,
      color = "95% quantile interval"
    ),
    height = 0.1
  ) +
  facet_wrap( ~ predictor_name,
              labeller = label_parsed, scales = "free") +
  labs(x = "predictor value", y = "count", color = "") +
  scale_shape_manual("", values = c("mean" = 16, median = 15)) +
  scale_color_manual("", values = c("95% quantile interval" = "black")) +
  theme_publication() +
  theme(
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.position = c(0.75, 0.1),
    legend.text = element_text(size = rel(1.1))
  ) +
  guides(shape = guide_legend(override.aes = list(size = 2, linewidth = 2)),
         linetype = guide_legend(override.aes = list(size = 2, linewidth = 2)))


ggsave(
  here("analysis/simulation/plots", "hist.png"),
  plot = p_hist,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)




##################################
# Violin & ROC
##################################
source(here("analysis/simulation/script/visualisations", "violin_ROC.R"))


##################################
# Hexbin
# p_hexbin
##################################
source(here("analysis/simulation/script/visualisations", "hexbin.R"))


# OTHER ANALYSES ----------------------------------------------------------

##################################
# Delta density by delta_type & metric
##################################

source(here("analysis/simulation/script/visualisations/delta_density2d.R"))



# Diagram -----------------------------------------------------------------

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

p_delta_saturation <- summary_df %>%
  filter(peak_coeff > 0.5) %>%
  select(c("sensitivity", "group_susceptibles", "delta", "peak_coeff")) %>%
  filter(delta != 0) %>%
  ggplot(aes(x = delta,
             y = group_susceptibles)) +
  facet_wrap( ~ peak_coeff) +
  geom_point(
    aes(col = sensitivity),
    shape = 16,
    stroke = 0,
    alpha = 0.5
  ) +
  # stat_density_2d(
  #   aes(fill = ..level..),
  #   contour = TRUE,
  #   bins = 100,
  #   contour_var = "ndensity",
  #   geom = "polygon"
  #   # colour = "white",
  #   # linewidth = 0.1
  # ) +
  geom_smooth(
    se = TRUE,
    method = "lm",
    formula = y ~ x,
    linewidth = 1,
    alpha = 1,
    col = "black"
  ) +
  #scale_colour_viridis_c(option = "viridis")+
  scale_y_continuous(n.breaks = 10) +
  theme_publication()+
  labs(x = "Delta", y = "Proportion of Susceptibles in the Group")
p_delta_saturation

ggsave(
  here("analysis/simulation/plots", "delta_saturation.png"),
  plot = p_delta_saturation,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

test_df <- summary_df %>%
  filter(peak_coeff == 1) %>%
  filter(delta >=0.25 ) %>%
  select(coverage, delta)
mean(test_df$coverage)
lm(coverage ~ delta, data = test_df) %>% summary()


summary_df %>%
  filter(peak_coeff == 1) %>%
  select(c("coverage", "group_susceptibles", "delta")) %>%
  ggplot(aes(x = delta,
             y = coverage)) +
  geom_point(
    aes(col = group_susceptibles),
    shape = 16,
    stroke = 0,
    alpha = 0.5
  ) +
  geom_smooth() +
  scale_y_continuous(n.breaks = 10)
