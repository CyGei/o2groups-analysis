library(tidyverse)
library(here)
# Helpers -----------------------------------------------------------------
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))

# Data --------------------------------------------------------------------

predictors <-
    c(
        "delta",
        "n_groups",
        "size",
        "intro_prop",
        "r0",
        "GT_mean",
        "GT_sd",
        "INCUB_mean",
        "INCUB_sd",
        "trials",
        "successes",
        "beta"
    )

predictors <- c(
  "delta",
  "Number of Groups",
  "Group Size",
  "% of Intro",
  "R0",
  "GT_mean",
  "GT_sd",
  "INCUB_mean",
  "INCUB_sd",
  "Within-Group Trans",
  "Total-Group Trans",
  "Total Trans",
  "Within-Group Trans Rate"
)
parameters <- c(
  "delta",
  "Number of Groups",
  "Group Size",
  "% of Intro",
  "R0",
  "GT_mean",
  "GT_sd",
  "INCUB_mean",
  "INCUB_sd",
  "Within-Group Trans",
  "Total-Group Trans",
  "Total Trans",
  "Within-Group Trans Rate"
)
metrics <-
    c(
        "coverage",
        "bias",
        "sensitivity",
        "specificity"
    )

# outcomes_df summarises across all simulations
outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

outcomes_long <- outcomes_df %>%
  filter(peak_coeff <= 1.3) %>%
  mutate(significance = ifelse(delta == 0, 1 - significance, significance)) %>%
  group_by(scenario, peak_coeff) %>%
  mutate(N_Trans = ifelse(row_number() == 1, sum(trials, na.rm = TRUE), NA)) %>%
  ungroup() %>%
  rename(
    "Number of Groups" = n_groups,
    "Group Size" = size,
    "R0" = r0,
    "% of Intro" = intro_prop,
    "Within-Group Trans" = successes,
    "Total-Group Trans" = trials,
    "Total Trans" = N_Trans,
    "Within-Group Trans Rate" = beta
  ) %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value")
outcomes_long$predictor_name <- factor(outcomes_long$predictor_name, levels = predictors)

hline_df = data.frame(
  outcome_name =  unique(outcomes_long$outcome_name),
  target = c(0.95, 0, 1, 1)
)

# Plots --------------------------------------------------------------------

##################################
# Histogram of predictor values
# p_hist
##################################

summary <- outcomes_long %>%
  group_by(predictor_name) %>%
  summarise(
    mean = mean(predictor_value, na.rm = TRUE),
    sd = sd(predictor_value, na.rm = TRUE),
    median = median(predictor_value, na.rm = TRUE),
    upper_quantile = quantile(predictor_value, 0.975, na.rm = TRUE),
    lower_quantile = quantile(predictor_value, 0.025, na.rm = TRUE)
  )
p_hist <- outcomes_long %>%
  ggplot() +
  geom_histogram(aes(x = predictor_value),
                 bins = 100) +
  geom_point(data = summary,
             aes(x = mean, y = 0, shape = "mean"),
             size = 2) +
  geom_point(data = summary,
             aes(x = median, y = 0, shape = "median"),
             size = 2) +
  geom_errorbarh(data = summary,
                 aes(xmin = lower_quantile,
                     xmax = upper_quantile,
                     y = 0,
                     color = "95% quantile interval"),
                 height = 0.1) +
  facet_wrap( ~ factor(predictor_name, levels = predictors), scales = "free") +
  theme_classic() +
  theme(
    strip.text.x = element_text(size = 10, family = "serif"),
    strip.text.y = element_text(size = 10, family = "serif")
  ) +
  labs(x = "predictor value", y = "count", color = "")+
  scale_shape_manual("", values = c("mean"= 16, median = 15)) +
  scale_color_manual("Metric",values = c("95% quantile interval" = "black"))

ggsave(
  here("analysis/simulation/plots", "hist.png"),
  plot = p_hist,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)


##################################
# Violin plots of outcomes
# p_violin
##################################

p_violin <- outcomes_long %>%
  filter(peak_coeff <= 1.3) %>%
  #mutate(peak_coeff = as.factor(peak_coeff)) %>%
  ggplot(aes(
             y = outcome_value,
             y_min = min(outcome_value),
             y_max = max(outcome_value))) +
  facet_wrap(~ outcome_name,
             scales = "free") +
  geom_violin(aes(x = as.factor(peak_coeff),
                  fill = peak_coeff),
              trim = TRUE,
              scale = "count",
              bw = 0.01,
              color = "gray") +
  stat_summary(
    fun.data = mean_y,
    aes(x = as.factor(peak_coeff),
        shape = "mean"),
    col = "white",
    fill = "black",
    geom = "point"
  ) +
  stat_summary(
    fun.data = med,
    aes(x = as.factor(peak_coeff),
        shape="median"),
    col = "white",
    fill = "black",
    geom = "point"
  ) +
  geom_hline(data = hline_df, aes(yintercept = target), linetype = "solid", col = "gray50") +
  scale_fill_gradientn(
    "Peak \n Coefficient",
    colours = c("#3679c0", "black", "#ff007f"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    breaks = seq(0.7, 1.3, 0.1)
  )+
  scale_shape_manual("", values=c("mean"= 21, "median" = 25)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 16, family = "serif"),
        strip.text.y = element_text(size = 16, family = "serif"),
        legend.text = element_text(size = 16)) +
  labs(x = "Peak Coefficient", y = "Metric Value")
p_violin <- p_violin +
  ggh4x::facetted_pos_scales(y = list(
    outcome_name == "bias" ~ scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)),
    outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
    outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
    outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
  ))
#p_violin
ggsave(
  plot = p_violin,
  here("analysis/simulation/plots", "violin.png"),
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)


##################################
# Scatter plots of Mean Vs SD
# of outcomes by Peak Coeff
# p_peak_outcomes
##################################
outcome_summary <- outcomes_df %>%
  select(all_of(metrics), peak_coeff) %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value") %>%
  dplyr::group_by(peak_coeff, outcome_name) %>%
  dplyr::summarise(
    mean = mean(outcome_value, na.rm = TRUE),
    sd = sd(outcome_value, na.rm = TRUE),
    median = median(outcome_value, na.rm = TRUE),
    upper_quantile = quantile(outcome_value, 0.975, na.rm = TRUE),
    lower_quantile = quantile(outcome_value, 0.025, na.rm = TRUE)
  )

p_peak_outcomes <- ggplot(outcome_summary,
       aes(x = mean,
           y = sd,
           color = as.factor(peak_coeff),
           group = outcome_name)) +
  geom_point() +
  facet_wrap(~outcome_name, scales = "free") +
  labs(x = "Mean", y = "Standard Deviation") +
  scale_color_viridis_d(name = "Peak Coefficient") +
  theme_classic()
p_peak_outcomes

ggsave(
  here("analysis/simulation/plots", "peak_outcomes.png"),
  plot = p_peak_outcomes,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)


##################################
# Hexbin
# p_hexbin
##################################

p_hexbin <- outcomes_long %>%
  ggplot(aes(x = predictor_value, y = outcome_value)) +
  facet_grid(outcome_name ~ as.factor(predictor_name), scales = "free") +
  geom_hex(aes(fill = stat(log(count)),
               alpha = stat(log(count))),
           bins = 100) +

  geom_smooth(
    aes(color = as.factor(peak_coeff),
        group = as.factor(peak_coeff)),
    se = FALSE,
    method = "gam",
    formula = y ~ s(x, bs = "cs", k = 7),
    # k=7 because max n_groups = 8
    linewidth = 0.5,
    alpha = 1
  ) +
  geom_hline(data = hline_df, aes(yintercept = target), linetype = "solid") +


  scale_fill_viridis_c(
    option = "inferno",
    # limits = c(1, 10),
    # trans = "log",
    # # produces a warning message but doesn't affect plot
    # begin = 0.1,
    # end = 0.85,
    # label = function(x)
    #   round(exp(x)),
    guide = "none"
  ) +
  scale_color_viridis_d()+
  scale_alpha_continuous(range = c(0.8, 1),
                         guide = "none") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "#e7e7e7"),
    strip.text.x = element_text(size = 6.5, family = "serif"),
    strip.text.y = element_text(size = 6.5, family = "serif")
  ) +
  labs(
    #title = "Relationship between predictors and outcomes",
    x = "predictor value",
    y = "outcome value",
    color = "peak coefficient"
    #caption = "smooth line was computed using a Generalized Additive Model (GAM) smoothing technique with a cubic spline function and 7 knots."
  )
p_hexbin <- p_hexbin +
  ggh4x::facetted_pos_scales(y = list(
    outcome_name == "bias" ~ scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)),
    outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.8, 0.95, 1)),
    outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
    outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
  ))

ggsave(
  here("analysis/simulation/plots", "hexbin.png"),
  plot = p_hexbin,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)
