library(tidyverse)
library(here)
# Helpers -----------------------------------------------------------------
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))


# Variables ---------------------------------------------------------------
predictors <-
  c(
    "delta",
    # "size",
    "size_freq",
    # "intro_n",
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
  # "s[G]",
  "p[s[G]]",
  # "n[intro[G]]",
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
id_vars <- c("scenario", "peak_coeff", "name")
peak_coeffs_breaks <- sort(c(1, seq(0.5, 1.5, 0.2)))


# Data --------------------------------------------------------------------

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

outcomes_long <- outcomes_df %>%
  select(-c(peak_date, all_of(extra_metrics)))

#predictors <- names(outcomes_long)[!( names(outcomes_long) %in% c(metrics, id_vars, extra_metrics))]

outcomes_long <- outcomes_long %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value")

outcomes_long$predictor_name <-
  factor(outcomes_long$predictor_name,
         levels = predictors,
         labels = label)
outcomes_long$outcome_name <-
  factor(outcomes_long$outcome_name, levels = c(metrics))


hline_df = data.frame(outcome_name =  metrics,
                      target = c(1, 1, 0, 0.95))


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
# Violin plots of outcomes
# p_violin
##################################

summary <- outcomes_long %>%
  group_by(peak_coeff, outcome_name) %>%
  summarise(
    mean = mean(outcome_value, na.rm = TRUE),
    sd = sd(outcome_value, na.rm = TRUE),
    median = median(outcome_value, na.rm = TRUE),
    upper_quantile = quantile(outcome_value, 0.975, na.rm = TRUE),
    lower_quantile = quantile(outcome_value, 0.025, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(
    lowerCI = mean - 1.96 * (sd / sqrt(n)),
    upperCI = mean + 1.96 * (sd / sqrt(n))
  )

summary %>% filter(outcome_name == "bias" & peak_coeff == 1)

outcomes_long %>%
  filter(peak_coeff == 1) %>%
  group_by(scenario, outcome_name) %>%
  summarise(mean = mean(outcome_value, na.rm = TRUE)) %>%
  filter(outcome_name == "bias") %>%
  ungroup() %>%
  summarise(MEAN = mean(mean, na.rm = TRUE),
            lowerCI = t.test(mean, conf.level = 0.95)$conf.int[[1]],
            upperCI = t.test(mean, conf.level = 0.95)$conf.int[[2]])


p_violin <- outcomes_long %>%
  ggplot() +
  facet_wrap( ~ outcome_name,
              scales = "free") +
  geom_violin(
    aes(
      x = as.factor(peak_coeff),
      y = outcome_value,
      fill = peak_coeff
    ),
    trim = TRUE,
    scale = "count",
    bw = 0.01,
    color = "gray"
  ) +
  geom_path(
    data = summary,
    aes(
      x = as.factor(peak_coeff),
      y = mean,
      group = outcome_name
    ),
    color = "#5b5b5b"
  ) +
  geom_point(
    data = summary,
    aes(
      x = as.factor(peak_coeff),
      y = mean,
      shape = "mean"
    ),
    size = 2,
    color = "white",
    fill = "black"
  ) +
  geom_hline(
    data = hline_df,
    aes(yintercept = target,
        linetype = "target value"),
    color = "#E32227",
  ) +
  scale_fill_gradientn(
    "Peak \n Coefficient",
    colours = c("#3679c0", "black", "#ff007f"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    breaks = sort(c(1, seq(0.5, 1.5, 0.2))),
    guide = "none"
  ) +
  scale_shape_manual("", values = c("mean" = 21, "median" = 25)) +
  scale_linetype_manual("", values = c("target value" = "solid")) +
  labs(x = "Peak Coefficient", y = "Metric Value") +
  theme_publication()

p_violin <- p_violin +
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
    )
  )
p_violin
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
    lower_quantile = quantile(outcome_value, 0.025, na.rm = TRUE),
    lowerCI = t.test(outcome_value, conf.level = 0.95)$conf.int[[1]],
    upperCI = t.test(outcome_value, conf.level = 0.95)$conf.int[[2]]
  )

p_peak_outcomes <- ggplot(outcome_summary,
                          aes(color = peak_coeff)) +
  geom_path(aes(x = mean,
                y = sd),
            alpha = 0.3) +
  geom_path(aes(x = median,
                y = sd),
            alpha = 0.3) +
  geom_point(aes(x = mean,
                 y = sd,
                 shape = "mean"),
             size = 3) +
  geom_point(aes(x = median,
                 y = sd,
                 shape = "median"),
             size = 3) +
  facet_wrap(~ outcome_name, scales = "free") +
  labs(x = "Value", y = "Standard Deviation", shape = "") +
  scale_color_gradientn(
    "Peak Coefficient",
    colours = c("#3679c0", "black", "#ff007f"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    breaks = sort(c(1, seq(0.5, 1.5, 0.2))),
    label = c("0.5", "0.7", "0.9", "", "1.1", "1.3", "1.5"),
  ) +
  scale_x_continuous(n.breaks = 10) +
  theme_publication() +
  guides(color = guide_colorbar(size = 5))

p_peak_outcomes <- p_peak_outcomes +
  ggh4x::facetted_pos_scales(
    x = list(
      bias = scale_x_continuous(limits = c(0.025, 0.3), breaks = seq(0, 0.3, 0.05)),
      coverage = scale_x_continuous(limits = c(0.7, 0.975), breaks = seq(0.7, 1, 0.025)),
      sensitivity = scale_x_continuous(limits = c(0, 0.35), breaks = seq(0, 1, 0.05)),
      specificity = scale_x_continuous(limits = c(0.72, 0.99), breaks = seq(0, 0.99, 0.03))
    ),
    y = list(
      bias = scale_y_continuous(limits = c(0.05, 0.35), breaks = seq(0.05, 0.35, 0.05)),
      coverage = scale_y_continuous(limits = c(0.05, 0.30), breaks =seq(0.05, 0.35, 0.05)),
      sensitivity = scale_y_continuous(limits = c(0.05, 0.4), breaks =seq(0.05, 0.4, 0.05)),
      specificity = scale_y_continuous(limits = c(0.05, 0.30), breaks = seq(0.05, 0.35, 0.05))
    )
  )+
  coord_cartesian(expand = 0.01)

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
source(here("analysis/simulation/script/visualisations/hexbin.R"))


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
# ROC point by Peak Coefficient
##################################

roc_df <- outcomes_df %>%
  mutate(FPR = 1 - specificity,
         TPR = sensitivity) %>%
  select(peak_coeff, FPR, TPR) %>%
  pivot_longer(cols = c(FPR, TPR),
               names_to = "Metric",
               values_to = "Value") %>%
  group_by(peak_coeff, Metric) %>%
  summarise(mu = mean(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE),
            lowerCI = t.test(Value, conf.level = 0.95)$conf.int[[1]],
            upperCI = t.test(Value, conf.level = 0.95)$conf.int[[2]])

p_fpr_tpr <- ggplot(roc_df, aes(
  x = peak_coeff,
  y = mu,
  group = Metric
)) +
  geom_line(aes(color = Metric)) +
  geom_point(aes(color = Metric), size = 4) +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI, fill = Metric), alpha = 0.3) +
  labs(x = "Peak Coefficient",
       y = "Mean Value") +
  scale_y_continuous(n.breaks = 10)+
  scale_x_continuous(breaks = c(0.5, 0.7, 0.9, 1, 1.1, 1.3, 1.5))+
  scale_color_manual(values = c("#E17327", "#932191"))+
  scale_fill_manual(values = c("#E17327", "#932191"))+
  theme_publication(base_size = 16)


ggsave(
  here("analysis/simulation/plots", "fpr_tpr.png"),
  plot = p_fpr_tpr,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)


p_roc <- outcomes_df %>%
  mutate(FPR = 1 - specificity,
         TPR = sensitivity) %>%
  select(peak_coeff, FPR, TPR) %>%
  group_by(peak_coeff) %>%
  summarise(across(c(FPR, TPR), function(x) mean(x, na.rm = TRUE)),
            header = "Receiver-Operating Characteristic") %>%
  ggplot(aes(x = FPR, y = TPR, color = peak_coeff)) +
  facet_wrap(~header) +
  geom_path() +
  geom_point(size = 4)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 1, 0.05),
                     limits = c(0, 0.355),
                     expand = c(0, 0))+
  scale_x_continuous(breaks = seq(0, 1, 0.05),
                     limits = c(0, 0.35),
                     expand = c(0, 0))+
  coord_cartesian() +
  scale_color_gradientn(
    "Peak Coefficient\n\n\n",
    colours = c("#3679c0", "black", "#ff007f"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    breaks = sort(c(1, seq(0.5, 1.5, 0.2))),
    guide = guide_colourbar(barwidth = 15, barheight = 1,
                            title.position = "right")
  ) +
    labs(x = "False Positive Rate (1- Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_publication(base_size = 16)

ggsave(
  here("analysis/simulation/plots", "roc.png"),
  plot = p_roc,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)





##################################
# Violin & ROC
##################################


summary <- outcomes_long %>%
  group_by(peak_coeff, outcome_name) %>%
  summarise(
    mean = mean(outcome_value, na.rm = TRUE),
    sd = sd(outcome_value, na.rm = TRUE),
    median = median(outcome_value, na.rm = TRUE),
    upper_quantile = quantile(outcome_value, 0.975, na.rm = TRUE),
    lower_quantile = quantile(outcome_value, 0.025, na.rm = TRUE)
  ) %>%
  filter(outcome_name %in% c("bias", "coverage"))

p_violin <- outcomes_long %>%
  filter(outcome_name %in% c("bias", "coverage")) %>%
  ggplot() +
  facet_wrap( ~ outcome_name,
              scales = "free_y",
              nrow = 2,
              ncol = 1) +
  geom_violin(
    aes(
      x = as.factor(peak_coeff),
      y = outcome_value,
      fill = peak_coeff
    ),
    trim = TRUE,
    scale = "count",
    bw = 0.01,
    color = "gray"
  ) +
  geom_path(
    data = summary,
    aes(
      x = as.factor(peak_coeff),
      y = mean,
      group = outcome_name
    ),
    color = "#E32227",
  ) +
  geom_point(
    data = summary,
    aes(
      x = as.factor(peak_coeff),
      y = mean,
      shape = "mean"
    ),
    size = 2,
    color = "white",
    fill = "black"
  ) +
  geom_hline(
    data = hline_df %>% filter(outcome_name %in% c("bias", "coverage")),
    aes(yintercept = target,
        linetype = "target value"),
    color = "#5b5b5b",
  ) +
  scale_fill_gradientn(
    "Peak \n Coefficient",
    colours = c("#3679c0", "black", "#ff007f"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    breaks = sort(c(1, seq(0.5, 1.5, 0.2))),
    guide = "none"
  ) +
  scale_shape_manual("", values = c("mean" = 21, "median" = 25)) +
  scale_linetype_manual("", values = c("target value" = "solid")) +
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(
        limits = c(-0.5, 1),
        breaks = seq(-0.5, 1.5, 0.25),
        expand = c(0, 0)
      ),
      outcome_name == "coverage" ~ scale_y_continuous(
        limits = c(0.6, 1),
        breaks = seq(0.6, 1, 0.1),
        expand = c(0, 0)
      )

    )
  )+
  labs(x = "Peak Coefficient", y = "Metric Value") +
  theme_publication()+
  guides(shape = guide_legend(override.aes = list(size=4)),
         linetype = guide_legend(override.aes = list(linewidth=1.5)))


library(patchwork)
p_violin_roc <- wrap_plots(p_violin,
                           p_roc,
                           nrow = 1,
                           ncol = 2,
                           guides = "collect") +
  plot_annotation(tag_levels = 'A')  &
  theme(legend.position = "bottom",
        legend.margin = margin(t = 10, unit = "pt"))

ggsave(
  here("analysis/simulation/plots", "violin_roc.png"),
  plot = p_violin_roc,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)



##################################
# Relationship between assorativity & saturation
##################################

p_delta_saturation <- outcomes_df %>%
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

test_df <- outcomes_df %>%
  filter(peak_coeff == 1) %>%
  filter(delta >=0.25 ) %>%
  select(coverage, delta)
mean(test_df$coverage)
lm(coverage ~ delta, data = test_df) %>% summary()


outcomes_df %>%
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
