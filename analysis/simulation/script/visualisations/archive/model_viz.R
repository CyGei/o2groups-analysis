library(tidyverse)
library(here)
library(latex2exp)
# Helpers -----------------------------------------------------------------
source(here("analysis/simulation/script/plot_helpers.R"))

# Data --------------------------------------------------------------------
scenarios_df <-
  readRDS(here("analysis/simulation/data/model", "scenarios_df.rds"))
# model_df is the most granular data on simulation level
model_df <-
  readRDS(here("analysis/simulation/data/model", "model_df.rds"))
# outcomes_df summarises across all simulations
outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

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
predictors_latex <- c(
  r("$\tau_{\ast \leftarrow a}$"),
  r("$\tau_{a \leftarrow a}$"),
  "$\beta_{a \leftarrow a}$",
  "w_{\sigma}",
  "w_{\mu}",
  "v_{\sigma}",
  "v_{\mu}",
  "\delta",
  "Number of Groups",
  "Group Size",
  "Proportion of Introductions",
  "Basic Reproduction Number"
)
predictors <- c(
   "delta",
    "Number of Groups",
    "Group Size",
    "Proportion of Introductions",
    "R0",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd",
    "Within Group Transmissions",
    "Total Group Transmissions",
    "Within Group Transmission Rate"
)

metrics <-
  c("coverage",
    "bias",
    "sensitivity",
    "specificity")

# Pivot the dataframe into long format.
# note for significance:
# significance should always be closest to 1
# When delta is 0 we do 1 - significance
outcomes_long <- outcomes_df %>%
  mutate(significance = ifelse(delta == 0, 1 - significance, significance)) %>%
  rename("Number of Groups" = n_groups,
         "Group Size" = size,
         "R0" = r0,
         "Proportion of Introductions" = intro_prop,
         "Within Group Transmissions" = successes,
         "Total Group Transmissions" = trials,
         "Within Group Transmission Rate" = beta) %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value")
outcomes_long$predictor_name <- factor(outcomes_long$predictor_name, levels = predictors)

mean_line <- outcomes_long %>%
  group_by(peak_coeff, predictor_name, outcome_name) %>%
  mutate(bin = cut(predictor_value, breaks = 100)) %>%
  group_by(peak_coeff, predictor_name, outcome_name, bin) %>%
  summarise(mean_outcome_value = mean(outcome_value, na.rm = TRUE),
            mid = extract_and_calculate_midpoint(bin))
# lowerQuantile = quantile(outcome_value, 0.25, na.rm = TRUE),
# upperQuantile = quantile(outcome_value, 0.75, na.rm = TRUE))

# PLOTS -------------------------------------------------------------------

#####################
# Grid Histogram
#####################
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
             aes(x = mean, y = 0, col = "mean"),
             size = 2) +
  geom_point(data = summary,
             aes(x = median, y = 0, col = "median"),
             size = 2) +
  geom_errorbarh(data = summary,
                 aes(xmin = lower_quantile,
                     xmax = upper_quantile,
                     y = 0,
                     color = "95% quantile interval"),
                 height = 0.1) +
  facet_wrap( ~ as.factor(predictor_name), scales = "free") +
  theme_classic() +
  theme(
    strip.text.x = element_text(size = 10, family = "serif"),
    strip.text.y = element_text(size = 10, family = "serif")
  ) +
  labs(x = "predictor value", y = "count", color = "")+
  scale_color_manual(values = c("#ff2cdc", "orange", "steelblue"))

ggsave(
  here("analysis/simulation/plots", "hist.png"),
  plot = p_hist,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

library(rasterly)
p_raster <-  rasterly::ggRasterly(
  data = outcomes_df %>% drop_na(bias),
  mapping = aes(x = delta, y = bias, color = peak_coeff),
  color = viridis_pal(alpha = 0.8, option = "plasma")(10)
)
p_raster+
  scale_y_continuous(limits = c(-2,2))

# true vs estimate plot
outcomes_df %>%
  mutate(est = delta  - bias) %>%
  ggplot(aes(x = delta, y = est)) +
  facet_wrap(~ peak_coeff) +
  geom_point(alpha = 0.4) +
  geom_abline(col = "steelblue") +
  theme_classic() +
  labs(x = "Truth", y = "Estimate")

#ROC
outcomes_df %>%
  group_by(peak_coeff) %>%
  summarise(
    mean_sensitivity = mean(sensitivity, na.rm = TRUE),
    mean_specificity = mean(specificity, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = 1 - mean_specificity, y = mean_sensitivity,
             col = peak_coeff)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed") +
  scale_color_viridis_c() +
  labs(x = "1 - Mean Specificity", y = "Mean Sensitivity") +
  lims(x = c(0, 1), y = c(0, 1)) +
  theme_classic()

# Sensitivity vs/ Specificity Ratio
outcomes_df %>%
  group_by(peak_coeff) %>%
  summarise(
    mean_sensitivity = mean(sensitivity, na.rm = TRUE),
    mean_specificity = mean(specificity, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = mean_sensitivity, y = mean_specificity,
             col = peak_coeff)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed") +
  scale_color_viridis_c() +
  lims(x = c(0, 1), y = c(0, 1)) +
  theme_classic()

# Violin --------------------------------------------------------------
mean_sd <- function(x) {
  m <- mean(x, na.rm = TRUE)
  ymin <- m - sd(x, na.rm = TRUE)
  ymax <- m + sd(x, na.rm = TRUE)
  return(c(y = m, ymin = ymin, ymax = ymax))
}

med <- function(x) {
  m <- median(x, na.rm = TRUE)
  return(c(y = m))
}
mean_y <- function(x) {
  m <- mean(x, na.rm = TRUE)
  return(c(y = m))
}
# Violin Plot
hline_dat = data.frame(
  outcome_name =  unique(outcomes_long$outcome_name),
  target = c(0.95, 0, 1, 1)
)
# Create a function to specify different y-axis limits for each facet
ylim_function <- function(Group) {
  if ( outcome_name == "bias") {
    return(c(-2, 2))
  } else {
    return(c(0,1))
  }
}

p_violin <- outcomes_long %>%
  mutate(peak_coeff = as.factor(peak_coeff)) %>%
  ggplot(aes(x = peak_coeff,
             y = outcome_value,
             y_min = min(outcome_value),
             y_max = max(outcome_value),
             fill = peak_coeff)) +
  facet_wrap(~ outcome_name,
             scales = "free") +
  geom_violin(trim = TRUE,
              scale = "count",
              bw = 0.01,
              color = "gray") +
  #coord_cartesian(limits = ylim_function)+
  # stat_summary(fun.data = mean_sd,  col = "black") +
  stat_summary(
    fun.data = mean_y,
    col = "black",
    geom = "point"
  ) +
  stat_summary(
    fun.data = med,
    shape = 1,
    col = "white",
    geom = "point"
  ) +
  geom_hline(data = hline_dat, aes(yintercept = target), linetype = "solid") +
  scale_fill_viridis_d("Peak Coefficient") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "", y = "Metric Value")

p_violin <- p_violin +
  ggh4x::facetted_pos_scales(y = list(
    outcome_name == "bias" ~ scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)),
    outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
    outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
    outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
  ))
p_violin

ggsave(
  plot = p_violin,
  here("analysis/simulation/plots", "violin.png"),
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

#calculate median and mean sensitivity by peak_coeff
outcomes_df %>%
  group_by(peak_coeff) %>%
  summarise(
    mean_sensitivity = mean(sensitivity, na.rm = TRUE),
    median_sensitivity = median(sensitivity, na.rm = TRUE),
    mean_specificity = mean(specificity, na.rm = TRUE),
    median_specificity = median(specificity, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(mean_sensitivity, median_sensitivity, mean_specificity, median_specificity),
               names_to = "metric",
               values_to = "value") %>%
  ggplot(aes(x = peak_coeff,
              y = value,
              col = metric)) +
  geom_point() +
  geom_line()+
  theme_classic() +
  labs(x = "Peak Coefficient", y = "Metric Value")




p_peak_bias <- outcomes_df %>%
  group_by(peak_coeff) %>%
  summarise(
    mean = mean(bias, na.rm = T),
    median = median(bias, na.rm = T)
  ) %>%
  ggplot(aes(
    x = mean,
    y = median,
    col = as.factor(peak_coeff)
  )) +
  geom_point(size = 1.2) +
  scale_color_viridis_d("Peak Coefficient") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_classic() +
  labs(
    x = "Mean Bias",
    y = "Median Bias")
p_peak_bias

outcomes_df %>%
  group_by(scenario, peak_coeff) %>%
  summarise(mean = mean(bias, na.rm = T),
            median = median(bias, na.rm = T)) %>%
  ggplot(aes(
    x = mean,
    y = median,
    col = as.factor(peak_coeff)
  )) +
  geom_density2d()+
  scale_color_viridis_d("Peak Coefficient") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_classic() +
  labs(
    x = "Mean Bias",
    y = "Median Bias")

ggsave(
  here("analysis/simulation/plots", "peak_bias.png"),
  plot = p_peak_bias,
 width = 16,
  height = 8,
  units = "in",
  dpi = 300
)


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


outcome_summary2 <- outcomes_df %>%
  select(all_of(metrics), peak_coeff, scenario) %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value") %>%
  dplyr::group_by(peak_coeff, outcome_name, scenario) %>%
  dplyr::summarise(
    mean = mean(outcome_value, na.rm = TRUE),
    sd = sd(outcome_value, na.rm = TRUE)
  )

p_peak_outcomes2 <- ggplot(outcome_summary2,
                           aes(x = mean,
                               y = sd,
                               color = as.factor(peak_coeff))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~outcome_name, scales = "free") +
  labs(x = "Mean", y = "Standard Deviation") +
  scale_color_viridis_d(name = "Peak Coefficient") +
  theme_classic()

ggsave(
  here("analysis/simulation/plots", "peak_outcomes2.png"),
  plot = p_peak_outcomes2,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

# HEXBIN ------------------------------------------------------------------


p_hexbin <- outcomes_long %>%
  #mutate(predictor_name = factor(predictor_name, levels = predictors)) %>% # set the levels of predictor_name in the desired order
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
  geom_hline(data = hline_dat, aes(yintercept = target), linetype = "solid") +


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
  here("analysis/simulation/plots", "hexbin2.png"),
  plot = p_hexbin,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)


# # scatter -----------------------------------------------------------------
#
# point_data <- rsample::initial_split(outcomes_long,
#                                      prop = 0.3,
#                                      strata = peak_coeff,
#                                      breaks = length(unique(outcomes_long$peak_coeff)))
#
# p_scatter <- training(point_data ) %>%
#   ggplot(aes(x = predictor_value, y = outcome_value, color = peak_coeff, group = peak_coeff)) +
#   facet_grid(outcome_name ~ predictor_name, scales = "free") +
#   geom_point(alpha = 0.2, size = 0.5)+
#   geom_smooth(
#     method = "gam",
#     formula = y ~ s(x, bs = "cs", k = 7),
#     # k=7 because max n_groups = 8
#     linewidth = 0.5,
#     alpha = 1
#   ) +
#   theme_classic() +
#   theme(
#     legend.position = "bottom",
#     panel.background = element_rect(fill = "#e7e7e7"),
#     strip.text.x = element_text(size = 12, family = "serif"),
#     strip.text.y = element_text(size = 12, family = "serif")
#   ) +
#   labs(
#     title = "Relationship between predictors and outcomes",
#     x = "predictor value",
#     y = "outcome value",
#     caption = "smooth line was computed using a Generalized Additive Model (GAM) smoothing technique with a cubic spline function and 7 knots."
#   )
#
# ggsave(
#   here("analysis/simulation/plots", "scatter.png"),
#   plot = p_scatter,
#   width = 16,
#   height = 8,
#   units = "in",
#   dpi = 300
# )

# OLD HEXBIN PLOT ---------------------------------------------------------
#
# cell_df <- do.call("rbind",
#                    split(
#                      outcomes_long,
#                      interaction(outcomes_long$outcome_name, outcomes_long$predictor_name)
#                    ) |>
#                      lapply(function(d) {
#                        hb <- hexbin::hexbin(d$predictor_value,
#                                             d$outcome_value,
#                                             xbins = 100,
#                                             IDs = TRUE)
#                        cbind(
#                          aggregate(d$peak_coeff, by = list(hb@cID), FUN = mean),
#                          count = hb@count,
#                          X = hexbin::hcell2xy(hb)$x,
#                          Y = hexbin::hcell2xy(hb)$y,
#                          outcome_name = d$outcome_name[1],
#                          predictor_name = d$predictor_name[1]
#                        )
#                      }))
#
#
# p_hexbin <- cell_df %>%
#   ggplot() +
#   facet_grid(outcome_name ~ predictor_name, scales = "free") +
#   geom_hex(aes(
#     x = X,
#     y = Y,
#     fill = x,
#     alpha = count
#   ),
#   stat = "identity") +
#   geom_line(data = mean_line,
#             aes(
#               y = mean_outcome_value,
#               x = mid,
#               col = peak_coeff,
#               group = factor(peak_coeff)
#             )) +
#   scale_fill_viridis_c(option = "inferno") +
#   scale_color_viridis_c(option = "inferno",
#                         guide = "none") +
#   scale_alpha_continuous(range = c(0.8, 1), guide = "none") +
#   theme_classic() +
#   theme(
#     legend.position = "bottom",
#     panel.background = element_rect(fill = "#d3d3d3"),
#     strip.text.x = element_text(size = 12, family = "serif"),
#     strip.text.y = element_text(size = 12, family = "serif")
#   ) +
#   labs(
#     title = "Relationship between predictors and outcomes",
#     fill = "Peak Coefficient",
#     x = "predictor value",
#     y = "outcome value",
#     caption = "Color: Represents the mean value of the peak coefficient within each hexagonal bin.
# Transparency: Indicates data density, with more transparent bins having fewer data points.
#        Line: average estimate"
#   )
#
# ggsave(
#   here("analysis/simulation/plots", "hexbin.svg"),
#   plot = p_hexbin,
#   width = 16,
#   height = 8,
#   units = "in",
#   dpi = 300
# )
# # the plot represents a grid of hexagonal bins, where each bin's color (fill) is determined by the mean value of the factor variable within that bin. The transparency (alpha) of each bin is based on the count of data points within the bin, with more transparent bins representing areas with fewer data points.
#
# p_hexbin




#
# formula <- y ~ x
# plot_scatter(model_df,
#              x = "r0",
#              y = "bias") +
#   stat_smooth(method = "lm") +
#   ggpubr::stat_regline_equation(aes(label = paste(..eq.label.., "   ", ..adj.rr.label.., sep = "~")),
#                                 col = "steelblue")
#
# formula <- y ~ poly(x, 2, raw = TRUE)
# plot_scatter(model_df,
#              x = "size",
#              y = "bias",
#              facet_vars = "peak_coeff") +
#   stat_smooth(method = "lm", formula = formula) +
#   ggpubr::stat_regline_equation(aes(label = paste(..eq.label.., "   ", ..adj.rr.label.., sep = "~")),
#                                 formula = formula,
#                                 col = "steelblue")
#
#
# plot_heatmap(
#   outcomes_df,
#   "delta",
#   "bias",
#   x_breaks = seq(-1, 1, 0.1),
#   y_breaks = seq(-2, 2, 0.1),
#   bin = FALSE,
#   min_alpha = 0.8
# )



# outcomes_df %>%
#   filter(peak_coeff == 1.3) %>%
#   select(delta, bias) %>%
#   ggplot(aes(x = sqrt(delta), y = bias))+
#   geom_point()+
#   geom_smooth()
#
#
# outcomes_df %>%
#   filter(peak_coeff == 1.3) %>%
#   select(trials, bias) %>%
#   ggplot(aes(x = 1/trials, y = bias))+
#   geom_point()+
#   geom_smooth(method= "lm")+
#   ggpubr::stat_regline_equation()
#
# outcomes_df %>%
#   filter(peak_coeff == 1.3) %>%
#   select(r0, bias) %>%
#   ggplot(aes(x = 1/r0, y = bias))+
#   geom_point()+
#   geom_smooth(method= "lm")+
#   ggpubr::stat_regline_equation()




# Correlations ------------------------------------------------------------

corr <- outcomes_df %>%
  select(!c(scenario, name, peak_coeff)) %>%
  drop_na() %>%
  cor()

p.mat <- outcomes_df %>%
  select(!c(scenario, name, peak_coeff)) %>%
  drop_na() %>%
  ggcorrplot::cor_pmat()

ggcorrplot::ggcorrplot(
  corr = corr,
  type = "lower",
  lab = TRUE,
  hc.order = TRUE,
  p.mat = p.mat,
  method = "circle"
)

corrplot::corrplot(
  corr,
  type = "upper",
  order = "hclust",
  col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu")
)
