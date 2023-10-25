library(tidyverse)
library(here)
library(data.table)
library(ggh4x)

# Helpers -----------------------------------------------------------------
source(here("analysis/manual/script/plot_helpers.R"))

# Data --------------------------------------------------------------------
scenarios_df <-
  fread(here("analysis/manual/data/model", "scenarios_df.csv"))
# model_df is the most granular data on simulation level
model_df <-
  fread(here("analysis/manual/data/model", "model_df.csv")) %>%
  drop_na(trials)

# Plot Delta Scaled vs Raw ------------------------------------------------

# plot standardised delta
tibble(
  raw_delta = seq(0, 100, 0.1),
  scaled_delta = o2groups::scale(raw_delta)
) %>%
  ggplot(aes(x = raw_delta, y = scaled_delta)) +
  # colour the assortative and dissortative space
  #geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 1), fill = "#00ffdd", alpha = 0.01) +
  #geom_rect(aes(xmin = 0, xmax = 100, ymin = -1, ymax = 0), fill = "#ff00f7", alpha = 0.01) +
  geom_line() +
  labs(x = "Raw delta", y = "Scaled delta") +
  theme_bw() +
  geom_vline(xintercept = 1, lty = "dotted") +
  geom_hline(yintercept = 0, lty = "dotted") +
  scale_x_continuous(breaks = seq(0, 100, 10),
                     labels = c(seq(0, 90, 10), expression(infinity)),
                     limits = c(0, 100),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.1), limits = c(-1, 1), expand = c(0.01, 0.01))


# PLOT EST ----------------------------------------------------------------


grid_theme <- list(
  ggplot2::theme(
    # editing legend
    # legend.background = element_rect(
    #   fill = "#c0c0c0",
    #   linewidth = 0.5,
    #   linetype = "solid",
    #   colour = "black"
    # ),
    legend.position = "bottom",
    # spacing
    axis.text.x = element_text(), # Show x-axis text
    axis.ticks.x = element_line(), # Show x-axis ticks
    plot.margin = rep(unit(0, "null"), 4),
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.length = unit(0, "null"),
    axis.ticks.margin = unit(0, "null"),
    # editing strips
    strip.switch.pad.grid = unit(1, "cm"),
    strip.background.y = element_rect(fill = "#4C4E52", color = "#4C4E52"),
    strip.background.x = element_rect(fill = "#f0f0e1", color = "#4C4E52"),
    strip.text.y.left = element_text(
      margin = margin(1, 1, 1, 1, "cm"),
      angle = 0,
      color = "white",
      face = "bold"
    ),
    strip.text.x = element_text(
      angle = 0,
      color = "black",
      face = "bold.italic",
      size = 10
    ),
    strip.placement = "outside",
    strip.clip = "off"
  )
)



# BIAS --------------------------------------------------------------------
#mean bias & 95% quantiles

p_bias <- model_df %>%
  drop_na() %>%
  group_by(param, scenario, peak_coeff, name) %>%
  summarise(
    significant_delta = unique(significant_delta),
    est = mean(bias),
    lower = quantile(bias, 0.025),
    upper = quantile(bias, 0.975)
  ) %>%
  ggplot(
    .,
    aes(
      x = name,
      y = est,
      color = as.factor(peak_coeff),
      shape = significant_delta
    )
  ) +
  facet_nested_wrap(
    vars(param, scenario),
    nrow = length(unique(model_df$param)),
    scales = "free_x"
    ) +
  theme(strip.placement = "outside") +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = 0.9)
  ) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_hline(
    yintercept = 0,
    lty = "solid",
    color = "#3d3c3c"
  ) +
  scale_y_continuous(breaks = seq(-2, 2, 0.5), limits = c(-2, 2)) +
  scale_colour_viridis_d()+
  theme_bw()+
  labs(x = "", y = "Mean Bias - 95% Quantile Interval",
       color = "Peak Coefficient",
       shape = "Assortative/Disassortative")+
  grid_theme

ggsave(here("analysis/manual/figures/model", "bias.png"), plot = p_bias,
       width = 10, height = 10, units = "cm", dpi = 300)

# COVERAGE ----------------------------------------------------------------
# mean 95% coverage and 95% binom CI

p_cov <- model_df %>%
  group_by(param, scenario, peak_coeff, name) %>%
  drop_na(is_within_ci) %>%
  dplyr::summarise(
    significant_delta = unique(significant_delta),
    est = binom.test(x = sum(is_within_ci), n = n())$est[[1]],
    lower = binom.test(x = sum(is_within_ci), n = n())$conf.int[[1]],
    upper = binom.test(x = sum(is_within_ci), n = n())$conf.int[[2]]
  ) %>%
  ggplot(
    .,
    aes(
      x = name,
      y = est,
      color = as.factor(peak_coeff),
      shape = significant_delta
    )
  ) +
  facet_nested_wrap(
    vars(param, scenario),
    nrow = length(unique(model_df$param)),
    scales = "free_x"
  ) +
  theme(strip.placement = "outside") +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = 0.9)
  ) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_hline(
    yintercept = 0.95,
    lty = "solid",
    color = "#3d3c3c"
  ) +
 # scale_y_continuous(breaks = seq(0.5, 0.95, 0.15), limits = c(0, 1)) +
  theme_bw()+
  scale_colour_viridis_d()+
  labs(x = "", y = "Coverage - 95% Binomial Interval",
       color = "Peak Coefficient",
       shape = "Assortative/Disassortative")+
  grid_theme


ggsave(here("analysis/manual/figures/model", "coverage.png"),plot = p_cov,
       width = 10, height = 10, units = "cm", dpi = 300)


# sensitivity --------------------------------------------------------------
p_sens <- model_df %>%
  group_by(param, scenario, peak_coeff, name) %>%
  summarise(
    significant_delta = unique(significant_delta),
    est = binom.test(x = sum(true_positive, na.rm = TRUE), n = n())$est[[1]],
    lower = binom.test(x = sum(true_positive, na.rm = TRUE),  n = n())$conf.int[[1]],
    upper = binom.test(x = sum(true_positive, na.rm = TRUE),  n = n())$conf.int[[2]]
  ) %>%
  ggplot(
    .,
    aes(
      x = name,
      y = est,
      color = as.factor(peak_coeff),
      shape = significant_delta
    )
  ) +
  facet_nested_wrap(
    vars(param, scenario),
    nrow = length(unique(model_df$param)),
    scales = "free_x"
  ) +
  theme(strip.placement = "outside") +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = 0.9)
  ) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_hline(
    yintercept = 1,
    lty = "solid",
    color = "#3d3c3c"
  ) +
  #scale_y_continuous(breaks = seq(0.5, 0.95, 0.15), limits = c(0, 1.1)) +
  scale_colour_viridis_d()+
  theme_bw()+
  labs(x = "", y = "Sensitivity - 95% Binomial Interval",
       color = "Peak Coefficient",
       shape = "Assortative/Disassortative")+
  grid_theme

ggsave(here("analysis/manual/figures/model", "sensitivity.png"), plot = p_sens,
       width = 10, height = 10, units = "cm", dpi = 300)



# Specificity -------------------------------------------------------------

p_spec <- model_df %>%
  group_by(param, scenario, peak_coeff, name) %>%
  summarise(
    significant_delta = unique(significant_delta),
    est = binom.test(x = sum(true_negative, na.rm = TRUE), n = n())$est[[1]],
    lower = binom.test(x = sum(true_negative, na.rm = TRUE),  n = n())$conf.int[[1]],
    upper = binom.test(x = sum(true_negative, na.rm = TRUE),  n = n())$conf.int[[2]]
  ) %>%
  ggplot(
    .,
    aes(
      x = name,
      y = est,
      color = as.factor(peak_coeff),
      shape = significant_delta
    )
  ) +
  facet_nested_wrap(
    vars(param, scenario),
    nrow = length(unique(model_df$param)),
    scales = "free_x"
  ) +
  theme(strip.placement = "outside") +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = 0.9)
  ) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_hline(
    yintercept = 1,
    lty = "solid",
    color = "#3d3c3c"
  ) +
  #scale_y_continuous(breaks = seq(0.5, 0.95, 0.15), limits = c(0, 1.1)) +
  scale_colour_viridis_d()+
  theme_bw()+
  labs(x = "", y = "Specificity - 95% Binomial Interval",
       color = "Peak Coefficient", shape = "Assortative/Disassortative")+
  grid_theme

ggsave(here("analysis/manual/figures/model", "specificity.png"), width = 10, height = 10, units = "cm", dpi = 300)





# Patchwork ---------------------------------------------------------------
library(patchwork)
p_patch <- p_bias + p_cov + p_sens + p_spec + plot_layout(guides = "collect", ncol = 2, nrow = 2) &
  theme(legend.position = "bottom")
p_patch
ggsave(here("analysis/manual/figures/model", "patchwork.png"),plot = p_patch, width = 10, height = 10, units = "cm", dpi = 300)


# Significance ------------------------------------------------------------

model_df %>%
  mutate(assortative = case_when(
    delta > 0 ~ "assortative",
    delta < 0 ~ "dissortative",
    TRUE ~ "neutral"
  ) ) %>%
  group_by(param, scenario, peak_coeff, name, assortative ) %>%
  summarise(
    est = binom.test(x = sum(significant_est), n = n())$est[[1]],
    lower = binom.test(x = sum(significant_est), n = n())$conf.int[[1]],
    upper = binom.test(x = sum(significant_est), n = n())$conf.int[[2]]) %>%
  ggplot(
    .,
    aes(
      x = name,
      y = est,
      color = as.factor(peak_coeff),
      shape = assortative
    )
  ) +
  facet_nested_wrap(
    vars(param, scenario),
    nrow = length(unique(model_df$param))
  ) +
  theme(strip.placement = "outside") +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = 0.9)
  ) +
  geom_point(position = position_dodge(width = 0.9)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  theme_bw()+
  labs(x = "", y = "Coverage - 95% Binomial Interval")+
  grid_theme
