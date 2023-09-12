library(tidyverse)
library(o2groups)
scenarios <- readRDS("analysis/data/scenarios.rds") %>% extract_scenario()
simulations <- readRDS("analysis/data/simulations.rds")
results_raw <- readRDS("analysis/data/results.rds")
#https://rpubs.com/rana2hin/raincloud
# Functions -------------------------------------------------------------------------------------
standardise <- function(x) {
  x <- as.numeric(x)
  d <- ifelse(is.finite(x),
    (x - 1) / (x + 1),
    1.0
  )
  return(d)
}

reverse_standardise <- function(d) {
  x <- (1 + d) / (1 - d)
  return(x)
}

normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Define the polar theme
polar_theme <- theme(
  text = element_text(size = 12),
  axis.text.x = element_text(color = "black"),
  axis.line.y = element_blank(),
  panel.grid.major.x = element_line(color = "gray", size = 0.1),
  panel.grid.major.y = element_line(color = "black", size = 0.1, linetype = "solid"),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  legend.background = element_rect(fill = "white", color = "black"),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)
gg_y_scale <- list(scale_y_continuous(breaks = seq(-1, 1, 0.1), limits = c(-1, 1), expand = c(0.01, 0.01)))


#plot standardised delta
tibble(
  raw_delta = seq(0, 100, 0.1),
  scaled_delta = standardise(raw_delta)
) %>%
  ggplot(aes(x = raw_delta, y = scaled_delta)) +
  #colour the assortative and dissortative space
  geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 1), fill = "#00ffdd", alpha = 0.01)+
  geom_rect(aes(xmin = 0, xmax = 100, ymin = -1, ymax = 0), fill = "#ff00f7", alpha = 0.01)+
  geom_line() +
  labs(x = "Raw delta", y = "Scaled delta") +
  theme_bw()+
  geom_vline(xintercept = 1, lty = "dotted") +
  geom_hline(yintercept = 0, lty = "dotted")+
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.1), limits = c(-1, 1), expand = c(0.01, 0.01))

reverse_standardise(0.9) - reverse_standardise(0.8)
reverse_standardise(-0.9) - reverse_standardise(-0.8)
reverse_standardise(0.3) - reverse_standardise(0.2)
reverse_standardise(-0.3) - reverse_standardise(-0.2)


# Data ------------------------------------------------------------------------------------------
# From now all values will be standardised
results <-
merge(results_raw,scenarios, by = c("scenario", "group"), all.x = TRUE) %>%
mutate(
    simulation = as.factor(simulation),
    param = factor(param, levels = unique(scenarios$param)),
    scenario = factor(scenario, levels = unique(scenarios$scenario)),
    peak_coeff = as.factor(peak_coeff),
    group = as.factor(group),
    across(
      .cols = c("delta", "est", "lower_ci", "upper_ci"),
      .names = "{.col}",
      .fns = standardise
    ),
    bias = delta - est,
    is_within_ci = ifelse(delta >= lower_ci &
      delta <= upper_ci,
    TRUE,
    FALSE
    ),
    delta_outside_0 = ifelse(delta != 0,
      TRUE,
      FALSE
    ),
    est_outisde_0 = ifelse(lower_ci > 0 | upper_ci < 0,
      TRUE,
      FALSE
    ),
  .keep = "none") %>%
  arrange(simulation, scenario, peak_coeff, group)
head(results)

# Metrics ---------------------------------------------------------------------------------------

# Estimate ---------------------------------------------------------------------------------------
# Uncertainty around the estimate:
# mean estimate and mean lower and mean upper CI

m1 <- results %>%
  drop_na(est) %>%
  group_by(peak_coeff, param, scenario, group) %>%
  summarise(across(
    .cols = c("delta", "est", "lower_ci", "upper_ci"),
    .fns = ~ mean(.)
   # .names = "mean_{.col}"
  )) %>%
  rename(lower = lower_ci,
         upper = upper_ci)

source("analysis/script/plot_scenarios.R")
plot_scenarios(data = m1, type = "est")

m1 %>%
  ggplot(aes(x = group, y = est, col = peak_coeff)) +
  # facet_grid(rows = vars(param),
  #            cols = vars(scenario),
  #            scales = "free_x",
  #            space = "free") +
  ggh4x::facet_nested_wrap(
    vars(param, scenario),
    scales = "free_x",
    strip = ggh4x::strip_nested(bleed = TRUE,
                         text_x = ggh4x::elem_list_text(colour = c("white", "black"),
                                                 face = c("bold","italic")),
                         background_x = ggh4x::elem_list_rect(fill = c("#4C4E52", "white")),
                         by_layer_x = TRUE),
    labeller = as_labeller(wrap_label),
    drop = TRUE
  ) +

  # estimate
  geom_point(position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2,
    position = position_dodge(0.9)
  ) +

  # true delta
  geom_segment(
    aes(
      y = delta,
      xend = as.integer(as.factor(group)) + 0.5,
      yend = delta
    ),
    color = "black",
    position = position_dodge(0.9),
    alpha = 0.7,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(
      y = delta,
      xend = as.integer(as.factor(group)) - 0.5,
      yend = delta
    ),
    color = "black",
    position = position_dodge(0.9),
    alpha = 0.7,
    show.legend = FALSE
  ) +

  # hline at 0
  geom_hline(aes(yintercept = 0), lty = "dotted") +
  labs(
    x = "Scenario", y = "Estimate",
    title = "Mean estimate, mean 95% lower and mean 95% upper CI",
    subtitle = "black lines refer to the true delta estimate"
  ) +
  theme_bw()


# central tendency of the estimates:
# mean estimate and 95% quantile interval of the estimate
m2 <- results %>%
  drop_na(est) %>%
  group_by(peak_coeff, param, scenario, group) %>%
  summarise(
    delta = unique(delta),
    mean_est = mean(est),
    lower = quantile(est, 0.025),
    upper = quantile(est, 0.975)
  ) %>%
  rename(est = mean_est)
plot_scenarios(m2, "est")

m2 %>%
  ggplot(aes(x = group, y = est, col = peak_coeff)) +
  facet_wrap(~scenario, scales = "free_x") +

  # estimate
  geom_point(position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2,
    position = position_dodge(0.9)
  ) +

  # true delta
  geom_segment(
    aes(
      y = delta,
      xend = as.integer(as.factor(group)) + 0.5,
      yend = delta
    ),
    color = "black",
    position = position_dodge(0.9),
    alpha = 0.7,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(
      y = delta,
      xend = as.integer(as.factor(group)) - 0.5,
      yend = delta
    ),
    color = "black",
    position = position_dodge(0.9),
    alpha = 0.7,
    show.legend = FALSE
  ) +

  # hline at 0
  geom_hline(aes(yintercept = 0), lty = "dotted") +
  labs(
    x = "Scenario", y = "Estimate",
    title = "Mean estimate & 95% quantile interval of the estimate",
    subtitle = "black lines refer to the true delta estimate"
  ) +
  theme_bw()+
  gg_y_scale



# Bias ----------------------------------------------------------------------------------------
# Bias, defined as difference between the mean delta estimate and the true delta value.
# Bias should be as small as possible and will be zero for a perfect model.

# Find the peak_coeff that minimises bias
m4 <- results %>%
  group_by(scenario, param, peak_coeff, group) %>%
  summarise(
    mean_bias = mean(bias),
    sd_bias = sd(bias)
  ) %>%
  mutate(lower = mean_bias - sd_bias,
         upper = mean_bias + sd_bias) %>%
  rename(bias = mean_bias)

plot_scenarios(m4, "bias")

m4 %>%
  ggplot() +
  aes(
    x = group,
    y = mean_bias,
    color = peak_coeff
  ) +
  facet_wrap(~scenario, scales = "free_x") +
  geom_errorbar(
    aes(
      ymin = bias_lower,
      ymax = bias_upper,
    ),
    position = position_dodge(0.9),
  ) +
  geom_point(
    position = position_dodge(0.9)
  ) +
  geom_hline(yintercept = 0, lty = "solid", color = "#3d3c3c") +
  theme_bw() +
  labs(
    title = "Histogram of Bias by Scenario",
    subtitle = "Point represent the mean bias. Error bars represent the one standard deviation interval.",
    x = "Scaled Bias",
    y = "Frequency",
    caption = "Bias is the difference between the delta estimate and the true delta value. Bias should be closest to 0 for a perfect model."
  )

# violin plot of bias distribution by scenario
results %>%
  ggplot(
    aes(
      x = group,
      y = bias,
      group = interaction(scenario, group, peak_coeff)
    )
  ) +
  # facet_wrap(~scenario, scales = "free") +
  ggh4x::facet_nested_wrap(
    vars(param, scenario),
    scales = "free_x",
    strip = ggh4x::strip_nested(
      bleed = TRUE,
      text_x = ggh4x::elem_list_text(
        colour = c("white", "black"),
        face = c("bold", "italic")
      ),
      background_x = ggh4x::elem_list_rect(fill = c("#4C4E52", "white")),
      by_layer_x = TRUE
    ),
    labeller = as_labeller(wrap_label),
    drop = TRUE
  ) +
  geom_violin() +
  ggforce::geom_sina(aes(col = peak_coeff), alpha = 0.4) +
  geom_hline(yintercept = 0, lty = "dotted") +
  theme_bw() +
  labs(
    title = "Distribution of Bias by Scenario",
    x = "Scaled Bias",
    y = "Frequency",
    caption = "Bias is the difference between the delta estimate and the true delta value. Bias should be closest to 0 for a perfect model."
  )
#peak bias at 0.1818 because est is -1 since there were 0 successes.

# coverage ----------------------------------------------------------------
# Coverage probability, defined as the proportion of simulations where a given confidence interval of the estimated delta contained the true delta value.
# The 95% coverage probability for a well-calibrated model should be 95%, i.e.
# The true value will be contained in the 95% CI in 95% of the simulations (analogous criterion is applicable for 50% coverage probability) ;
# --> How many times does the true delta falls within the 95% CI?

m5 <- results %>%
  group_by(peak_coeff, param, scenario, group) %>%
  summarise(
    est = binom.test(x = sum(is_within_ci), n = n())$est[[1]],
    lower = binom.test(x = sum(is_within_ci), n = n())$conf.int[[1]],
    upper = binom.test(x = sum(is_within_ci), n = n())$conf.int[[2]]
  )
plot_scenarios(m5, "95coverage")
m5 %>%
  ggplot(aes(
    x = group, y = est,
    color = peak_coeff
  )) +
  facet_wrap(~scenario, scales = "free") +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  geom_hline(yintercept = 0.95) +
  labs(x = "Group", y = "Proportion within CI",
  title = "Coverage Rate by Scenario", subtitle = "Point refers to frequency estimate. Error bar refers to 95% binomial CI.",
  caption = "Coverage probability is defined as the proportion of simulations where a given confidence interval of the delta estimate contained the true delta value.") +
  theme_bw()


# Singificance --------------------------------------------------------------------------------
# Significance is defined as the proportion of simulations
# where the delta estimate detects significant assortativity or dissortativity (i.e. the 95% CI does not contain 0).

m6 <- results %>%
  group_by(peak_coeff, scenario, group) %>%
  summarise(
    est = binom.test(x = sum(est_outisde_0), n = n())$est[[1]],
    lower = binom.test(x = sum(est_outisde_0), n = n())$conf.int[[1]],
    upper = binom.test(x = sum(est_outisde_0), n = n())$conf.int[[2]]) %>%
    left_join(scenarios, by = c("scenario", "group")) %>%
    select(-c(size, r, intro_n)) %>%
  mutate(
    scaled_delta = standardise(delta),
    shape = case_when(
    scaled_delta > 0 ~ "Assortative",
    scaled_delta < 0 ~ "Dissortative",
    TRUE ~ "Neutral"),
    scenario = factor(scenario, levels = unique(scenarios$scenario))
    )

plot_scenarios(m6, "significance")

m6 %>%
  ggplot(aes(
    x = group, y = est,
    color = peak_coeff,
    shape = shape,
    group = interaction(scenario, group, peak_coeff)
  )) +
  facet_wrap(~scenario, scales = "free_x") +
  geom_point(
    size = 2,
    position = position_dodge(width = 0.6)
  ) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = 0.6),
    width = 0.2
  ) +
  labs(x = "Group", y = "Proportion",
       title = "Proportion of simulations where significance is detected",
       subtitle = "Point refers to the mean proportion. Error bar refers to 95% binomial CI.",
       caption = "Significance is defined as the proportion of simulations where the delta estimate
  detects significant assortativity or dissortativity (i.e. the 95% CI does not contain 0).") +
  theme_bw()



# output -----------------------------------------------------------------------------------------
# Calculating sensitivity and specificity
# Positive here refers to the model detecting significant assortativity or dissortativity (i.e. the 95% CI does not contain 0).
out <- results %>%
  select(peak_coeff, delta_outside_0, est_outisde_0) %>%
  group_by(peak_coeff) %>%
  summarise(
    true_positive = sum(delta_outside_0 == TRUE &
      est_outisde_0 == TRUE),
    true_negative = sum(delta_outside_0 == FALSE &
      est_outisde_0 == FALSE),
    false_positive = sum(delta_outside_0 == FALSE &
      est_outisde_0 == TRUE),
    TPR = true_positive / sum(delta_outside_0 == TRUE),
    FPR = false_positive / sum(delta_outside_0 == TRUE),
    TNR = true_negative / sum(delta_outside_0 == FALSE),
    PPV = true_positive / sum(est_outisde_0 == TRUE),
    NPV = true_negative / sum(est_outisde_0 == FALSE)
  ) %>%
  select(-starts_with(c("true_", "false_")))

#RoC curve
out %>%
  ggplot(aes(x = FPR, y = TPR, col = peak_coeff)) +
  geom_point(size = 4) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian()+
  theme_bw()

# Normalise the values
out_normalised <- as.data.frame(lapply(out[, -1], normalise))
out_normalised$peak_coeff <- out$peak_coeff

out %>% #out_normalised
  pivot_longer(cols = -peak_coeff, names_to = "metric") %>%
  ggplot(aes(x = metric, y = value, group = peak_coeff)) +
  geom_line(aes(color = peak_coeff, )) +
  geom_point(aes(color = peak_coeff), show.legend = FALSE) +
  geom_vline(
    xintercept = c(1:4),
    colour = "gray",
    lty = "solid"
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) +
  geom_text(
    aes(
      label = scales::percent(value, accuracy = 1),
      color = peak_coeff
    ),
    position = position_nudge(y = 0.03),
    size = 3.5
  ) +
  theme_void() +
  coord_polar(clip = "off") +
  polar_theme +
  theme(legend.position = "bottom")
