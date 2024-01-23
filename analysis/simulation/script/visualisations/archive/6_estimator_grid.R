library(tidyverse)
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))

# Data --------------------------------------------------------------------

# Delta estimator function
estimator <- Vectorize(function(x_n, f) {
  binom_est <- x_n
  numerator <- binom_est * (1 - f)
  denominator <- f * (1 - binom_est)
  gamma <- numerator / denominator
  delta <-
    ifelse(is.infinite(gamma), 1, ifelse(is.na(gamma),
                                         NA,
                                         ifelse(
                                           is.numeric(gamma) & is.finite(gamma),
                                           (gamma - 1) /
                                             (gamma + 1)
                                         )))
  return(delta)
}, vectorize.args = c("x_n", "f"))

grid <- expand.grid(
  x_n = seq(0, 1, by = 0.01),
  f = seq(0, 1, by = 0.01),
  delta = seq(-1, 1, 0.25)
) %>%
  mutate(est = estimator(x_n, f),
         bias = delta - est)


# Plot --------------------------------------------------------------------

#heatmap
appender <- function(string){
  latex2exp::TeX(paste("$\\delta_{\\a} = $", string))

}

p_estimator_grid <- ggplot(grid,
       aes(x = f, y = x_n, fill = bias, color = bias)) +
  facet_wrap( ~ delta,
              labeller = as_labeller(appender,
                                     default = label_parsed)
              )+
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#008080", "white","#EEBC54"),
    #colours = c("#008000", "white", "#800080"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  ) +
  scale_colour_gradientn(
    colours = c("#008080", "white","#EEBC54"),
    #colours = c("#008000", "white", "#800080"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  ) +
  # geom_abline(
  #   slope = 1,
  #   intercept = 0,
  #   colour = "black",
  #   size = 0.5
  # ) +
  theme_publication()+
  theme(
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown(),
    legend.title = ggtext::element_markdown(),
    legend.position = "bottom",
    axis.title = element_text(size = 20)
  )+
  labs(
    x = "*f<sub>a<sub>*",
    y = "*\u03C0<sub>a\u2190a</sub>*",
    fill = "Bias",
    colour = "Bias"
  )

ggsave(
  here::here("analysis/simulation/plots",
             "estimator_grid.png"),
  p_estimator_grid,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)





# Bias Diagram ------------------------------------------------------------

expand.grid(truth = seq(-1, 1, 0.01), est = seq(-1, 1, 0.01)) %>%
  mutate(bias = truth - est) %>%
  ggplot(aes(x = truth, y = est, fill = bias, colour = bias)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#008080", "white","#EEBC54"),
                       values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
                       guide = "colorbar") +
  scale_colour_gradientn(
    colours = c("#008080", "white","#EEBC54"),
    #colours = c("#008000", "white", "#800080"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  ) +
  labs(x = expression(delta),
       y = expression(bar(delta)),
       fill = expression(paste("Bias (", delta, "-", bar(delta), ")")),
       colour = expression(paste("Bias (", delta, "-", bar(delta), ")"))) +
  coord_fixed() +
  theme_publication() +
  guides(
    fill = guide_colorbar(
      barwidth = 10,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(size = 10, face = "bold"),
      ticks = FALSE,
      label.theme = element_text(size = 10)
    ),
    colour = guide_colorbar(
      barwidth = 10,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(size = 10, face = "bold"),
      ticks = FALSE,
      label.theme = element_text(size = 10)
    )
  ) +
  theme(legend.key = element_rect(color = "black", fill = 'white'))

ggsave(
  here("analysis/simulation/plots", "bias_grid.png"),
  width = 5,
  height = 5,
  dpi = 300
)




# Inferred Delta heatmap --------------------------------------------------
ggplot(grid,
         aes(
           x = f,
           y = x_n,
           fill = est
         )) +
  geom_tile(width = 0.01,
            height = 0.01,
            color = NA,
            lwd = 0) +
  # scale_fill_gradientn(
  #   colours = c("navyblue", "white", "orange"),
  # ) +
  scico::scale_fill_scico(palette = 'vik') +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.25),
    expand = c(0, 0.01)) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    expand = c(0, 0.01)) +
  labs(
    x = latex2exp::TeX(r"($f_{a}$)", bold = TRUE, italic = TRUE),
    y = latex2exp::TeX(r"($\pi_{a \leftarrow a}$)", bold = TRUE, italic = TRUE),
    fill = latex2exp::TeX(r"($\bar{\delta}_{a}$)", bold = TRUE, italic = TRUE)
  ) +
  theme_publication()+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,
                                barwidth = 10),
         legend.title = element_text(face = "bold"))+
  coord_fixed(ratio = 1)

ggsave(
  here("analysis/simulation/plots", "est_grid.png"),
  width = 5,
  height = 5,
  dpi = 300
)










# Sensitivity -------------------------------------------------------------
set.seed(123)
sampled_data <- summary_df %>%
  filter(alpha == 0.05 & peak_coeff == 1) %>%
  filter(n_groups == 2) %>%
  mutate(
    size_freq_cat = case_when(
      size_freq <= 0.3 ~ "below 0.3",
      size_freq >= 0.7 ~ "above 0.7",
      #size_freq >= 0.45 & size_freq <= 0.55 ~ "between 0.45 and 0.55",
      TRUE ~ NA
    )
  ) %>%
  drop_na(size_freq_cat) %>%
  group_by(size_freq_cat) %>%
  nest() %>%
  ungroup() %>%
  mutate(min_obs = min(map_int(data, nrow))) %>%
  mutate(
    data = map2(
      data,
      min_obs,
      ~ .x %>% sample_n(.y)
    )
  ) %>%
  unnest(data)


p_estimator_sentivity_f <-
  sampled_data %>%
  ggplot(aes(x = delta, y = sensitivity, color = size_freq_cat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  theme_publication() +
  scale_color_manual(values = c("purple", "orange", "#2a2727"),
                     labels = c(
                       latex2exp::TeX(r"($\geq 0.7$)"),
                       latex2exp::TeX(r"($\leq 0.3$)"),
                       latex2exp::TeX(r"($0.45 \leq $f$ \leq 0.55$)")
                     )) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = expression(delta),
       y = "Sensitivity",
       color = latex2exp::TeX(r"($f$)", bold = TRUE, italic = TRUE))

p_estimator_sentivity_f


sampled_data %>%
  select(
    scenario,
    name,
    delta,
    size,
    size_freq,
    size_freq_cat,
    trials,
    sd_peak_date,
    sensitivity
  ) %>%
  mutate(delta_cat = case_when(
    delta <= -0.5 ~ "strong_disassortative",
    delta >= 0.5 ~ "strong_assortative")
  ) %>%
  drop_na(delta_cat) %>%
  group_by(delta_cat, size_freq_cat) %>%
  summarise(
    mean_sensitivity = mean(sensitivity),
    mean_trials = mean(trials),
    mean_sd_peak_date = mean(sd_peak_date))


calculate_mean_ci <- function(x) {
  # Calculate mean
  mean_value <- mean(x)

  # Calculate standard deviation (for a sample)
  std_dev <- sd(x)

  # Number of observations
  n <- length(x)

  # Calculate critical value for a 95% confidence interval
  critical_value <- qnorm(0.975)  # 0.975 because it's a two-tailed test

  # Calculate margin of error
  margin_of_error <- critical_value * (std_dev / sqrt(n))

  # Calculate 95% confidence interval
  confidence_interval <- c(mean_value - margin_of_error, mean_value + margin_of_error)

  # Return a list with the results
  return(list(mean = mean_value, ci = confidence_interval))
}


sampled_data %>%
  mutate(delta_cat = case_when(delta <= -0.5 ~ "-0.5",
                               delta >= 0.5 ~ "0.5",
                               TRUE ~ NA_character_)) %>%
  drop_na(delta_cat, size_freq_cat) %>%
  select(delta, delta_cat, sensitivity, size_freq_cat) %>%
  ggplot(aes(x = delta_cat, y = sensitivity)) +
  facet_grid( ~ size_freq_cat,
              labeller =  labeller(size_freq_cat = c(
                "below 0.3" = "Small",
                "above 0.7" = "Large",
                "between 0.45 and 0.55" = "Medium")
              )) +
  geom_violin() +
  # stat_summary(
  #   fun.y = function(x) calculate_mean_ci(x)$mean,
  #   fun.ymin = function(x) calculate_mean_ci(x)$ci[1],
  #   fun.ymax = function(x) calculate_mean_ci(x)$ci[2],
  #   geom = "pointrange",
  # ) +
  #stat summary with 95% CI
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    aes(color = size_freq_cat),
    width = 0.2
  ) +
  stat_summary(fun.y = mean,
               geom = "point",
               aes(color = size_freq_cat)) +
  stat_summary(fun.y = mean,
               geom = "line",
               aes(group = size_freq_cat)) +
  scale_color_manual(
    values = c("purple", "orange", "#2a2727"),
    labels = c(
      latex2exp::TeX(r"($\geq 0.7$)"),
      latex2exp::TeX(r"($\leq 0.3$)"),
      latex2exp::TeX(r"($0.45 \leq $f$ \leq 0.55$)")
    )
  ) +
  scale_x_discrete(
    labels = c(
      latex2exp::TeX(r"($\delta \leq -0.5$)"),
      latex2exp::TeX(r"($\delta \geq 0.5$)")
    )
  ) +
  theme_publication() +
  labs(
    x = expression(delta),
    y = "Sensitivity",
    color = latex2exp::TeX(r"($f$)", bold = TRUE, italic = TRUE)
  )


set.seed(123)
sampled_data <- summary_df %>%
  filter(alpha == 0.05 & peak_coeff == 1) %>%
  filter(n_groups == 2 & abs(delta) >= 0.7) %>%
  mutate(
    size_freq_cat = case_when(
      size_freq <= 0.3 ~ "below 0.3",
      size_freq >= 0.7 ~ "above 0.7",
      size_freq >= 0.45 & size_freq <= 0.55 ~ "between 0.45 and 0.55",
      TRUE ~ NA
    ),
    delta_cat = case_when(
      delta <= -0.75 ~ "below -0.75",
      delta >= 0.75 ~ "above 0.75",
      TRUE ~ NA
    )
  ) %>%
  drop_na(size_freq_cat, delta_cat) %>%
  group_by(size_freq_cat, delta_cat) %>%
  nest() %>%
  ungroup() %>%
  mutate(min_obs = min(map_int(data, nrow))) %>%
  mutate(
    data = map2(
      data,
      min_obs,
      ~ .x %>% sample_n(.y)
    )
  ) %>%
  unnest(data)


sampled_data %>%
  ggplot(aes(x = delta, y = bias, color = size_freq_cat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE,
              method = "lm",
              formula = y ~ x,
              span = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_publication() +
  scale_color_manual(
    values = c("purple", "orange", "#2a2727"),
    labels = c(
      latex2exp::TeX(r"($\geq 0.7$)"),
      latex2exp::TeX(r"($\leq 0.3$)"),
      latex2exp::TeX(r"($0.45 \leq $f$ \leq 0.55$)")
    )
  ) +
  scale_x_continuous(limits = c(-1, 1),
                     breaks = seq(-1,1,0.5)) +
  labs(
    x = expression(delta),
    y = "Bias",
    color = latex2exp::TeX(r"($f$)", bold = TRUE, italic = TRUE)
  )

p_estimator_sentivity_f










ggsave(
  here::here("analysis/simulation/plots",
             "estimator_sensitivity_f.png"),
  p_estimator_sentivity_f,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)



rm(list = setdiff(ls(), ls_snapshot))
gc()
