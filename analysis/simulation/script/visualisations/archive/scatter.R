mean_line <- outcomes_long %>%
  group_by(peak_coeff, predictor_name, outcome_name) %>%
  mutate(bin = cut(predictor_value, breaks = 100)) %>%
  group_by(peak_coeff, predictor_name, outcome_name, bin) %>%
  summarise(mean_y = mean(outcome_value, na.rm = TRUE),
            midpoint_x = extract_and_calculate_midpoint(bin))



p_scatter <- outcomes_long %>%
  ggplot(aes(x = predictor_value,
             y = outcome_value)) +
  facet_grid(outcome_name ~
               as.factor(predictor_name),
             scales = "free") +
  geom_point(size = 0.3,
             alpha = 0.5,
             aes(colour = peak_coeff)) +
  geom_line(data = mean_line,
            aes(x = midpoint_x, y = mean_y,
                color = peak_coeff,
                group = peak_coeff))+
  geom_hline(data = hline_df,
             aes(yintercept = target),
             linetype = "solid") +
  scale_color_gradient2(midpoint = 1)+
  theme_bw() +
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
  )



p_scatter <- p_scatter +
  ggh4x::facetted_pos_scales(y = list(
    outcome_name == "bias" ~ scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)),
    outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.8, 0.95, 1)),
    outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
    outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
  ))

ggsave(
  here("analysis/simulation/plots", "scatter.png"),
  plot = p_scatter,
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
outcome_summary <- summary_df %>%
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

