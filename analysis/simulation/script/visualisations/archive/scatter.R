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

