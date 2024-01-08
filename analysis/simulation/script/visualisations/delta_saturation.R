
#below is for all peak coeffs & alpha levels
# p_delta_saturation <- summary_df %>%
#   filter(as.numeric(as.character(peak_coeff)) > 0.5) %>%
#   select(c("coverage", "group_susceptibles", "delta", "peak_coeff", "alpha")) %>%
#   filter(delta != 0) %>%
#   ggplot(aes(x = delta,
#              y = group_susceptibles,
#              group = interaction(alpha, peak_coeff))) +
#   facet_grid(alpha ~ peak_coeff) +
#   # geom_point(
#   #   aes(col = coverage),
#   #   shape = 16,
#   #   stroke = 0,
#   #   alpha = 0.5
#   # ) +
#   stat_summary_hex(aes(z = coverage),
#                    fun = function(x) mean(x)) +
#   geom_smooth(
#     se = TRUE,
#     method = "lm",
#     formula = y ~ x,
#     linewidth = 1,
#     alpha = 1,
#     col = "black"
#   ) +
#   scale_fill_viridis_c(option = "plasma") +
#   scale_y_continuous(n.breaks = 10) +
#   theme_publication()+
#   labs(x = "Delta", y = "Proportion of Susceptibles in the Group")



p_delta_saturation <- summary_df %>%
  select(c(
    "coverage",
    "group_susceptibles",
    "delta",
    "peak_coeff",
    "alpha"
  )) %>%
  filter(delta != 0 & alpha == 0.05) %>%
  ggplot(aes(x = delta,
             y = group_susceptibles,
             z = coverage)) +
  facet_wrap(vars(peak_coeff),
             nrow = 2
             #labeller = labeller(peak_coeff = ~paste0("PC: ", .x))
  ) +
  # stat_summary_hex(
  #   fun = function(x)
  #     mean(x),
  #   bins = 20
  # ) +
  geom_point(
    aes(col = coverage),
    alpha = 0.2
  ) +
  geom_smooth(
    se = TRUE,
    method = "lm",
    formula = y ~ x,
    alpha = 1,
    col = "black"
  ) +
  scale_fill_viridis_c("Coverage (95%)", option = "plasma",
                       breaks = c(0.25, 0.5, 0.75, 0.95),
                       labels = c("25%", "50%", "75%", "95%")) +
  scale_color_viridis_c("Coverage (95%)", option = "plasma",
                        breaks = c(0.25, 0.5, 0.75, 0.95),
                        labels = c("25%", "50%", "75%", "95%")) +
  theme_publication() +
  theme(legend.position = c(0.925, 0.22),
        legend.box.background = element_rect(colour = "black", linewidth = 1)) +
  labs(x = "Delta", y = "Proportion of Susceptibles") +
  guides(fill = guide_colorbar(title.position = "top",
                               direction = "vertical",
                               barheight = 10),
         color = guide_colorbar(title.position = "top",
                                direction = "vertical",
                                barheight = 10))

ggsave(
  here("analysis/simulation/plots", "delta_saturation.png"),
  plot = p_delta_saturation,
  width = 14,
  height = 8,
  units = "in",
  dpi = 300
)


rm(list = setdiff(ls(), ls_snapshot))
gc()

