

# delta_density_2d <- function(outcome, hline = NULL, violin_adjust = 1) {
#   data <- outcomes_df %>%
#     filter(peak_coeff == 1) %>%
#     dplyr::select(delta, !!sym(outcome)) %>%
#     mutate(
#       delta_type = case_when(
#         delta < 0 ~ "disassortative",
#         delta > 0 ~ "assortative",
#         delta == 0 ~ "neutral"
#       ),
#       metric =  outcome
#     )
#
#   tab <- data %>%
#     group_by(delta_type) %>%
#     summarise(
#       # lowerCI = t.test(!!sym(outcome), conf.level = 0.95)$conf.int[1],
#       # upperCI = t.test(!!sym(outcome), conf.level = 0.95)$conf.int[2],
#       # mean_outcome = mean(!!sym(outcome)),
#       # mean_delta = mean(delta),
#
#       median_outcome = median(!!sym(outcome), na.rm = TRUE),
#       lower_quantile = quantile(!!sym(outcome), 0.25, na.rm = TRUE),
#       upper_quantile = quantile(!!sym(outcome), 0.75, na.rm = TRUE),
#       median_delta = median(delta, na.rm = TRUE),
#       metric = outcome
#     )
#
#
#   p <-
#     ggplot(data) +
#     ggh4x::facet_grid2(metric ~ delta_type,
#                        scales = "free",
#                        independent = "y")
#
#
#   if (outcome != "specificity") {
#     p <- p +
#       stat_density_2d(
#         data = data %>%
#           filter(delta_type != "neutral"),
#         aes(
#           x = delta,
#           y = !!sym(outcome),
#           fill = ..level..,
#           group = interaction(delta_type, metric)
#         ),
#         contour = TRUE,
#         # bins = 10,
#         contour_var = "ndensity",
#         geom = "polygon",
#         colour = "white"
#       ) +
#       scale_fill_gradient(limits = c(0, 1),
#                           breaks = seq(0, 1, 0.25))
#   }
#
#
#
#   if (outcome != "sensitivity") {
#     p <- p +
#       geom_violin(
#         data = data %>%
#           filter(delta_type == "neutral"),
#         aes(x = delta,
#             y = !!sym(outcome)),
#         adjust = violin_adjust,
#         fill = "grey",
#         alpha = 0.3
#       )
#   }
#
#   if(!is.null(hline)){
#     p <- p +
#       geom_hline(data = tibble(hline = hline,
#                                delta_type = c("assortative",
#                                               "disassortative",
#                                               "neutral")),
#                  aes(yintercept = hline),
#                  color = "#3d3d3d")
#   }
#
#
#   p <- p +
#     geom_point(data = tab,
#                aes(x = median_delta,  y = median_outcome,
#                    color = "median")) +
#     geom_errorbar(
#       data = tab,
#       aes(
#         x = median_delta,
#         ymin = lower_quantile,
#         ymax = upper_quantile,
#         color = "median"
#       ),
#       width = 0.1
#     ) +
#     scale_color_manual(
#       name = "",
#       values = c("median" = "#ff7400"),
#       labels = c("median" = "median,\n50% quantile interval")
#     ) +
#     ggh4x::facetted_pos_scales(x = list(delta_type == "neutral" ~ scale_x_continuous(breaks = 0))) +
#     labs(x = "", y = "") +
#     theme_publication(base_size = 10) +
#     theme(plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
#           legend.position = "bottom")
#
#   return(p)
#
# }
#
#
# p_bias <-
#   delta_density_2d("bias", hline = rep(0, 3), violin_adjust = 1.5)
# p_cov <-
#   delta_density_2d("coverage",
#                    hline = rep(0.95, 3),
#                    violin_adjust = 1.5) +
#   theme(strip.text.x = element_blank())
# p_sens <-
#   delta_density_2d("sensitivity",
#                    hline = c(1, NA, NA),
#                    violin_adjust = 1.5) +
#   theme(strip.text.x = element_blank())
# p_spec <-
#   delta_density_2d("specificity", hline = c(NA, NA, 1), violin_adjust = 1.5) +
#   theme(strip.text.x = element_blank())+
#   scale_x_continuous(limits = c(0),
#                      breaks = 0)
#
#
# library(patchwork)
# p_delta_density2d <- wrap_plots(p_bias,
#            p_cov,
#            p_sens,
#            p_spec,
#            ncol = 1,
#            guides = "collect") &
#   theme(legend.position = "bottom")
# p_delta_density2d
#
# p_delta_density2d <-
#   gridExtra::grid.arrange(
#     patchwork::patchworkGrob(p_delta_density2d),
#     left = "value",
#     bottom = grid::grid.text(expression(delta))
#   )


# FACET VERSION -----------------------------------------------------------

data <- outcomes_df %>%
  filter(peak_coeff == 1) %>%
  dplyr::select(delta, all_of(metrics)) %>%
  mutate(
    delta_type = case_when(
      delta < 0 ~ "disassortative",
      delta > 0 ~ "assortative",
      delta == 0 ~ "neutral"
    )
  ) %>%
  pivot_longer(
    cols = -c(delta, delta_type),
    names_to = "outcome",
    values_to = "value"
  )


tab <- data %>%
  group_by(delta_type, outcome) %>%
  summarise(
    median_outcome = median(value, na.rm = TRUE),
    lower_quantile = quantile(value, 0.25, na.rm = TRUE),
    upper_quantile = quantile(value, 0.75, na.rm = TRUE),
    median_delta = median(delta, na.rm = TRUE),
  )
tab



hline_df <- data.frame(
  outcome_name = c("sensitivity", "specificity", "bias", "coverage"),
  target = c(1, 1, 0, 0.95)
)

my_hlines <- merge(tibble(
  outcome = c("sensitivity", "specificity", "bias", "coverage"),
  target = c(1, 1, 0, 0.95)
),
tibble(delta_type = c(
  "assortative", "disassortative", "neutral"
))) %>%
  mutate(
    target = case_when(
      outcome == "sensitivity" & delta_type != "assortative" ~ NA,
      outcome == "specificity" & delta_type != "neutral" ~ NA,
      .default = target
    )
  )

p_delta_density2d <- ggplot(data) +
  ggh4x::facet_grid2(outcome ~ factor(
    delta_type,
    levels = c("disassortative",
               "assortative",
               "neutral")
  ),
  scales = "free",
  independent = "all") +
  stat_density_2d(
    data = data %>% filter(delta_type != "neutral"),
    aes(
      x = delta,
      y = value,
      fill = ..level..,
      group = interaction(delta_type, outcome)
    ),
    contour = TRUE,
    # bins = 10,
    contour_var = "ndensity",
    geom = "polygon",
    colour = "white"
  ) +
  # geom_point(
  #   data = data %>% filter(delta_type != "neutral"),
  #   aes(x = delta, y = value),
  #   col = "red",
  #   alpha = 0.1,
  #   size = 0.2
  # ) +
  geom_violin(
    data = data %>% filter(delta_type == "neutral"),
    aes(x = delta, y = value),
    adjust = 1.5,
    fill = "grey",
    alpha = 0.3
  ) +
  geom_hline(data = my_hlines,
             aes(yintercept = target),
             color = "#E32227") +
  geom_point(data = tab,
             aes(x = median_delta,  y = median_outcome,
                 color = "median")) +
  geom_errorbar(
    data = tab,
    aes(
      x = median_delta,
      ymin = lower_quantile,
      ymax = upper_quantile,
      color = "median"
    ),
    width = 0.1
  ) +
  scale_color_manual(
    name = "",
    values = c("median" = "black"),
    labels = c("median" = "median,\n50% quantile interval")
  ) +
  theme_publication(base_size = 10) +
  labs(x = as.expression(bquote(bold(delta))),
       y = "Outcome") +
  theme(axis.title.x = element_text(face = "bold")) +
  scale_fill_viridis_c(option = "plasma")

p_delta_density2d


ggsave(
  here("analysis/simulation/plots", "delta_density2d.png"),
  plot = p_delta_density2d,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

outcomes_df %>%
  mutate(
    delta_type = case_when(
      delta < 0 ~ "disassortative",
      delta > 0 ~ "assortative",
      delta == 0 ~ "neutral"
    )
    ) %>%
  select(delta_type, trials) %>%
  group_by(delta_type) %>%
  summarise(mean = mean(trials),
            sd = sd(trials),
            se = sd(trials)/sqrt(n()),
            median = median(trials),
            min = min(trials),
            max = max(trials),
            n = n())


