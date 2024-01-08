# my estimator function
estimator <-  Vectorize(function(x, n, f, alpha = 0.05) {
  binom_est <- binom.test(x, n, conf.level = 1 - alpha)$estimate[[1]]
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
}, vectorize.args = c("x", "n", "f"))

grid <- expand.grid(
  x = seq(0, 100, by = 1),
  n = seq(0, 100, by = 1),
  f = seq(0.01, 1-0.01, by = 0.01)
)
grid <- grid[grid$n >= grid$x & grid$n > 0,]
grid$est <- estimator(grid$x, grid$n, grid$f)


# GGplot --------------------------------------------------------------------
p_estimator_heatmap <-
  ggplot(grid,
       aes(
         x = f,
         y = x / n,
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
    x = "Relative Group Size (***f<sub>a<sub>***)",
    y = "Proportion of Within-Group Transmissions (***\u03B2<sub>a\u2190a</sub>***)",
    fill = "Inferred &delta;"
  ) +
  theme_publication()+
  theme(
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown(),
    legend.title = ggtext::element_markdown(),
  ) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,
                                barwidth = 10))+
  coord_fixed(ratio = 1)

ggsave(
  here::here("analysis/simulation/plots",
             "estimator_heatmap.png"),
  p_estimator_heatmap,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

ggh4x::facet_wrap2()
design =  matrix(c(1,2,3,4,5,6,7,8,9), 3, 3, byrow = TRUE)

count <- summary_df %>%
  filter(alpha == 0.05 & peak_coeff == 1) %>%
  mutate(size_freq_chop =  santoku::chop(size_freq, breaks = seq(0,1, 0.1), close_end = FALSE, drop = TRUE)) %>%
  select(size_freq_chop, delta, sensitivity) %>%
  group_by(size_freq_chop) %>%
  summarise(n = n())

p_estimator_sentivity_f <- summary_df %>%
  filter(alpha == 0.05 & peak_coeff == 1) %>%
  mutate(size_freq_chop =  santoku::chop(size_freq, breaks = seq(0,1, 0.1), close_end = FALSE, drop = TRUE)) %>%
  select(size_freq_chop, delta, sensitivity) %>%
  ggplot(aes(x = delta, y = sensitivity))+
  #facet_wrap(~size_freq_chop, scales = "free")+
  ggh4x::facet_manual(~size_freq_chop, design = design)+
  geom_point()+
  geom_smooth()+
  #number of observations as label
  geom_label(data = count, aes(label = paste0(
    "n = ", n
  )), x = 0, y = 0.7, size = 5)+
  scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(0,1))+
  theme_publication()+
  theme(
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown(),
    legend.title = ggtext::element_markdown(),
  )+
  labs(
    x = "Inferred &delta;",
    y = "Sensitivity"
  )

ggsave(
  here::here("analysis/simulation/plots",
             "estimator_sensitivity_f.png"),
  p_estimator_sentivity_f,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

# Plot the results
# p_delta_investigation <-
#   ggplot(grid,
#          aes(
#            x = n,
#            y = x,
#            fill = est,
#            colour = est
#          )) +
#   facet_wrap(vars(f),
#              labeller = labeller(
#                f = function(x)
#                  paste("*f* = ", x)
#              )) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "blue",
#     mid = "white",
#     high = "red",
#     midpoint = 0
#   ) +
#   scale_colour_gradient2(
#     low = "blue",
#     mid = "white",
#     high = "red",
#     midpoint = 0
#   ) +
#   labs(
#     fill = "Inferred Delta",
#     colour = "Inferred Delta",
#     x = "No. of Transmissions Emitted (***n***)",
#     y = "No. of Within-Group Transmissions (***x***)"
#   ) +
#   theme_publication() +
#   theme(
#     strip.text = ggtext::element_markdown(),
#     axis.title.x = ggtext::element_markdown(),
#     axis.title.y = ggtext::element_markdown()
#   ) +
#   guides(
#     colour = guide_colorbar(
#       title.position = "top",
#       title.hjust = 0.5,
#       label.position = "bottom",
#       barwidth  = 10,
#       nrow = 1,
#       byrow = TRUE
#     ),
#     fill = guide_colorbar(
#       title.position = "top",
#       title.hjust = 0.5,
#       label.position = "bottom",
#       barwidth = 10,
#       nrow = 1,
#       byrow = TRUE
#     )
#   )
# p_delta_investigation


# Plotly ------------------------------------------------------------------
#3d scatter plot
# plotly::plot_ly(
#   data = grid,
#   x = ~ n,
#   y = ~ x,
#   z = ~ f,
#   color = ~ est,
#   type = 'scatter3d',
#   mode = 'markers',
#   symbol = "square",
#   #not working
#   colors = c("blue", "white", "red")
# )
#
#
# htmlwidgets::saveWidget(
#   as_widget(p_plotly),
#   here::here(
#     "analysis/simulation/plots",
#     "plotly_delta_investigation.html"
#   ),
# )

rm(list = setdiff(ls(), ls_snapshot))
gc()
