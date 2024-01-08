library(tidyverse)
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
appender <- function(string)
  latex2exp::TeX(paste("$\\delta_{\\a} = $", string))

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
  labs(x = expression(paste("True Delta (", delta, ")")),
       y = expression(paste("Estimated Delta (", bar(delta), ")")),
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
