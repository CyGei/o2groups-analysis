p_gamma_delta <- tibble(delta = seq(-1, 0.965, 0.001),
                        gamma = o2groups::reverse_scale(delta)) %>%
  mutate(effect = case_when(
    delta < 0 ~ "disassortative",
    delta > 0 ~ "assortative",
    delta == 0 ~ "neutral"
  )) %>%
  ggplot(aes(x = gamma, y = delta)) +
  geom_path(aes(color = effect),
            linewidth = 1,
            key_glyph = draw_key_rect) +
  geom_point(data = tibble(
    delta = 0,
    gamma = 1
  ),
  fill = "darkgrey",
  color = "black",
  shape = 21,
  size = 5) +
  geom_segment(aes(
    x = 0,
    y = -0.99,
    xend = 0,
    yend = -1
  ),
  color = "#4A98C5"
  #arrow = arrow(length = unit(0.4, "cm"))
  ) +
  geom_segment(aes(
    x = 48,
    y = 0.96,
    xend = 49,
    yend = 0.96,
  ),
  color = "#c5774a",
  arrow = arrow(length = unit(0.4, "cm"))) +
  scale_color_manual(
    "Pattern",
    breaks=c("disassortative", "neutral", "assortative"),
    values = c(
    "disassortative" = "#4A98C5",
    "assortative" = "#c5774a",
    "neutral" = "darkgrey"
  )) +
  scale_x_continuous(
    limits = c(0, 50),
    breaks = c(0, 1, seq(10, 50, 10)),
    labels = c(0, 1, seq(10, 40, 10), expression(paste("+",infinity))),
    expand = c(0.01, 0.1)
  ) +
  theme_publication() +
  coord_cartesian() +
  labs(x = expression(gamma),
       y = expression(delta)) +
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.4, "cm"),
                                                       ends = "last")))+
  guides(
    colour = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      label.theme = element_text(color = "white",
                                 margin = margin(t = -17.5),
                                 face = "bold"),
      keywidth = 10,
      nrow = 1,
      byrow = TRUE
    )
  )

ggsave(
  here("analysis/simulation/plots", "gamma_delta.png"),
  p_gamma_delta,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)


# Peak Window -------------------------------------------------------------

alpha <- 1
set.seed(123)
p_peak_coeff <- data.frame(values = rnorm(20000, mean = 10, sd = 2.9)) %>%
  ggplot() +
  geom_histogram(aes(x = values, fill = after_stat(x)),
                 binwidth = 1,
                 alpha = alpha,
                 color = "white") +
  scale_fill_gradientn(
    "Peak window",
    colours = c(
      alpha("#3679c0", alpha),
      alpha("black", alpha),
      alpha("#ff007f", alpha)
    ),
    values = scales::rescale(c(-1, -0.2, -0.05, 0, 0.05, 0.2, 1)),
    labels = c(0, 0.5, 1, 1.5, 2)
  ) +
  theme_publication(18) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.2,0.1,2,0.2),"cm"),
        legend.position=c(0.43,-0.25)
  ) +
  guides(fill = guide_colourbar(barwidth = unit(7, "cm")))+
  labs(x = "Date of symptom onset", y = "Number of cases") +
  scale_y_continuous(
    labels = function(x)
      x / 100
  ) +
  scale_x_continuous(limits = c(0, 20))

ggsave(
  here("analysis/simulation/plots", "peak_coeff.png"),
  plot = p_peak_coeff,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

rm(list = setdiff(ls(), ls_snapshot))
gc()
