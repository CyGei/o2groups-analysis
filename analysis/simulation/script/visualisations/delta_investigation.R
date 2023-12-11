
# Binomial Test
b_est <- function(x, n, alpha = 0.05) {
  est <- x / n
  lwr <- qbeta(alpha / 2, x, n - x + 1)
  zvals <- !is.na(x) & x == 0
  nvals <- !is.na(x) & x == n
  lwr[zvals] <- 0
  lwr[nvals] <- (alpha / 2) ^ (1 / n[nvals])
  upr <- qbeta(1 - alpha / 2, x + 1, n - x)
  upr[zvals] <- 1 - (alpha / 2) ^ (1 / n[zvals])
  upr[nvals] <- 1
  cbind(est, lwr, upr)
}

#Calculate Delta
calculate_delta <- function(x, n, f, alpha = 0.05) {
  b_est(x, n, alpha = alpha) %>%
    o2groups::get_gamma(beta = ., f = f) %>%
    o2groups::scale()
}

grid <- expand.grid(
  x = seq(0, 100, by = 1),
  n = seq(0, 100, by = 1),
  f = seq(0.1, 0.9, by = 0.1)
) %>%
  filter(n >= x)


# Apply the function to the grid
results <- grid %>%
  rowwise() %>%
  mutate(result = list(calculate_delta(x, n, f))) %>%
  unnest_wider(result,
               names_sep = "_") %>%
  as.matrix() %>%
  as_tibble() %>%
  rename(est = 4,
         lower_ci = 5,
         upper_ci = 6)

# Plot the results
p_delta_investigation <- ggplot(results, aes(
  x = n,
  y = x,
  fill = est,
  colour = est
)) +
  facet_wrap(vars(f),
             labeller = labeller(
               f = function(x)
                 paste("*f* = ", x)
             )) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  scale_colour_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    fill = "Inferred Delta",
    colour = "Inferred Delta",
    x = "No. of Transmissions Emitted (***n***)",
    y = "No. of Within-Group Transmissions (***x***)"
  ) +
  theme_publication() +
  theme(
    strip.text = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown()
  ) +
  guides(
    colour = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      barwidth  = 10,
      nrow = 1,
      byrow = TRUE
    ),
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      barwidth = 10,
      nrow = 1,
      byrow = TRUE
    )
  )

ggsave(
  here::here(
    "analysis/simulation/plots",
    "delta_investigation.png"
  ),
  p_delta_investigation,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)


library(plotly)
p_plotly<- plotly::plot_ly(
  results, x= ~n, y= ~x, z= ~est,
  type='mesh3d', intensity = ~est,
  colorscale = list(
    c(0, "blue"),
    c(0.5, "white"),
    c(1, "red")
  )
) %>%
  layout(
    scene = list(
      xaxis = list(title = "No. of Transmissions Emitted (n)"),
      yaxis = list(title = "No. of Within-Group Transmissions (x)"),
      zaxis = list(title = "Inferred Delta")
    ),
    margin = list(l = 0, r = 0, b = 0, t = 0)
  )%>% colorbar(title = "Inferred Delta")

htmlwidgets::saveWidget(
  as_widget(p_plotly),
  here::here(
    "analysis/simulation/plots",
    "plotly_delta_investigation.html"
  ),
)

rm(list = setdiff(ls(), ls_snapshot))
gc()
