library(tidyverse)
library(santoku)

# Data --------------------------------------------------------------------

# Delta estimator function
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

min <- 0
max <- 10
step <- 1
f_start <- 0.01
f_width <- 0.01

grid <- expand.grid(
  x = seq(min, max, by = step),
  n = seq(min, max, by = step),
  f = seq(f_start, 1-f_width, by = f_width),
  delta = seq(-1, 1, 0.1)
)
grid <- grid[grid$n >= grid$x & grid$n > 0,]
grid$est <- estimator(grid$x, grid$n, grid$f)
grid$bias <- round(grid$delta - grid$est, 3)





estimator_x_n <- Vectorize(function(x_n, f, alpha = 0.05) {
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


#create a grid such that x_n and f go from 0 to 1 in steps of 0.1
new_grid <- expand.grid(
  x_n = seq(0, 1, by = 0.1),
  f = seq(0, 1, by = 0.1),
  delta = seq(-1, 1, 0.1)
) %>%
  mutate(est = estimator_x_n(x_n, f, delta),
         bias = delta - est)


# mutate(delta_cut = santoku::chop(
#   delta,
#   breaks = sort(c(0, seq(-1, 1, by = 0.2))),
#   labels = santoku::lbl_midpoints()
# ))
# Plot --------------------------------------------------------------------

#heatmap of bias coloured by f and x/n
grid %>%
  mutate(
    delta_cut = santoku::chop(
      delta,
      breaks = sort(c(0, seq(-1, 1, by = 0.2))),
      labels = santoku::lbl_midpoints()
    ),
    x_n = x / n
  ) %>%
  ggplot(aes(x = x_n, y = f, fill = bias)) +
  facet_wrap( ~ delta_cut,
              labeller = labeller(delta_cut = function(x) paste0("\U03B4 = ", x))) +
  geom_tile(
    width = (1 / (max - min)) * step,   #based on the step size of x/n
    height = f_width                      #based on the specified f_width
  ) +
  scale_fill_gradientn(
    colours = c("#ff007f", "grey", "#00CC66")
  )+
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = "black",
    size = 0.5
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = c(0, 0.01)) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = c(0, 0.01))+
  #theme_publication()+
  theme(
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown(),
    legend.title = ggtext::element_markdown(),
    legend.position = "right"
  )


# newgrid
new_grid %>%
  ggplot(aes(x = x_n, y = f, fill = bias)) +
  facet_wrap( ~ delta,
              labeller = labeller(delta = function(x) paste0("\U03B4 = ", x))) +
  geom_tile()+
  scale_fill_gradientn(
    colours = c("#ff007f", "grey", "#00CC66"),
    na.value = "white"
  )+
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = "black",
    size = 0.5
  )




#
#
# plotly::plot_ly(
#   data = grid,
#   x = ~ delta,
#   y = ~ x/n,
#   z = ~ f,
#   color = ~ bias,
#   type = 'scatter3d',
#   mode = 'markers',
#   symbol = "square",
#   #not working
#   colors = c("blue", "white", "red")
# )
