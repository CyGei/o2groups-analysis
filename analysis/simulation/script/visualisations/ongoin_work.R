# labs(x = "True Delta",
#      y = expression(paste("Bias (", delta, "-", bar(delta), ")")))+



summary_df %>%
  filter(peak_coeff == "1" & alpha == "0.05") %>%
  select(delta, sensitivity, size_freq) %>%
  ggplot(aes(x = delta, y = sensitivity, z = size_freq)) +
  stat_summary_hex(bins = 100) +
  viridis::scale_fill_viridis(option = "plasma") +
  theme_publication() +
  labs(x = "True Delta", y = "Sensitivity", fill = "Relative Size (f)")

summary_df %>%
  filter(peak_coeff == "1" & alpha == "0.05") %>%
  select(delta, sensitivity, size_freq) %>%
  ggplot(aes(x = delta, y = sensitivity, color = size_freq)) +
  geom_point() +
  viridis::scale_color_viridis(option = "plasma") +
  theme_publication() +
  labs(x = "True Delta", y = "Sensitivity", color = "Relative Size (f)")


summary_df %>%
  filter(peak_coeff == "1" & alpha == "0.05") %>%
  select(delta, bias, size_freq) %>%
  ggplot(aes(x = delta, y = bias, color = size_freq)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  viridis::scale_color_viridis(option = "plasma") +
  theme_publication() +
  labs(x = "True Delta",
       y = expression(paste("Bias (", delta, "-", bar(delta), ")")),
       color = "Relative Size (f)")


summary_df %>%
  filter(peak_coeff == "1" & alpha == "0.05") %>%
  select(delta, bias, size_freq) %>%
  ggplot(aes(x = delta, y = bias, z = size_freq)) +
  stat_summary_hex(bins = 100) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  viridis::scale_color_viridis(option = "plasma") +
  theme_publication() +
  labs(x = "True Delta",
       y = expression(paste("Bias (", delta, "-", bar(delta), ")")),
       color = "Relative Size (f)")


# make a grid of x and x' value do x -x' and plot that against the bias
expand.grid(truth = seq(-1, 1, 0.1), est = seq(-1, 1, 0.1)) %>%
  mutate(bias = truth - est) %>%
  ggplot(aes(x = truth, y = est)) +
  geom_tile(aes(fill = bias)) +
  scale_fill_gradient2(low = "#C70071",
                       high = "#00C756",
                       guide = "colorbar") +
  labs(x = expression(paste("True Delta (", delta, ")")),
       y = expression(paste("Estimated Delta (", bar(delta), ")")),
       fill = expression(paste("Bias (", delta, "-", bar(delta), ")"))) +
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
    )
  ) +
  theme(legend.key = element_rect(color = "black", fill = 'white'))
ggsave(
  here("analysis/simulation/plots", "bias_grid.png"),
  width = 5,
  height = 5,
  dpi = 300
)


grid <- expand.grid(
  x = seq(0, 100, by = 1),
  n = seq(0, 100, by = 1),
  f = seq(0.1, 1, by = 0.2)
)
grid <- grid[grid$n >= grid$x & grid$n > 0,]
grid$est <- estimator(grid$x, grid$n, grid$f)
grid

grid %>%
  filter(est >= 0) %>%
  ggplot() +
  facet_wrap(~ f) +
  geom_point(aes(x = n, y = x, col = est))



expand.grid(true = -0.5,
            est = seq(-1, 1, by = 0.1)) %>%
  mutate(bias = true - est) %>%
  ggplot(aes(x = true, y = est)) +
  geom_point(aes(col = bias), size = 10) +
  scale_color_gradient2(midpoint = 0)



summary_df %>%
  filter(peak_coeff == "1" & alpha == "0.05" & delta >= 0.4) %>%
  summarise(mean = mean(coverage, na.rm = TRUE),
            sd = sd(coverage, na.rm = TRUE))


summary_df %>%
  filter(peak_coeff == "1" & alpha == "0.05") %>%
  mutate(delta_cut = santoku::chop(delta, breaks = sort(c(seq(
    -1, 1, 0.1
  ), 0)))) %>%
  group_by(delta_cut) %>%
  ggplot(aes(x = delta_cut, y = bias)) +
  geom_violin(fill = "grey", col = NA) +
  geom_boxplot(width = 0.08, outlier.shape = NA) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2,
    col = "royalblue"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_publication() +
  labs(x = "True Delta",
       y = expression(paste("Bias (", delta, "-", bar(delta), ")"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(-0.25, 0.25))








summary_df %>%
  filter(peak_coeff == "1" & alpha == "0.05") %>%
  select(sd_peak_date, sensitivity) %>%
  mutate(sd_peak_date = santoku::chop(sd_peak_date,
                                      breaks = seq(0, 10, 0.5),
                                      close_end = FALSE )) %>% #santoku::chop_evenly(sd_peak_date, intervals = 15)) %>%
  ggplot(aes(x = sd_peak_date, y = sensitivity)) +
  geom_violin(fill = "grey", col = NA)+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x, 0.25),
               fun.ymax = function(x) quantile(x, 0.75),
               geom = "pointrange",
               col = "steelblue") +
  stat_summary(fun = median, geom = "line", col = "steelblue", group = 1) +
  stat_summary(fun = mean, geom = "point", size = 2, col = "black") +
  stat_summary(fun = mean, geom = "line",  col = "black", group = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



summary_df %>%
  filter(peak_coeff == "1" & alpha == "0.05") %>%
  select(size_freq, sensitivity) %>%
  mutate(size_freq = santoku::chop(size_freq, breaks = seq(0, 1, 0.1))) %>%
  ggplot(aes(x = size_freq, y = sensitivity)) +
  geom_violin(fill = "grey", col = NA)+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x, 0.25),
               fun.ymax = function(x) quantile(x, 0.75),
               geom = "pointrange",
               col = "steelblue") +
  stat_summary(fun = median, geom = "line", col = "steelblue", group = 1) +
  stat_summary(fun = mean, geom = "point", size = 2, col = "black") +
  stat_summary(fun = mean, geom = "line",  col = "black", group = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



summary_df %>%
  filter(peak_coeff == "1" & alpha == "0.05") %>%
  select(size_freq, bias, delta) %>%
  mutate(size_freq = santoku::chop(size_freq, breaks = sort(c(0.5, seq(0, 1, 0.1)))),
         delta_sign = case_when(
           delta < 0 ~ "Negative",
           delta > 0 ~ "Positive",
           delta == 0 ~ "Zero"
         )) %>%
  ggplot(aes(x = size_freq, y = bias)) +
  facet_wrap(~ delta_sign) +
  geom_violin(fill = "grey", col = NA)+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x, 0.25),
               fun.ymax = function(x) quantile(x, 0.75),
               geom = "pointrange",
               col = "steelblue") +
  stat_summary(fun = median, geom = "line", col = "steelblue", group = 1) +
  stat_summary(fun = mean, geom = "point", size = 2, col = "black") +
  stat_summary(fun = mean, geom = "line",  col = "black", group = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

