# Model Sanity Check
# The purpose of this script is to check wether the proportions of within and between group transmission
# are simulated as expected.

pacman::p_load_gh("CyGei/o2groups") # for simulation
pacman::p_load_gh("CyGei/linktree@realtime") # for analysis
pacman::p_load(dplyr, furrr, ggplot2)

plan(multisession, workers = availableCores() - 2)

set.seed(123)
group_n = 5
size = rep(100, group_n)
name = LETTERS[1:group_n]
gamma = rep(1, group_n)
intro_n = rep(1, group_n)
r0 = rep(2, group_n)

ref <- tibble(group = name, ref = size / sum(size))

sims <- future_map(
  1:100,
  ~ simulate_groups(
    duration = duration,
    group_n = group_n,
    size = size,
    name = name,
    gamma = gamma,
    intro_n = intro_n,
    r0 = r0,
    generation_time = generation_time,
    incubation_period = incubation_period
  ),
  .options = furrr_options(seed = TRUE)
)

ttabs <- future_map(sims, \(x) {
  ttab <- linktree:::ttable(from = x$source_group,
                    to = x$group,
                    level = name)
  as_tibble(prop.table(ttab, 1))
}) %>%
  bind_rows(.id = "sim")

ttabs %>%
  ggplot(aes(y = n, x = 1)) +
  ggh4x::facet_nested(
    rows = vars("From", from),
    cols = vars("To", to),
    switch = "y"
  )+
  geom_violin(
    col = NA,
    fill = "#88807B",
    scale = "width",
    adjust = 0.9,
    alpha = 0.7
  )+
  #95% quantile and median
  stat_summary(
    fun.data = function(x) {
      y <- quantile(x, c(0.025, 0.5, 0.975))
      names(y) <- c("ymin", "y", "ymax")
      y
    },
    geom = "pointrange",
    fatten = 0.7,
    size = 0.5,
  )+
  geom_hline(
    data = ttabs %>%
      filter(from == to) %>%
      mutate(yintercept = unique(ref$ref)),
    aes(yintercept = yintercept),
    color = "steelblue"
    )+
  scale_y_continuous(position = "right")+
  theme_bw()+
  labs(y = expression(pi),
       x = "")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")




# Heatmap of pi and f -----------------------------------------------------

grid <- expand.grid(pi = seq(0, 1, 0.001),
                    f = seq(0, 1, 0.001)) %>%
  mutate(gamma = linktree:::gamma_formula(pi, f),
         delta = linktree::gamma2delta(gamma))


# heatmap ggplot2
grid %>%
  ggplot(aes(x = pi, y = f, fill = delta))+
  geom_tile()+
  scale_fill_gradientn(colors = c("purple", "white", "orange"),
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       guide = guide_colorbar(frame.colour = "black",
                                              frame.linewidth = 0.5))+
  labs(x = expression(pi[a %<-% a]), y = bquote(f[a]), fill = bquote(delta[a])) +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0)) +  # Add this line
  scale_y_continuous(expand = c(0, 0))  +
  theme(aspect.ratio = 1,
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        plot.margin = margin(10, 0, 0, 0))


 # The first plot shows the proportion of within and between group transmission for each simulation. The second plot shows the relationship between the proportion of within group transmission (\(\pi\)), the proportion of between group transmission (\(f\)), and the relative transmissibility of the virus within and between groups (\(\delta\)).
 # The proportion of within and between group transmission is simulated as expected. The proportion of within group transmission is higher than the proportion of between group transmission. The relationship between \(\pi\), \(f\), and \(\delta\) is also as expected.
 # The next step is to simulate the outbreak and analyze the data.
