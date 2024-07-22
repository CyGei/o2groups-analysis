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
