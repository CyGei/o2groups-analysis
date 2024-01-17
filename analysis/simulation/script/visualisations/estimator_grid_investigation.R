library(o2groups)
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))
source(here("analysis/simulation/script/simulations/process_helpers.R"))




# Scenarios ---------------------------------------------------------------

# relative_size <- function(x) {
#   return(x / sum(x))
# }
#
# absolute_size <- function(proportions, total) {
#   return(proportions * total)
# }
#
# delta_seq <- c(-1, 0, 1)
# delta_seq <-
#   c(min(delta_seq) + 0.1,
#     delta_seq[2:(length(delta_seq) - 1)],
#     max(delta_seq) - 0.1)
# grid <- expand.grid(
#   size1 = c(25,50,75),
#   size2 = c(25,50,75),
#   delta1 = delta_seq,
#   delta2 = delta_seq
# ) %>%
#   filter(size1 + size2 == 100)
#
# grid_list <- apply(grid, 1, as.list)
#
# scenarios <-
#   map(
#     .x = grid_list,
#     ~ list(
#       scenario = noah::pseudonymize("A"),
#       size = c(.x$size1, .x$size2),
#       scaled_delta = c(.x$delta1, .x$delta2),
#       delta = o2groups::reverse_scale(c(.x$delta1, .x$delta2)),
#       duration = 365,
#       n_groups = 2,
#       intro_n = round(c(.x$size1, .x$size2) * 0.1),
#       name = LETTERS[1:2],
#       r0 = c(2, 2)
#     )
#   )



# Grid Based Simulation ---------------------------------------------------


# n_workers <- future::availableCores() - 1
# plan(multisession, workers = n_workers)
# n_simulations <- 100
# master_list <- list()
# set.seed(123)
# tictoc::tic()
# for(i in 1:length(scenarios)) {
#   scenario <- scenarios[[i]]
#   cat("Running scenario", scenario$scenario, "\n")
#   master_list[[scenario$scenario]] <- list()
#   # Simulations
#   simulations <- purrr::map(1:n_simulations, function(j) {
#     sim <- simulate_groups(
#       duration = scenario$duration,
#       n_groups = scenario$n_groups,
#       size = scenario$size,
#       name = scenario$name,
#       delta = scenario$delta,
#       intro_n = scenario$intro_n,
#       r0 = scenario$r0,
#       generation_time = simulacr::make_disc_gamma(mean = 4, sd = 1)$d(0:50),
#       incubation_period = simulacr::make_disc_gamma(mean = 4, sd = 1)$r(sum(scenario$size)),
#     )$data
#     sim$scenario <- scenario$scenario
#     sim$simulation <- j
#     return(sim)
#   })
#   master_list[[scenario$scenario]]$simulations <- simulations
#
#   # Transmissions by Peak cutoffs
#   results <-
#     process_simulations(simulations = simulations,
#                         peak_coeffs = 1,
#                         scenario = scenario)
#
#
#   master_list[[scenario$scenario]]$results <- results
#
#   # Scenario Dataframe
#   scenario_df <- scenario_to_df(scenario)
#   master_list[[scenario$scenario]]$scenario_df <- scenario_df
#
#   # Beta & Delta Estimates
#   estimates <- compute_estimates(alpha = 0.05,
#                                  scenario_df = scenario_df[,!(names(scenario_df) %in% "scenario")],
#                                  results = results)
#
#   master_list[[scenario$scenario]]$estimates <- estimates
#
#
# }
# tictoc::toc() #~13min
# saveRDS(master_list,here("analysis/simulation/data", "master_list.rds"))



# Grid Simulation Results -----------------------------------------------------------------
master_list <- readRDS(here("analysis/simulation/data", "master_list.rds"))


# extract all results dataframes
estimates <- purrr::map(master_list, function(x) {
  return(x$estimates)
}) %>%
  dplyr::bind_rows(.id = "scenario")


appender <- function(string){
  latex2exp::TeX(paste("$\\delta_{\\a} = $", string))

}
p_grid_simulations <- as_tibble(estimates) %>%
  select(scenario, name, size_freq, beta_est, est, delta, bias) %>%
  ggplot(aes(x = size_freq, y = beta_est, fill = bias)) +
  facet_wrap( ~ delta,
                labeller = as_labeller(appender,
                                       default = label_parsed)
  )+
  geom_point(size = 4,
             shape = 22,
             stroke = 0.1,
             position = position_jitter(width = 0.05, height = 0),
             col = "lightgrey")+
  scale_fill_gradientn(
    colours = c("#008080", "white","#EEBC54"),
    #colours = c("#008000", "white", "#800080"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  )+
  scale_x_continuous(breaks = seq(0, 1, by = 0.25))+
  labs(x =latex2exp::TeX("$f_{\\a}$"),
       y =latex2exp::TeX("$\\bar{\\pi_}_{\\a\\leftarrow a}$"),
       fill = expression(paste("Bias (", delta, "-", bar(delta), ")")))+
  theme_publication()
p_grid_simulations


# ggsave(
#   here("analysis/simulation/plots", "grid_simulations.png"),
#   p_grid_simulations,
#   width = 7.5,
#   height = 4.3,
#   units = "in",
#   dpi = 500
# )




scenario_df <- purrr::map(master_list, function(x) {
  return(x$scenario_df)
}) %>%
  dplyr::bind_rows(.id = "scenario")

summary <- estimates %>%
  group_by(name, scenario) %>%
  summarise(
    peak_date = mean(peak_date, na.rm = TRUE),
    bias = mean(bias, na.rm = TRUE),
    sensitivity = sum(true_positive, na.rm = TRUE) / sum(significant_delta, na.rm = FALSE),
    specificity = sum(true_negative, na.rm = TRUE) / sum(significant_delta == FALSE, na.rm = FALSE),
    n_cases = mean(n_cases, na.rm = TRUE),
    total_cases = mean(total_cases, na.rm = TRUE),
    beta_est = mean(beta_est, na.rm = TRUE),
  ) %>%
  ungroup()%>%
  left_join(scenario_df, by = c("scenario", "name")) %>%
  mutate(group_susceptibles = 1 - (n_cases / size),
    sd_peak_date = sd(peak_date, na.rm = TRUE),
    sd_n_cases = sd(n_cases, na.rm = TRUE),
    sd_delta = sd(delta, na.rm = TRUE),
    sd_r0 = sd(r0, na.rm = TRUE),
    sd_intro_n = sd(intro_n, na.rm = TRUE),
    sd_intro_prop = sd(intro_prop, na.rm = TRUE),
    sd_group_susceptibles = sd(group_susceptibles, na.rm = TRUE),
    total_susceptibles = 1 - (sum(n_cases) / sum(size)),
    sd_beta_est = sd(beta_est, na.rm = TRUE)
  ) %>%
  ungroup()

summary %>%
  ggplot(aes(x = delta, y = sensitivity, fill = size_freq)) +
  facet_wrap( ~ scenario)+
  geom_label(aes(label = name),
             size = 3,
             color = "black")+
  scale_x_continuous(breaks = c(-0.99,0, 0.99))+
  scale_fill_gradientn(
    colours = c("orange", "white","purple"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  )



scenario_names <- sort(unique(scenario_df$scenario))

summary %>%
  filter(scenario %in% scenario_names[c(7, 14, 17, 18)]) %>%
  ggplot(aes(x = delta, y = sensitivity)) +
  facet_wrap( ~ scenario) +
  geom_point(aes(color = name,
                 size = size_freq)) +
  geom_text(aes(label = name),
            color = "black") +
  scale_x_continuous(breaks = c(-0.99, 0, 0.99)) +
  scale_size_continuous(breaks = c(0.25, 0.5, 0.75)) +
  labs(title = "Scenario Configurations (2 groups)")

estimates %>%
  filter(scenario %in% scenario_names[c(7,14,17,18)]) %>%
  ggplot(aes(x = trials,
             y = est,
             group = name,
             color = name)) +
  facet_wrap( ~ scenario)+
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci)) +
  geom_hline(aes(yintercept = delta,
                 color = name))+
  geom_hline(aes(yintercept = 0), linetype = "dashed")


estimates %>%
  filter(scenario %in% scenario_names[c(7,14,17,18)]) %>%
  ggplot() +
  facet_wrap( ~ scenario)+
  geom_histogram(aes(x = peak_date,
                     group = name,
                     fill = name))+
  ggtitle("Peak Date by Group (100 simulations)")

estimates %>%
  filter(scenario %in% scenario_names[c(7,14,17,18)]) %>%
  ggplot() +
  facet_wrap( ~ scenario)+
  geom_histogram(aes(x = trials,
                     group = name,
                     fill = name))+
  labs(x = latex2exp::TeX("$\\tau_{.\\leftarrow a}$"))+
  ggtitle("No of transmissions pairs emitted by Group (over 100 simulations)")


estimates %>%
  filter(scenario %in% scenario_names[c(7,14,17,18)]) %>%
  ggplot() +
  facet_wrap( ~ scenario)+
  geom_histogram(aes(x = successes,
                     group = name,
                     fill = name))+
  labs(x = latex2exp::TeX("$\\tau_{a\\leftarrow a}$"))+
  ggtitle("No of within group transmission pairs by Group (over 100 simulations)")

# CI grid -----------------------------------------------------------------


# Specify x/n values
target_values <- c(0, 0.25, 0.5, 0.75, 1)
# Initialize an empty data frame to store results
pi_df <- data.frame()

# Loop through possible combinations to find x and n values
for (x_over_n in target_values) {
  for (n in seq(10, 100, by = 10)) {
    x <- round(x_over_n * n)

    # Ensure that x is a valid value (between 0 and n)
    if (x >= 0 && x <= n) {
      result <- data.frame(x, n, b_est(x, n))
      pi_df <- rbind(pi_df, result)
    }
  }
}

# Plot Pi results
ggplot(pi_df,
       aes(x = n,
           y = est,
           color = x)) +
  geom_hline(yintercept = target_values,
             linetype = "dashed",
             color = "grey") +
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  position =  position_dodge2(width = 5)) +
  scale_color_gradientn(
    colours = c("black", "#FF00FF"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_publication()



delta_df <- mutate(.data = pi_df,
                   across(
                     .cols = c("est", "lower_ci", "upper_ci"),
                     .fns = function(x) {
                       gamma <- o2groups::get_gamma(beta = x, f = 0.5)
                       delta <- o2groups::scale(gamma)
                       return(delta)
                     }
                   ))
# %>%
#   rename_with(
#     .fn = \(x) paste0("delta_", x),
#     .cols =  c("est", "lower_ci", "upper_ci")
#   )

b_est(0, 10) %>%
  o2groups::get_gamma(beta = ., f = 0.5) %>%
  o2groups::scale()

# Plot results
ggplot(delta_df,
       aes(x = n,
           y = est,
           color = x)) +
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  position =  position_dodge2(width = 5)) +
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "red") +
  scale_color_gradientn(
    colours = c("black", "#FF00FF"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_publication()+
  labs(
    y = latex2exp::TeX("$\\bar{\\delta}_a$"),
    x = latex2exp::TeX("$\\tau_{.\\leftarrow a}$"),
    color = latex2exp::TeX("$\\tau_{a\\leftarrow a}$"),
    title =  latex2exp::TeX("Estimates of $\\bar{\\delta}_a$ by $\\tau_{.\\leftarrow a}$ & $\\tau_{a\\leftarrow a}$")
  )



