# Investigate why sensitivity is poor when delta is large:
# find groups with high delta but low sensitivity

df <- summary_df %>%
  filter(alpha == 0.05 & peak_coeff == 1) %>%
  select(sensitivity, all_of(predictors)) %>% #predictors <- names(summary_long)[!( names(summary_long) %in% c(metrics, id_vars, extra_metrics))]
  drop_na(sensitivity) %>%
  pivot_longer(
    cols = all_of(predictors[!predictors == 'delta']),
    names_to = "predictor_name",
    values_to = "predictor_value"
  ) %>%
  group_by(predictor_name) %>%
  mutate(predictor_value = (predictor_value - min(predictor_value)) / (max(predictor_value) - min(predictor_value)))

bins = 100

df %>%
  ggplot(aes(x = delta, y = sensitivity)) +
  facet_wrap(~ predictor_name) +
  stat_summary_hex(
    aes(z = predictor_value),
    bins = bins,
    colour = NA,
    geom = "tile",
    #hex
    drop = TRUE
  ) +
  theme_publication() +
  scale_fill_viridis_c(name = "Normalised Value",
                       option = "plasma")

df %>%
  filter(predictor_name %in% c("n_cases", "size", "sd_peak_date")) %>%
  ggplot(aes(x = delta, y = sensitivity)) +
  facet_wrap(~ predictor_name) +
  stat_summary_hex(
    aes(z = predictor_value),
    bins = bins,
    colour = NA,
    geom = "tile",
    #hex
    drop = TRUE
  ) +
  theme_publication() +
  scale_fill_viridis_c(name = "Normalised Value",
                       option = "plasma")


df %>%
  filter(predictor_name %in% c("size_freq")) %>%
  ggplot(aes(x = delta, y = sensitivity)) +
  facet_wrap(~ predictor_name) +
  stat_summary_hex(
    aes(z = predictor_value),
    bins = 50,
    colour = NA,
    geom = "tile",
    #hex
    drop = TRUE
  ) +
  theme_publication() +
  scale_fill_viridis_c(name = "Normalised Value",
                       option = "plasma")



pointrange_df <- df %>%
  pivot_wider(names_from = predictor_name,
              values_from = predictor_value) %>%
  filter(abs(delta) > 0.8 & size <= 0.1) %>%
  mutate(delta_sign = ifelse(delta > 0, "positive", "negative")) %>%
  pivot_longer(
    cols = all_of(predictors[!predictors == 'delta']),
    names_to = "predictor_name",
    values_to = "predictor_value"
  ) %>%
  group_by(predictor_name, delta_sign) %>%
  summarise(
    mean = mean(predictor_value),
    sd = sd(predictor_value),
    se = sd / sqrt(n()),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se
  ) %>%
  group_by(predictor_name) %>%
  #comppute the differences between the means of delta signs for a given predictor
  mutate(delta_sign_diff = mean - lag(mean)) %>%
  ungroup() %>%
  # sort the from largest to smallest to plot
  arrange(desc(abs(delta_sign_diff)))



pointrange_df %>%
  ggplot(aes(
    x = predictor_name,
    y = mean,
    ymin = lower,
    ymax = upper,
    col = delta_sign
  )) +
  geom_pointrange(position = position_dodge(0.5)) +
  scale_x_discrete(limits = unique(pointrange_df$predictor_name)) +
  scale_color_manual(values = c("positive" = "red", "negative" = "blue")) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#
# est_df =
#   read_files(path = here("analysis/simulation/data", "estimates"),
#              sample_n = 100) %>%
#   bind_rows()

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
  filter(n >= x)  # Ensure that n is greater than or equal to x


# Apply the function to the grid
results <- grid %>%
  rowwise() %>%
  mutate(result = list(calculate_delta(x, n, f)))

# Unpack the results
results <- results %>%
  unnest_wider(result,
               names_sep = "_") %>%
  as.matrix() %>%
  as_tibble() %>%
  rename(est = 4,
         lower_ci = 5,
         upper_ci = 6)

# Plot the results
ggplot(results, aes(
  x = n,
  y = x,
  fill = est,
  colour = est
)) +
  facet_wrap(~ f) +
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
  theme_publication() +
  labs(
    fill = "Inferred Delta",
    colour = "Inferred Delta",
    x = "No of Transmissions Emitted",
    y = "No of Within-Group Transmissions"
  )


bin_df <- df %>%
  filter(predictor_name == "size_freq") %>%
  pivot_wider(names_from = predictor_name,
              values_from = predictor_value) %>%
  mutate(
    delta_bin = cut(delta, seq(-1, 1, by = 0.1)),
    sensitivity_bin = cut(sensitivity, seq(0, 1, by = 0.1)),
    size_freq_bin = cut(size_freq, seq(0, 1, by = 0.1))
  ) %>%
  rowwise() %>%
  mutate(across(ends_with("_bin"), \(x) get_midpoint(x),
                .names = "{.col}_midpoint"))
bin_df

# Calculate the counts for each size_freq_bin
count_df <- bin_df %>%
  drop_na(size_freq_bin) %>%
  group_by(size_freq_bin) %>%
  summarise(count = n())

bin_df %>%
  drop_na(size_freq_bin) %>%
  ggplot(aes(x = delta, y = sensitivity)) +
  facet_wrap( ~ size_freq_bin) +
  geom_point() +
  geom_smooth() +
  geom_label(data = count_df,
             aes(
               x = 0,
               y = 0.8,
               label = paste0("n= ", count)
             )) +
  theme_publication() +
  scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.1),
                     breaks = seq(0,1, 0.25)) +
  labs(x = "Delta", y = "Sensitivity")


#rm(list = setdiff(ls(), ls_snapshot))
#gc()
