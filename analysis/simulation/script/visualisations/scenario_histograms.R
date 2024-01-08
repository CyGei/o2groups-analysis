

#remove object from list (scenario)
remove_object <- function(list, object_name) {
  list <- list[!names(list) %in% object_name]
  return(list)
}

scenario_to_df <- function(scenario) {
  scenario_df <- remove_object(
    scenario,
    c(
      "scenario",
      "generation_time",
      "incubation_period",
      "duration",
      "delta"
    )
  )
  scenario_df <-
    rename(.data = as.data.frame(scenario_df), delta = scaled_delta)
  scenario_df$size_freq <-
    scenario_df$size / sum(scenario_df$size)
  scenario_df$intro_prop <- scenario_df$intro_n / scenario_df$size
  return(scenario_df)
}

read_scenarios <- function(path, sample_n = Inf) {
  files <-
    list.files(path = path,
               pattern = "*.rds",
               full.names = TRUE)

  # Sample files if sample_n is provided
  if (sample_n < length(files)) {
    set.seed(123)  # You can use any seed value
    files <- sample(files, size = sample_n)
  }

  list <- furrr::future_map(
    .x = files,
    .f = ~ readRDS(.x) %>% scenario_to_df() %>% mutate(scenario = basename(.x)),
    .options = furrr_options(seed = NULL)
  )

  return(list)
}


predictors <-
  c(
    "delta",
    "size",
    "size_freq",
    "intro_n",
    "intro_prop",
    "r0",
    "n_groups",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd"
  )

group_specific_predictors <-
  c("delta",
    "size",
    "size_freq"
    "intro_n",
    "intro_prop",
    "r0")

epidemic_specific_predictors <- c("n_groups",
                                  "GT_mean",
                                  "GT_sd",
                                  "INCUB_mean",
                                  "INCUB_sd")
predictor_name_labels <-
  stringr::str_to_title(paste0(
    "**",
    #letters[seq_along(predictors)],
    #". ",
    stringr::str_replace_all(predictors, "_", " "),
    "**"
  ))
predictor_name_labels <-
  setNames(predictor_name_labels, predictors)
predictor_level_labels <- c(
  "Group Specific" = expression(bolditalic("Group Specific")),
  "Epidemic Specific" = expression(bolditalic("Epidemic Specific"))
)


# scenarios <-
#   read_scenarios(path = here("analysis/simulation/data", "scenarios"))
# scenarios <- bind_rows(scenarios)

# DATA
hist_df <- scenarios %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  mutate(
    predictor_level = ifelse(
      predictor_name %in% group_specific_predictors,
      "Group Specific",
      "Epidemic Specific"
    ),
    predictor_level = factor(
      predictor_level,
      levels = c("Group Specific", "Epidemic Specific")
    ),
    predictor_name = factor(
      predictor_name,
      levels = predictors,
      labels = predictor_name_labels
    )
  ) %>%
  select(-all_of(c("name", "scenario")))

mean_df <-
  hist_df %>%
  group_by(predictor_name, predictor_level) %>%
  summarise(mean = mean(predictor_value),
            sd = sd(predictor_value),
            .groups = "drop")

scenario_hist <- hist_df %>% ggplot(aes(
  group = predictor_name,
  x = predictor_value
)) +
  ggh4x::facet_nested_wrap(
    predictor_level~predictor_name,
    scales = "free",
    #add Letter (A, B, C, etc.) to each facet
    labeller = labeller(
      predictor_name = as_labeller(function(label) {
        paste0(LETTERS[seq_along(predictor_name_labels)], ". ", label)
      }),
    )
    )+
  geom_histogram(aes(, y = ..density..), bins = 20) +
  geom_pointrange(
    data = mean_df,
    aes(y = 0 , x = mean, xmin = mean - sd, xmax = mean + sd),
    size = 0.75
  )+
  theme_publication()+
  theme(
    strip.text = ggtext::element_markdown(size = rel(1.2))
  )+
  labs(
    x = NULL,
    y = "Density"
  )


ggsave(
  here("analysis/simulation/plots", "scenario_hist.png"),
  scenario_hist,
  width = 20,
  height = 8,
  units = "in",
  dpi = 300
)



rm(list = setdiff(ls(), ls_snapshot))
gc()

title = "Distribution of predictor values",
subtitle = "Histograms show the distribution of predictor values across all scenarios. Points show the mean and 95% CI of the predictor values."

