

# Functions ---------------------------------------------------------------
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
    #"size",
    "size_freq",
    #"intro_n",
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
   # "size",
    "size_freq",
    #"intro_n",
    "intro_prop",
    "r0")

epidemic_specific_predictors <- c("n_groups",
                                  "GT_mean",
                                  "GT_sd",
                                  "INCUB_mean",
                                  "INCUB_sd")
predictor_name_labels <- c(
  latex2exp::TeX(r"( $\delta$ )"),
  #latex2exp::TeX(r"( $Size$ )"),
  latex2exp::TeX(r"( $\textit{f}$ )"),
  #latex2exp::TeX(r"( $n_{intro}$ )"),
  latex2exp::TeX(r"( $p_{intro}$ )"),
  latex2exp::TeX(r"( $R_0$ )"),
  latex2exp::TeX(r"( $n_{groups}$ )"),
  latex2exp::TeX(r"( $\mu_{GT}$ )"),
  latex2exp::TeX(r"( $\sigma_{GT}$ )"),
  latex2exp::TeX(r"( $\mu_{INCUB}$ )"),
  latex2exp::TeX(r"( $\sigma_{INCUB}$ )")
)

predictor_lookup_table <- tibble(
  name = predictors,
  label = predictor_name_labels
)

# Data --------------------------------------------------------------------
scenarios <-
  read_scenarios(path = here("analysis/simulation/data", "scenarios"))
scenarios <- bind_rows(scenarios)

hist_df <- scenarios %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  mutate(group_level = ifelse(
    predictor_name %in% group_specific_predictors,
    "Group Specific",
    "Epidemic Specific"
  ))


hist_df$predictor_name <- factor(
  hist_df$predictor_name,
  levels = predictor_lookup_table$name,
  labels = predictor_lookup_table$label
)
hist_df$group_level <- factor(
  hist_df$group_level,
  levels = c("Group Specific", "Epidemic Specific"),
  labels = c(
    latex2exp::TeX(r"( Group Specific )", bold = TRUE, italic = TRUE),
    latex2exp::TeX(r"( Epidemic Specific )", bold = TRUE, italic = TRUE)
  )
)

mean_df <-
  hist_df %>%
  group_by(predictor_name, group_level) %>%
  summarise(
    mean = mean(predictor_value),
    sd = sd(predictor_value),
    .groups = "drop"
  )




individual_plots <- function(var_name) {
  filter_var <-
    predictor_lookup_table[predictor_lookup_table$name == var_name,]$label
  hist_df %>%
    filter(predictor_name == filter_var) %>%
    ggplot(aes(x = predictor_value)) +
    ggh4x::facet_nested_wrap(
      group_level ~ predictor_name,
      scales = "free",
      labeller = labeller(predictor_name = label_parsed,
                          group_level = label_parsed)
    ) +
    geom_histogram(
      aes(y = after_stat(count / sum(count))),
      fill = "grey",
      colour = "black",
      bins = 21
    ) +
    geom_pointrange(
      data = mean_df %>%
        filter(predictor_name == filter_var),
      aes(
        y = 0 ,
        x = mean,
        xmin = mean - sd,
        xmax = mean + sd,
        fill = "Mean (+/- 1SD)"
      ),
      color = "orange", # add legend for this
      size = 0.75
    ) +
    theme_publication() +
    labs(x = "value",
         y = "Frequency",
         fill = ""
         )+
    theme(plot.margin = unit(c(0,0,0,0), "cm"))
}


p_list <- purrr::map(predictors, individual_plots)

p <- wrap_plots(p_list, guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

# hist_df %>%
#   group_by(predictor_name, group_level) %>%
#   mutate(
#     predictor_value_bin = santoku::chop_evenly(predictor_value,
#                                                intervals = 21,
#                                                labels =  lbl_midpoints())
#   ) %>%
#   count(predictor_value_bin) %>%
#   mutate(xbin = as.numeric(as.character(predictor_value_bin))) %>%
#   ggplot(aes(group = predictor_name)) +
#   ggh4x::facet_nested_wrap(
#     group_level ~ predictor_name,
#     scales = "free",
#     labeller = labeller(predictor_name = label_parsed,
#                         group_level = label_parsed)
#   ) +
#   geom_segment(aes(
#     x = xbin,
#     xend = xbin,
#     y = 0,
#     yend = n
#   ), size = 8) +
#   geom_pointrange(data = mean_df,
#                 aes(
#                   y = 0 ,
#                   x = mean,
#                   xmin = mean - sd,
#                   xmax = mean + sd
#                 ),
#                 color = "orange",
#                 size = 0.75) +
#   scale_x_continuous()+
#   scale_y_continuous()+
#   theme_publication() +
#   labs(x = NULL,
#        y = "Count")

ggsave(
  here("analysis/simulation/plots", "scenario_hist.png"),
  p,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

cat("scenario_hist.png \n")

rm(list = setdiff(ls(), ls_snapshot))
gc()


