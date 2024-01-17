hexbin_plot <- function(
    data,
    group_specific_predictors, #character vector
    epidemic_specific_predictors,#character vector
    predictor_name_labels = NULL, #named character vector
    alpha_value,
    peak_coeff_value,
    scatter_size = 0.5,
    meanpoint_size = 1,
    linerange_size = 0.25,
    linetrend_size = 0.4,
    base_size = 12
    ) {
  library(dplyr)
  library(ggplot2)
  library(ggtext)
  library(ggh4x)
  library(santoku)
  predictor_names <- c(group_specific_predictors, epidemic_specific_predictors)

  # DATA
  hex_df <- data %>%
    filter(
      alpha == alpha_value & peak_coeff == peak_coeff_value &
        predictor_name %in% predictor_names
    ) %>%
    mutate(
      alpha = as.character(alpha),
      predictor_level = ifelse(
        predictor_name %in% group_specific_predictors,
        "Group specific",
        "Epidemic specific"
      ),
      predictor_level = factor(
        predictor_level,
        levels = c("Group specific", "Epidemic specific")
      ),
      predictor_name = factor(
        predictor_name,
        levels = predictor_names
      ),
    ) %>%
    select(-all_of(c("name", "scenario")))

  mean_df <- hex_df %>%
    group_by(predictor_name) %>%
    mutate(predictor_value_chopped =
             # santoku::chop_quantiles(predictor_value,
             #                       probs = 1:3/4,
             #                       labels = santoku::lbl_midpoints())
             santoku::chop_evenly(predictor_value,
                                  intervals = 7,
                                  labels = santoku::lbl_midpoints())

           ) %>%

    group_by(
      alpha,
      peak_coeff,
      outcome_name,
      predictor_level,
      predictor_name,
      predictor_value_chopped
    ) %>%
    summarise(
      sd = sd(outcome_value, na.rm = TRUE),
      lower = quantile(outcome_value, 0.25, na.rm = TRUE),
      upper = quantile(outcome_value, 0.75, na.rm = TRUE),
      outcome_value = mean(outcome_value, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    group_by(alpha,
             peak_coeff,
             outcome_name,
             predictor_level,
             predictor_name) %>%
    mutate(freq = n / sum(n)) %>%
    ungroup() %>%
    mutate(predictor_value = as.numeric(as.character(predictor_value_chopped)))


  #LABELS
  outcome_labels <- c(
    "bias" = "**1. Bias**",
    "coverage" = "**2. Coverage**",
    "sensitivity" = "**3. Sensitivity**",
    "specificity" = "**4. Specificity**"
  )

  predictor_level_labels <- c(
    "Group specific" = expression(bolditalic("Group-specific")),
    "Epidemic specific" = expression(bolditalic("Epidemic-specific"))
  )


  if(is.null(predictor_name_labels)){
    predictor_name_labels <-
      stringr::str_to_title(paste0(
        "**",
        letters[seq_along(predictor_names)],
        ". ",
        stringr::str_replace_all(predictor_names, "_", " "),
        "**"
      ))
    predictor_name_labels <-
      setNames(predictor_name_labels, predictor_names)
  }


  hex_df <- hex_df %>%
    mutate(
      predictor_name = factor(
        predictor_name,
        levels = predictor_names,
        labels = predictor_name_labels
      )
    )

  mean_df <- mean_df %>%
    mutate(
      predictor_name = factor(
        predictor_name,
        levels = predictor_names,
        labels = predictor_name_labels
      )
    )

  #PLOT
  p_hexbin <-
    hex_df %>%
    ggplot(aes(x = predictor_value, y = outcome_value)) +
    ggh4x::facet_nested(
      outcome_name ~ predictor_level + predictor_name,
      labeller = labeller(
        outcome_name = outcome_labels,
        predictor_level = predictor_level_labels,
        predictor_name = label_parsed
      ),
      scales = "free"
    ) +
    geom_point(
      stroke = 0,
      shape = 16,
      alpha = 0.1,
      size = scatter_size,
      col = "black"
    ) +
    geom_hline(
      data = hline_df %>%
        mutate(target = ifelse(
          outcome_name == "coverage",
          1 - as.numeric(alpha_value),
          target
        )),
      aes(yintercept = target),
      colour = "#0041f3",
      linewidth = 0.35,
      linetype = "f4"
    ) +
    geom_linerange(
      data = mean_df,
      aes(ymin = lower,
          ymax = upper),
      col = "#f300bb",
      linewidth = linerange_size
    ) +
    geom_point(
      data = mean_df,
      aes(y = outcome_value),
      size = meanpoint_size,
      col = "#f300bb"
    ) +
    geom_line(data = mean_df,
              linewidth = linetrend_size,
              col = "#f300bb") +
    labs(
      x = "Predictor value",
      y = "Metric value",
      linetype = "",
      color = ""
    ) +
    theme_publication_hexbin(base_size = base_size) +
    theme(
      strip.text.x = element_text(size = rel(1.1)),
      strip.text.y = ggtext::element_markdown(size = rel(1.1)),
      axis.text.x = element_text(angle = 45, hjust = 1),
    ) +
    ggh4x::facetted_pos_scales(
      y = list(
        outcome_name == "bias" ~ scale_y_continuous(limits = c(-2, 2),
                                                    breaks = seq(-2, 2, 0.5)),
        outcome_name == "coverage" ~ scale_y_continuous(limits = c(0.25, 1)),
        outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
        outcome_name == "specificity" ~ scale_y_continuous(limits = c(0.25, 1))
      )
    ) +
    scale_x_continuous(n.breaks = 5) +
    guides(
      colour = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        barwidth = 10,
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

  return(p_hexbin)
}



# Master Plot ----------------------------------------------------------------

predictors <-
  c(
    "delta",
    "size",
    "size_freq",
    "intro_n",
    "intro_prop",
    "r0",
    "n_cases",
    "group_susceptibles",
    "successes",
    "trials",
    "n_groups",
    "total_cases",
    "total_susceptibles",
    "sd_peak_date",
    "sd_n_cases",
    "sd_group_susceptibles",
    "sd_size",
    "sd_size_freq",
    "sd_delta",
    "sd_r0",
    "sd_intro_n",
    "sd_intro_prop",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd"
  )

# labels <- c(
#   "delta[G]",
#   "s[G]",
#   "p[s[G]]",
#   "n[intro[G]]",
#   "p[intro[G]]",
#   "R[0[G]]",
#   "n[cases[G]]",
#   "p[susceptibles[G]]",
#   "n[G %<-% G]",
#   "n[. %<-% G]",
#   "n[groups]",
#   "sum(n[cases[G]])",
#   "p[susceptibles]",
#   "sigma[peak]",
#   "sigma[n[cases[G]]]",
#   "sigma[p[susceptibles[G]]]",
#   "sigma[s[G]]",
#   "sigma[p[s[G]]]",
#   "sigma[delta[G]]",
#   "sigma[R[0[G]]]",
#   "sigma[n[intro[G]]]",
#   "sigma[p[intro[G]]]",
#   "mu[GT]",
#   "sigma[GT]",
#   "mu[incub]",
#   "sigma[incub]"
# )

labels<- c(
  "delta",
  "size",
  "f",
  "n[intro]",
  "p[intro]",
  "R[0]",
  "n[cases]",
  "p[sus]",
  "n[A %<-% A]",
  "n[. %<-% A]",
  "n[groups]",
  "sum(n[cases])",
  "p[sus]",
  "sigma[peak]",
  "sigma[n[cases]]",
  "sigma[p[sus]]",
  "sigma[size]",
  "sigma[f]",
  "sigma[delta]",
  "sigma[R[0]]",
  "sigma[n[intro]]",
  "sigma[p[intro]]",
  "mu[GT]",
  "sigma[GT]",
  "mu[incub]",
  "sigma[incub]"
)

predictor_name_labels <-
  setNames(labels, predictors)

group_specific_predictors<- c(
  "delta",
  "size",
  "size_freq",
  "intro_n",
  "intro_prop",
  "r0",
  "n_cases",
  "group_susceptibles",
  "successes",
  "trials"
)

epidemic_specific_predictors <- c(
  "n_groups",
  "total_cases",
  "total_susceptibles",
  "sd_peak_date",
  "sd_n_cases",
  "sd_group_susceptibles",
  "sd_size",
  "sd_size_freq",
  "sd_delta",
  "sd_r0",
  "sd_intro_n",
  "sd_intro_prop",
  "GT_mean",
  "GT_sd",
  "INCUB_mean",
  "INCUB_sd")

p_master <- hexbin_plot(
  data = summary_long,
  group_specific_predictors = group_specific_predictors,
  epidemic_specific_predictors = epidemic_specific_predictors,
  predictor_name_labels = predictor_name_labels,
  alpha_value = "0.05",
  peak_coeff_value = "1",
  scatter_size = 0.35,
  meanpoint_size = 0.7,
  linerange_size = 0.25,
  linetrend_size = 0.25,
  base_size = 8
)


ggsave(
  here("analysis/simulation/plots", "hexbin_all.png"),
  p_master,
  width = 7.5,
  height = 4.3,
  units = "in",
  dpi = 500
)


# Short -------------------------------------------------------------------


predictors <-
  c(
    "delta",
    "size_freq",
    "r0",
    "n_cases",
    "n_groups",
    "sd_peak_date",
    "sd_n_cases",
    "sd_group_susceptibles",
    "sd_size",
    "sd_size_freq",
    "sd_delta",
    "sd_r0",
    "sd_intro_n",
    "sd_intro_prop",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd"
  )


labels<- c(
  "delta",
  "f",
  "R[0]",
  "n[cases]",
  "n[groups]",
  "sigma[peak]",
  "sigma[n[cases]]",
  "sigma[p[sus]]",
  "sigma[size]",
  "sigma[f]",
  "sigma[delta]",
  "sigma[R[0]]",
  "sigma[n[intro]]",
  "sigma[p[intro]]",
  "mu[GT]",
  "sigma[GT]",
  "mu[incub]",
  "sigma[incub]"
)

predictor_name_labels <-
  setNames(labels, predictors)

group_specific_predictors<- c(
  "delta",
  "size_freq",
  "r0",
  "n_cases"
)

epidemic_specific_predictors <- c(
  "n_groups",
  "sd_peak_date",
  "sd_n_cases",
  "sd_group_susceptibles",
  "sd_size",
  "sd_size_freq",
  "sd_delta",
  "sd_r0",
  "sd_intro_n",
  "sd_intro_prop",
  "GT_mean",
  "GT_sd",
  "INCUB_mean",
  "INCUB_sd")

p_master_short <- hexbin_plot(
  data = summary_long,
  group_specific_predictors = group_specific_predictors,
  epidemic_specific_predictors = epidemic_specific_predictors,
  predictor_name_labels = predictor_name_labels,
  alpha_value = "0.05",
  peak_coeff_value = "1",
  scatter_size = 0.35/1.5,
  meanpoint_size = 0.7/2,
  linerange_size = 0.25/1.5,
  linetrend_size = 0.25/1.5,
  base_size = 8/1.5
)



ggsave(
  here("analysis/simulation/plots", "hexbin_all_short.png"),
  p_master_short,
  width = 7.5,
  height = 4.3,
  units = "in",
  dpi = 500
)



rm(list = setdiff(ls(), ls_snapshot))
gc()


# Don't Touch -------------------------------------------------------------

# predictors <-
#   c(
#     "delta",
#     "size",
#     "size_freq",
#     "intro_n",
#     "intro_prop",
#     "r0",
#     "n_cases",
#     "group_susceptibles",
#     "successes",
#     "trials",
#     "n_groups",
#     "total_cases",
#     "total_susceptibles",
#     "sd_peak_date",
#     "sd_n_cases",
#     "sd_group_susceptibles",
#     "sd_size",
#     "sd_size_freq",
#     "sd_delta",
#     "sd_r0",
#     "sd_intro_n",
#     "sd_intro_prop",
#     "GT_mean",
#     "GT_sd",
#     "INCUB_mean",
#     "INCUB_sd"
#   )
#
# label <- c(
#   "delta[G]",
#   "s[G]",
#   "p[s[G]]",
#   "n[intro[G]]",
#   "p[intro[G]]",
#   "R[0[G]]",
#   "n[cases[G]]",
#   "p[susceptibles[G]]",
#   "n[G %<-% G]",
#   "n[. %<-% G]",
#   "n[groups]",
#   "sum(n[cases[G]])",
#   "p[susceptibles]",
#   "sigma[peak]",
#   "sigma[n[cases[G]]]",
#   "sigma[p[susceptibles[G]]]",
#   "sigma[s[G]]",
#   "sigma[p[s[G]]]",
#   "sigma[delta[G]]",
#   "sigma[R[0[G]]]",
#   "sigma[n[intro[G]]]",
#   "sigma[p[intro[G]]]",
#   "mu[GT]",
#   "sigma[GT]",
#   "mu[incub]",
#   "sigma[incub]"
# )



