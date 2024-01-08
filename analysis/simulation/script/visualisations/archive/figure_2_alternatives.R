group_specific_predictors <- c("delta", "size_freq", "n_cases")
epidemic_specific_predictors <- c("sd_peak_date")

patch_df <- summary_long %>%
  filter(predictor_name %in% c(group_specific_predictors, epidemic_specific_predictors)) %>%
  mutate(
    alpha = as.character(alpha),
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
      levels = c("delta", "size_freq", "n_cases", "sd_peak_date")
    ),
  ) %>%
  select(-all_of(c("name", "scenario")))


selected_alpha <- "0.05"
selected_peak_coeff <- 1
x_breaks_list <- list(
  delta = sort(c(0, seq(-1, 1, 0.1))),
  size_freq = seq(0, 1, 0.1),
  n_cases = seq(0, 100, 10),
  sd_peak_date = seq(0, 10, 1)
)

everyother <- function(x)
  x[seq_along(x) %% 2 == 0]
rounder <- function(x) {
  round(x, 2)
}
lbl_mid <-  santoku::lbl_midpoints(fmt = rounder)

patch_df_main <- patch_df %>%
  filter(alpha == selected_alpha &
           peak_coeff == selected_peak_coeff) %>%
  mutate(
    predictor_value_chopped = case_when(
      predictor_name == "delta" ~
        santoku::chop(predictor_value, x_breaks_list$delta,
                      labels = lbl_mid),

      predictor_name == "size_freq" ~
        santoku::chop(predictor_value, x_breaks_list$size_freq, labels = lbl_mid),

      predictor_name == "n_cases" ~
        santoku::chop(predictor_value, x_breaks_list$n_cases, labels = lbl_mid),

      predictor_name == "sd_peak_date" ~
        santoku::chop(predictor_value, x_breaks_list$sd_peak_date, labels = lbl_mid)
    )
  )

mean_df <- patch_df_main %>%
  group_by(outcome_name,
           predictor_level,
           predictor_name,
           predictor_value_chopped) %>%
  summarise(
    sd = sd(outcome_value, na.rm = TRUE),
    lower = quantile(outcome_value, 0.25, na.rm = TRUE),
    upper = quantile(outcome_value, 0.75, na.rm = TRUE),
    outcome_value = mean(outcome_value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(outcome_name, predictor_level, predictor_name) %>%
  mutate(freq = n / sum(n))

p_main <-
  patch_df_main %>%
  ggplot(aes(x = predictor_value_chopped, y = outcome_value)) +
  ggh4x::facet_nested(
    outcome_name ~ predictor_level + predictor_name,
    labeller = as_labeller(
      c(
        "bias" = "**1. Bias**",
        "coverage" = "**2. Coverage**",
        "sensitivity" = "**3. Sensitivity**",
        "specificity" = "**4. Specificity**",
        "delta" = "**A. &delta;**",
        "size_freq" = "**B. Relative Group Size**",
        "n_cases" = "**C. No. of Cases**",
        "sd_peak_date" = "**D. &sigma; Peak Date**",
        "Group Specific" = "<span style='color: #4C4E52'>***Group Specific***</span>",
        "Epidemic Specific" = "<span style='color: #4C4E52'>***Epidemic Specific***</span>"
      )
    ),
    scales = "free"
  ) +
  geom_violin(
    fill = "grey", col = NA,
    #aes(fill = after_stat(ndensity))
    ) +
  geom_hline(
    data = hline_df %>%
      mutate(target = ifelse(
        outcome_name == "coverage",
        1 - as.numeric(selected_alpha),
        target
      )),
    aes(yintercept = target),
    colour = "black",
    linewidth = 0.6,
    linetype = "longdash"
  ) +
  geom_pointrange(
    data = mean_df,
    aes(ymin = lower,
        ymax = upper),
    lty = "solid",
    size = 0.1,
    col = "black"#"#03d075"
  ) +
  geom_line(
    data = mean_df,
    group = 1,
    size = 0.5,
    col = "black"
  ) +
  #scale_fill_viridis_c(option = "plasma") +
  theme_publication(base_size = 9) +
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 1),
                                                  breaks = seq(-2, 2, 0.25)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0.5, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0.25, 1))
    ),
    x = list(
      predictor_name == "delta" ~   scale_x_discrete(
        breaks = c(seq(-0.95, 0, 0.2), 0, seq(0.15, 0.95, 0.2)),
        #everyother
        labels = ifelse(
          b == -0.95,
          "-1&larr;",
          ifelse(b == 0.95, "&rarr;1", as.character(b))
        ) #everyother
      )
    )
  ) +
  theme(
    strip.text.x = ggtext::element_markdown(size = rel(1.25)),
    strip.text.y = ggtext::element_markdown(size = rel(1.1)),
    axis.text.x = ggtext::element_markdown(),
    #angle = 45, hjust = 1
  ) +
  # guides(
  #   colour = guide_colorbar(
  #     title.position = "top",
  #     title.hjust = 0.5,
  #     label.position = "bottom",
  #     barwidth = 10,
  #     nrow = 1,
  #     byrow = TRUE
  #   ),
  #   fill = guide_colorbar(
  #     title.position = "top",
  #     title.hjust = 0.5,
  #     label.position = "bottom",
  #     barwidth = 10,
  #     nrow = 1,
  #     byrow = TRUE
  #   )
  # ) +
  labs(
    x = "Predictor Value",
    y = "Metric Value",
    fill = "density"
  )

p_main


ggsave(
  here("analysis/simulation/plots", "line_main.png"),
  p_main,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)




# Function ----------------------------------------------------------------



# Alpha -------------------------------------------------------------------

p_alpha_list <- purrr::map2(
  .x = as.character(c(0.1, 0.25)),
  .y = c(1.5, 2),
  .f = function(alpha, peak_coeff) {
    patch_df_main <- patch_df %>%
      filter(alpha == alpha &
               peak_coeff == peak_coeff) %>%
      mutate(
        predictor_value_chopped = case_when(
          predictor_name == "delta" ~
            santoku::chop(predictor_value, x_breaks_list$delta,
                          labels = lbl_mid),

          predictor_name == "size_freq" ~
            santoku::chop(predictor_value, x_breaks_list$size_freq, labels = lbl_mid),

          predictor_name == "n_cases" ~
            santoku::chop(predictor_value, x_breaks_list$n_cases, labels = lbl_mid),

          predictor_name == "sd_peak_date" ~
            santoku::chop(predictor_value, x_breaks_list$sd_peak_date, labels = lbl_mid)
        )
      )

    mean_df <- patch_df_main %>%
      group_by(outcome_name,
               predictor_level,
               predictor_name,
               predictor_value_chopped) %>%
      summarise(
        sd = sd(outcome_value, na.rm = TRUE),
        lower = quantile(outcome_value, 0.25, na.rm = TRUE),
        upper = quantile(outcome_value, 0.75, na.rm = TRUE),
        outcome_value = mean(outcome_value, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      ungroup() %>%
      group_by(outcome_name, predictor_level, predictor_name) %>%
      mutate(freq = n / sum(n))

    p_main <-
      patch_df_main %>%
      ggplot(aes(x = predictor_value_chopped, y = outcome_value)) +
      ggh4x::facet_nested(
        outcome_name ~ predictor_level + predictor_name,
        labeller = as_labeller(
          c(
            "bias" = "**1. Bias**",
            "coverage" = "**2. Coverage**",
            "sensitivity" = "**3. Sensitivity**",
            "specificity" = "**4. Specificity**",
            "delta" = "**A. &delta;**",
            "size_freq" = "**B. Relative Group Size**",
            "n_cases" = "**C. No. of Cases**",
            "sd_peak_date" = "**D. &sigma; Peak Date**",
            "Group Specific" = "<span style='color: #4C4E52'>***Group Specific***</span>",
            "Epidemic Specific" = "<span style='color: #4C4E52'>***Epidemic Specific***</span>"
          )
        ),
        scales = "free"
      ) +
      geom_violin(aes(fill = after_stat(ndensity)),
                  col = NA) +
      geom_hline(
        data = hline_df %>%
          mutate(target = ifelse(
            outcome_name == "coverage",
            1 - as.numeric(alpha),
            target
          )),
        aes(yintercept = target),
        colour = "black",
        linewidth = 0.6,
        linetype = "longdash"
      ) +
      geom_pointrange(
        data = mean_df,
        aes(ymin = lower,
            ymax = upper),
        lty = "solid",
        size = 0.1,
        col = "#03d075"
      ) +
      geom_line(
        data = mean_df,
        group = 1,
        size = 0.5,
        col = "#03d075"
      ) +
      scale_fill_viridis_c(option = "plasma") +
      theme_publication(base_size = 9) +
      ggh4x::facetted_pos_scales(
        y = list(
          outcome_name == "bias" ~ scale_y_continuous(limits = c(-2, 2),
                                                      breaks = seq(-2, 2, 0.5)),
          outcome_name == "coverage" ~ scale_y_continuous(limits = c(0.25, 1)),
          outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
          outcome_name == "specificity" ~ scale_y_continuous(limits = c(0.25, 1))
        ),
        x = list(
          predictor_name == "delta" ~   scale_x_discrete(
            breaks = c(seq(-0.95, 0, 0.2), 0, seq(0.15, 0.95, 0.2)),
            #everyother
            labels = ifelse(
              b == -0.95,
              "-1&larr;",
              ifelse(b == 0.95, "&rarr;1", as.character(b))
            ) #everyother
          )
        )
      ) +
      theme(
        strip.text.x = ggtext::element_markdown(size = rel(1.25)),
        strip.text.y = ggtext::element_markdown(size = rel(1.1)),
        axis.text.x = ggtext::element_markdown(),
        #angle = 45, hjust = 1
      ) +
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
      ) +
      labs(
        x = "Predictor Value",
        y = "Metric Value",
        fill = "density",
        linetype = "",
        color = ""
      )
  }
)
p_patch2 <-
  patchwork::wrap_plots(
    (
      p_alpha_list[[1]] +
        theme(strip.background.y = element_blank(),
              strip.text.y = element_blank())
    ),
    p_alpha_list[[2]] + labs(y = ""),
    ncol = 2,
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(10, "points"),
    plot.margin = unit(c(0, 0.35, 0.1, 0.1), 'lines')
  ) &
  patchwork::plot_annotation(tag_levels = 'A')

ggsave(
  here("analysis/simulation/plots", "patch2.png"),
  plot = p_patch2,
  width = 25,
  height = 15,
  units = "in",
  dpi = 300
)

rm(list = setdiff(ls(), ls_snapshot))
gc()

