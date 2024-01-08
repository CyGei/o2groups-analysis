



# Inputs ---------------------------------------------------------------
group_specific_predictors <- c("delta", "size_freq", "n_cases")
epidemic_specific_predictors <- c("sd_peak_date")
x_breaks_list <- list(
  delta = sort(c(0, seq(-1, 1, 0.1))),
  size_freq = seq(0, 1, 0.1),
  n_cases = seq(0, 100, 10),
  sd_peak_date = sort(c(1, seq(0, 20, 5)))
)
selected_alpha <- "0.05"
selected_peak_coeff <- 1


# DATA --------------------------------------------------------------------

hex_df <- summary_long %>%
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


lbl_mid <-  santoku::lbl_midpoints(fmt = rounder)

mean_df <- hex_df %>%
  mutate(
    predictor_value_chopped = case_when(
      predictor_name == "delta" ~
        santoku::chop(predictor_value, x_breaks_list$delta,
                      labels = lbl_mid),

      predictor_name == "size_freq" ~
        santoku::chop(predictor_value, x_breaks_list$size_freq, labels = lbl_mid),

      predictor_name == "n_cases" ~
        santoku::chop_evenly(predictor_value, intervals = 10, labels = lbl_mid),

      predictor_name == "sd_peak_date" ~
        santoku::chop(predictor_value, x_breaks_list$sd_peak_date, labels = lbl_mid)
    )
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
  mutate(freq = n / sum(n))


# PLOT --------------------------------------------------------------------


selected_hex_df <- hex_df %>%
  filter(alpha == selected_alpha &
           peak_coeff == selected_peak_coeff)
selected_mean_df <- mean_df %>%
  filter(alpha == selected_alpha &
           peak_coeff == selected_peak_coeff) %>%
  mutate(predictor_value = as.numeric(as.character(predictor_value_chopped)))

p_hexbin_main <-
  selected_hex_df %>%
  ggplot(aes(x = predictor_value, y = outcome_value)) +
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
  geom_hex(aes(fill = after_stat(ndensity),
               colour = after_stat(ndensity)),
           linewidth = 0.1,
           bins = 100) +
  geom_hline(
    data = hline_df %>%
      mutate(target = ifelse(
        outcome_name == "coverage",
        1 - as.numeric(selected_alpha),
        target
      )),
    aes(yintercept = target),
    colour = "#03d075",
    #"#F31DC0",
    linewidth = 0.6,
    linetype = "longdash"
  ) +
  geom_pointrange(data = selected_mean_df,
                  aes(ymin = lower,
                      ymax = upper),
                  col = "#B200F3",
                  size = 0.1) +
  geom_line(data = selected_mean_df,
            linewidth = 0.6,
            col = "#B200F3",
            size = 0.1) +
  scale_colour_gradientn("Density",
                         colours = c("#000000", "#ed2012", "#fae73d")) +
  scale_fill_gradientn("Density",
                       colours = c("#000000", "#ed2012", "#fae73d"), ) +
  scale_colour_gradientn("Density",
                         colours = c("#000000", "#ed2012", "#fae73d")) +
  #scico::scale_fill_scico("Log Count", palette = "lajolla") +
  #scico::scale_colour_scico("Log Count", palette = "lajolla") +
  labs(
    x = "Predictor Value",
    y = "Metric Value",
    linetype = "",
    color = ""
  ) +
  theme_publication_hexbin(base_size = 12) +
  theme(
    strip.text.x = ggtext::element_markdown(size = rel(1.25)),
    strip.text.y = ggtext::element_markdown(size = rel(1.1))
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

p_hexbin_main

ggsave(
  here("analysis/simulation/plots", "hexbin_main.png"),
  p_hexbin_main,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)


# plot for each alpha
hexbin_alpha_list <- purrr::map2(
  .x = as.character(c(0.05, 0.1, 0.25)),
  .y = c(1, 1.5, 2),
  .f = function(alpha, peak_coeff) {
    data = hex_df %>%
      filter(alpha == alpha & peak_coeff == peak_coeff) %>%
      ggplot(aes(x = predictor_value, y = outcome_value)) +
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
      geom_hex(
        aes(fill = after_stat(ndensity),
            colour = after_stat(ndensity)),
        linewidth = 0.1,
        bins = 100
      ) +
      geom_hline(
        data = hline_df %>%
          mutate(target = ifelse(
            outcome_name == "coverage", 1 - as.numeric(alpha), target
          )),
        aes(yintercept = target),
        colour = "#14B508",
        linewidth = 0.6,
        linetype = "longdash"
      ) +
      scale_fill_gradientn("Log Count",
                           colours = c("#000000", "#ed2012", "#fae73d")) +
      scale_colour_gradientn("Log Count",
                             colours = c("#000000", "#ed2012", "#fae73d")) +
      labs(
        x = "Predictor Value",
        y = "Metric Value",
        linetype = "",
        color = ""
      ) +
      theme_publication_hexbin(base_size = 9) +
      theme(
        strip.text.x = ggtext::element_markdown(size = rel(1.25)),
        strip.text.y = ggtext::element_markdown(size = rel(1.1))
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
  }
)


p_hexbin_peak1_all_alpha <-
  patchwork::wrap_plots(hexbin_alpha_list,
                        ncol = 1,
                        guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(10, "points"),
    plot.margin = unit(c(0, 0.35, 0.1, 0.1), 'lines')
  ) &
  patchwork::plot_annotation(tag_levels = 'A')


ggsave(
  here("analysis/simulation/plots", "hexbin_peak1_all_alpha.png"),
  p_hexbin_peak1_all_alpha,
  width = 16,
  height = 25,
  units = "in",
  dpi = 300
)



rm(list = setdiff(ls(), ls_snapshot))
gc()
