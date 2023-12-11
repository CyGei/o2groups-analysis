group_specific_predictors <- c("delta", "size_freq", "n_cases")
epidemic_specific_predictors <- c("sd_peak_date")
group_specific_predictors
epidemic_specific_predictors


hex_df <- summary_long %>%
  filter(
    peak_coeff == 1 &
      predictor_name %in% c(group_specific_predictors, epidemic_specific_predictors)
  ) %>%
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

# subset data for smoothed lines
# smoothed_df <- hex_df %>%
#   filter(
#     (
#       outcome_name == "Bias" &
#         predictor_name %in% c("Size~Frequency", "Number~of~Cases")
#     ) |
#       (
#         outcome_name == "Coverage" &
#           predictor_name == "Peak~Date~Asynchronicity"
#       ) |
#       (outcome_name == "Sensitivity" & predictor_name == "delta") |
#       (outcome_name == "Specificity" &
#          predictor_name == "SD~Peak~Date")
#   )

p_hexbin_main <- ggplot(data = hex_df %>% filter(alpha == "0.05"),
                        aes(x = predictor_value, y = outcome_value)) +
  ggh4x::facet_nested(
    outcome_name ~ predictor_level + predictor_name,
    labeller = as_labeller(
      c(
        "bias" = "**1. Bias**",
        "coverage" = "**2. Coverage**",
        "sensitivity" = "**3. Sensitivity**",
        "specificity" = "**4. Specificity**",
        "delta" = "**A. &delta;**",
        "size_freq" = "**B. Size Frequency**",
        "n_cases" = "**C. No. of Cases**",
        "sd_peak_date" = "**D. &sigma; Peak Date**",
        "Group Specific" = "***Group Specific***",
        "Epidemic Specific" = "***Epidemic Specific***"
      )
    ),
    scales = "free"
  ) +
  geom_hex(aes(
    fill = log(after_stat(count)),
    colour = log(after_stat(count))
  ),
  #log(count) #ndensity
  linewidth = 0.1,
  bins = 100) +
  # geom_bin_2d(aes(fill = after_stat(log(count)),
  #                 colour = log(after_stat(count))),
  #             linewidth = 0.1,
  #             bins = 100) +
  geom_hline(
    data = hline_df,
    aes(yintercept = target),
    colour = "blue",
    #"#F31DC0",
    linewidth = 0.6,
    linetype = "longdash"
  ) +
  # geom_smooth(
  #   data = smoothed_df %>% filter(alpha == "0.05"),
  #   linewidth = 0.7,
  #   colour = "#24c8fb"
  # ) +
  # scale_fill_viridis_c("Log Count",
  #                      option = "plasma") +
  # scale_colour_viridis_c("Log Count",
  #                        option = "plasma") +
  scale_fill_gradientn("Log Count",
                       colours = c("#000000", "#ed2012", "#fae73d"),) +
  scale_colour_gradientn("Log Count",
                         colours = c("#000000", "#ed2012", "#fae73d")) +
  # scico::scale_fill_scico("Log Count", palette = "lajolla") +
  # scico::scale_colour_scico("Log Count", palette = "lajolla") +
  labs(
    x = "Predictor Value",
    y = "Metric Value",
    linetype = "",
    color = ""
  ) +
  theme_publication_hexbin(base_size = 12) +
  theme(
    strip.text = ggtext::element_markdown(),
    strip.text.y = ggtext::element_markdown()
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
hexbin_alpha_list <- purrr::map(
  .x = as.character(alpha_breaks),
  .f = function(alpha) {
    ggplot(data = hex_df %>% filter(alpha == alpha),
           aes(x = predictor_value, y = outcome_value)) +
      ggh4x::facet_nested(
        outcome_name ~ predictor_level + predictor_name,
        labeller = as_labeller(
          c(
            "bias" = "**1. Bias**",
            "coverage" = "**2. Coverage**",
            "sensitivity" = "**3. Sensitivity**",
            "specificity" = "**4. Specificity**",
            "delta" = "**A. &delta;**",
            "size_freq" = "**B. Size Frequency**",
            "n_cases" = "**C. No. of Cases**",
            "sd_peak_date" = "**D. &sigma; Peak Date**",
            "Group Specific" = "***Group Specific***",
            "Epidemic Specific" = "***Epidemic Specific***"
          )
        ),
        scales = "free"
      ) +
      geom_hex(aes(
        fill = after_stat(ndensity), #log(after_stat(count)),
        colour = after_stat(ndensity) #log(after_stat(count))
      ),
      linewidth = 0.1,
      bins = 100) +
      geom_hline(
        data = hline_df %>%
          mutate(target = ifelse(outcome_name == "coverage", 1 - as.numeric(alpha), target)),
        aes(yintercept = target),
        colour = "blue",
        linewidth = 0.6,
        linetype = "longdash"
      ) +
      scale_fill_gradientn("Log Count",
                           colours = c("#000000", "#ed2012", "#fae73d"),) +
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
        strip.text = ggtext::element_markdown(),
        strip.text.y = ggtext::element_markdown()
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
  patchwork::wrap_plots(hexbin_alpha_list, ncol = 1, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(10, "points"),
    plot.margin = unit(c(0, 0.35, 0.1, 0.1), 'lines')
  ) &
  patchwork::plot_annotation(tag_levels = 'A')


ggsave(
  here("analysis/simulation/plots", "hexbin_peak1_all_alpha.png"),
  p_hexbin_peak1_all_alpha,
  width = 8,
  height = 20,
  units = "in",
  dpi = 300
)



rm(list = setdiff(ls(), ls_snapshot))
gc()
