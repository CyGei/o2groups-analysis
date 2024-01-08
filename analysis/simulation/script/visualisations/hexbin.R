hexbin_plot <- function(
    data,
    group_specific_predictors, #character vector
    epidemic_specific_predictors,#character vector
    predictor_name_labels = NULL, #named character vector
    xbreaks, #list
    alpha_value,
    peak_coeff_value) {
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

  mean_df <- hex_df %>%
    mutate(
      predictor_value_chopped = case_when(
        predictor_name == "delta" ~
          santoku::chop(predictor_value, xbreaks$delta,
                        labels = santoku::lbl_midpoints()),

        predictor_name == "size_freq" ~
          santoku::chop(predictor_value, xbreaks$size_freq,
                        labels = santoku::lbl_midpoints()),

        predictor_name == "n_cases" ~
          santoku::chop_evenly(predictor_value, intervals = 10,
                               labels = santoku::lbl_midpoints()),

        predictor_name == "sd_peak_date" ~
          santoku::chop(predictor_value, xbreaks$sd_peak_date,
                        labels = santoku::lbl_midpoints())
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
    "Group Specific" = "<span style='color: #4C4E52'>***Group Specific***</span>",
    "Epidemic Specific" = "<span style='color: #4C4E52'>***Epidemic Specific***</span>"
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


  #PLOT
  p_hexbin <-
    hex_df %>%
    ggplot(aes(x = predictor_value, y = outcome_value)) +
    ggh4x::facet_nested(
      outcome_name ~ predictor_level + predictor_name,
      labeller = as_labeller(c(outcome_labels, predictor_level_labels, predictor_name_labels)),
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
          1 - as.numeric(alpha_value),
          target
        )),
      aes(yintercept = target),
      colour = "#0041f3",
        #"#03d075",
      #"#F31DC0",
      linewidth = 0.6,
      linetype = "longdash"
    ) +
    geom_pointrange(data = mean_df,
                    aes(ymin = lower,
                        ymax = upper),
                    col = "#f300bb", #"#B200F3",
                    size = 0.25) +
    geom_line(data = mean_df,
              linewidth = 0.5,
              col = "#f300bb") +
    scale_colour_gradientn("Density",
                         colours = c("#000000", "#ed2012", "#fae73d"),
                         breaks = seq(0,1,0.25),
                         limits = c(0,1))+
    scale_fill_gradientn("Density",
                         colours = c("#000000", "#ed2012", "#fae73d"),
                         breaks = seq(0,1,0.25),
                         limits = c(0,1))+
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

  return(p_hexbin)
}



# Plotting ----------------------------------------------------------------

predictor_name_labels <- c(
  "delta" = "**A. &delta;**",
  "size_freq" = "**B. Relative Size**",
  "n_cases" = "**C. No. of Cases**",
  "sd_peak_date" = "**D. &sigma; Peak Date**"
)

xbreaks <- list(
  delta = sort(c(0, seq(-1, 1, 0.1))),
  size_freq = seq(0, 1, 0.1),
  sd_peak_date = sort(c(1, seq(0, 20, 5)))
)

p_main <- hexbin_plot(
  data = summary_long,
  group_specific_predictors = c("delta", "size_freq", "n_cases"),
  epidemic_specific_predictors = c("sd_peak_date"),
  predictor_name_labels = predictor_name_labels,
  xbreaks = xbreaks,
  alpha_value = "0.05",
  peak_coeff_value = "1"
)
p_main

ggsave(
  here("analysis/simulation/plots", "hexbin.png"),
  p_main,
  width = 14,
  height = 8,
  units = "in",
  dpi = 300
)


# Alternative Params ------------------------------------------------------
p_list <- purrr::map2(
  .x = as.character(c(0.1, 0.25)),
  .y = c(1.5, 2),
  .f = function(alpha, peak_coeff) {
   p <- hexbin_plot(
      data = summary_long,
      group_specific_predictors = c("delta", "size_freq", "n_cases"),
      epidemic_specific_predictors = c("sd_peak_date"),
      predictor_name_labels = predictor_name_labels,
      xbreaks = xbreaks,
      alpha_value = alpha,
      peak_coeff_value = peak_coeff
    )
   return(p)
  })


# Create a patchwork object
p1 <- p_list[[1]] + theme(strip.text.y = element_blank(),
                          strip.background.y = element_blank(),
                          legend.position = "bottom") +
  ggtitle(lab = "Alpha = 0.1, Peak Coeff = 1.5")

p2 <- p_list[[2]] + theme(
  axis.text.y = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank(),
  legend.position = "bottom",
)+ ggtitle(lab = "Alpha = 0.25, Peak Coeff = 2")


p_patch <- ggpubr::ggarrange(p1+theme(legend.spacing.x = unit(10, "points"),
                           plot.margin = unit(c(0.1, 0.2, 0.1, 0.1), 'lines')),
                  p2+theme(legend.spacing.x = unit(10, "points"),
                           plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines')),
                  ncol=2,
                  common.legend = TRUE,
                  legend="bottom",
                  labels = c("A.", "B"))+
  theme(legend.spacing.x = unit(10, "points")
        #plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines')
        ) #t, r, b, l

ggsave(
  here("analysis/simulation/plots", "hexbin_patch.png"),
  p_patch,
  width = 18,
  height = 8,
  units = "in",
  dpi = 300
)




rm(list = setdiff(ls(), ls_snapshot))
gc()



