library(tidyverse)
library(rasterly)
library(ggtext)
library(scales)

raster_plot <- function(predictor, outcome, outcomes_df) {
  cat(
    "Predictor: ", predictor, "\n",
    "Outcome: ", outcome, "\n"
  )

  hex1 <- "#0096FF"
  hex2 <- "black"
  hex3 <- "#ff007f"
  pal <- gradient_n_pal(
    colours = c(hex1, hex2, hex3),
    values = c(0.7, 1, 1.6)
  )
  show_col(pal(seq(0.7,1.6, 0.1)))

  df <- outcomes_df %>%
    select(all_of(c(predictor, outcome, "peak_coeff"))) %>%
    drop_na()


  if(outcome == "bias"){
    y_range <- c(-2, 2)
  } else {
    y_range <- c(0, 1)
  }
  x_min <- floor(min(df[[predictor]]))
  x_max <- max(df[[predictor]])*1.1
  x_range <- c(x_min, x_max)


  # Create the raster plot
  p <- ggRasterly(
    data = df,
    mapping = aes(
      x = .data[[predictor]],
      y = .data[[outcome]],
      color = peak_coeff
    ),
    color = pal(seq(0.7, 1.6, 0.1)),
    x_range = x_range,
    y_range = y_range
  )

  # Add smooth line
  p <- p +
    geom_smooth(
      data = df,
      aes(
        x = .data[[predictor]],
        y = .data[[outcome]],
        color = as.factor(peak_coeff),
        group = as.factor(peak_coeff)
      ),
      se = FALSE,
      method = "gam",
      formula = y ~ s(x, bs = "cs", k = 7),
      linewidth = 0.5,
      alpha = 1
    )

  # Customize the theme
  p <- p +
    scale_x_continuous(
      position = "top",
      sec.axis = dup_axis()
    ) +
    theme_bw() +
    labs(x = predictor, y = outcome) +
    theme(
      axis.text.x.top = element_blank(),
      axis.ticks.x.top = element_blank(),
      axis.title.x.bottom = element_blank(),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt"),
      # legend.position = "none",
      axis.title.x.top = ggtext::element_textbox(
        linetype = 1,
        r = unit(10, "pt"),
        padding = margin(5, 10, 5, 10),
        family = "Palatino",
        face = "bold.italic",
        size = 10
      )
    )

  return(p)
}


outcomes_df <- readRDS(here::here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
  rename(
    "Number of Groups" = n_groups,
    "Group Size" = size,
    "R0" = r0,
    "Proportion of Introductions" = intro_prop,
    "Within Group Transmissions" = successes,
    "Total Group Transmissions" = trials,
    "Within Group Transmission Rate" = beta
  )


predictors <- c(
  "delta",
  "Number of Groups",
  "Group Size",
  "Proportion of Introductions",
  "R0",
  "GT_mean",
  "GT_sd",
  "INCUB_mean",
  "INCUB_sd",
  "Within Group Transmissions",
  "Total Group Transmissions",
  "Within Group Transmission Rate"
)

outcomes <- c(
  "bias",
  "coverage",
  "sensitivity"
 # "specificity"
)

#######
# Bias
#######

p_bias <- lapply(predictors, function(predictor) {
  raster_plot(predictor, "bias", outcomes_df)
})

p_bias[[1]] <- p_bias[[1]] +
  theme(
    axis.title.y = ggtext::element_textbox(
    linetype = 1,
    r = unit(10, "pt"),
    padding = margin(5, 10, 5, 10),
    orientation = "left-rotated",
    family = "Palatino",
    face = "bold.italic",
    size = 10
  ))

p_bias[2:length(p_bias)] <- map(p_bias[2:length(p_bias)], function(p){
  p +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title.y = element_blank())
})
wrap_plots(p_bias, nrow = 1, guides = "collect")


p_coverage <- lapply(predictors, function(predictor) {
  raster_plot(predictor, "coverage", outcomes_df)
})
p_coverage[[1]] <- p_coverage[[1]] +
  theme(
    axis.title.y = ggtext::element_textbox(
      linetype = 1,
      r = unit(10, "pt"),
      padding = margin(5, 10, 5, 10),
      orientation = "left-rotated",
      family = "Palatino",
      face = "bold.italic",
      size = 10
    ))

p_coverage[2:length(p_coverage)] <- map(p_coverage[2:length(p_coverage)], function(p){
  p +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title.y = element_blank())
})




p_sensitivity <- lapply(predictors, function(predictor) {
  raster_plot(predictor, "sensitivity", outcomes_df)
})

#crashes probably due to only 0 values in x
# p_specificity <- lapply(predictors, function(predictor) {
#   raster_plot(predictor, "specificity", outcomes_df)
# })

plot_list <- list()
for (outcome in outcomes) {
  for (predictor in predictors) {
    plot_list[[paste0(outcome, "_" , predictor)]] <-
      raster_plot(predictor, outcome, outcomes_df)
  }
}



library(patchwork)
patch <- wrap_plots(plot_list, nrow = 3, guides = "collect")

ggsave(
  here::here("analysis/simulation/plots", "patch.png"),
  plot = patch,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)
