# Required packages
library(ggplot2)
library(gridExtra)
library(ggpubr)


# helpers -----------------------------------------------------------------

# Wrapping function that replaces underscores with line breaks
wrap_label <- function(x) {
  gsub("_", "\n", x)
}

# Count the number of facets in a ggplot object
count_facets <- function(ggplot_obj) {
  plot_data <- ggplot2::ggplot_build(ggplot_obj)
  num_facets <- max(as.integer(plot_data$layout$layout$PANEL))
  return(num_facets)
}

grid_theme <- list(
  ggplot2::theme(
    # editing legend
    legend.background = element_rect(
      fill = "#c0c0c0",
      linewidth = 0.5,
      linetype = "solid",
      colour = "black"
    ),
    # spacing
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = rep(unit(0, "null"), 4),
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.length = unit(0, "null"),
    axis.ticks.margin = unit(0, "null"),
    # editing strips
    strip.switch.pad.grid = unit(1, "cm"),
    strip.background.y = element_rect(fill = "#4C4E52", color = "#4C4E52"),
    strip.background.x = element_rect(fill = NA, color = "#4C4E52"),
    strip.text.y.left = element_text(
      margin = margin(1, 1, 1, 1, "cm"),
      angle = 0,
      color = "white",
      face = "bold"
    ),
    strip.text.x = element_text(
      angle = 0,
      color = "black",
      face = "italic",
      size = 10
    ),
    strip.placement = "outside",
    strip.clip = "off"
  ),
  ggplot2::labs(x = "", y = "")
)

grid_theme <- list(
  ggplot2::theme(
    # editing legend
    legend.background = element_rect(
      fill = "#c0c0c0",
      linewidth = 0.5,
      linetype = "solid",
      colour = "black"
    ),
    # spacing
    axis.text.x = element_text(),  # Show x-axis text
    axis.ticks.x = element_line(),  # Show x-axis ticks
    plot.margin = rep(unit(0, "null"), 4),
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.length = unit(0, "null"),
    axis.ticks.margin = unit(0, "null"),
    # editing strips
    strip.switch.pad.grid = unit(1, "cm"),
    strip.background.y = element_rect(fill = "#4C4E52", color = "#4C4E52"),
    strip.background.x = element_rect(fill = NA, color = "#4C4E52"),
    strip.text.y.left = element_text(
      margin = margin(1, 1, 1, 1, "cm"),
      angle = 0,
      color = "white",
      face = "bold"
    ),
    strip.text.x = element_text(
      angle = 0,
      color = "black",
      face = "italic",
      size = 10
    ),
    strip.placement = "outside",
    strip.clip = "off"
  ),
  ggplot2::labs(x = "", y = "")
)

# GGPLOTS ------------------------------------------------

# Est plot
plot_est_helper <- function(data) {
  ggplot(data, aes(x = group, y = est, col = peak_coeff)) +
    facet_grid(
      rows = vars(param),
      cols = vars(scenario),
      labeller = as_labeller(wrap_label),
      #scales = "free_x",
      switch = "y"
    ) +
    geom_point(position = position_dodge(0.4)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2,
                  position = position_dodge(0.4)) +
    geom_segment(
      aes(
        y = delta,
        xend = as.integer(as.factor(group)) + 0.5,
        yend = delta
      ),
      color = "black",
      position = position_dodge(0.9),
      alpha = 0.7,
      show.legend = FALSE
    ) +
    geom_segment(
      aes(
        y = delta,
        xend = as.integer(as.factor(group)) - 0.5,
        yend = delta
      ),
      color = "black",
      position = position_dodge(0.9),
      alpha = 0.7,
      show.legend = FALSE
    ) +
    geom_hline(aes(yintercept = 0), lty = "dotted") +
    scale_y_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1, 1)) +
    theme_bw() +
    grid_theme
}

#Bias plot
plot_bias_helper <- function(data) {
  ggplot(data, aes(x = group, y = bias, col = peak_coeff)) +
    aes(x = group,
        y = bias,
        color = peak_coeff) +
    facet_grid(
      rows = vars(param),
      cols = vars(scenario),
      labeller = as_labeller(wrap_label),
      #scales = "free_x",
      switch = "y"
    ) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper, ),
                  position = position_dodge(width = 0.4), ) +
    geom_point(position = position_dodge(width = 0.4)) +
    geom_hline(yintercept = 0,
               lty = "solid",
               color = "#3d3c3c") +
    scale_y_continuous(breaks = seq(-2, 2, 0.5), limits = c(-2, 2)) +
    theme_bw() +
    grid_theme

}


plot_coverage_helper <- function(data) {
  ggplot(data, aes(x = group, y = est, col = peak_coeff)) +
    facet_grid(
      rows = vars(param),
      cols = vars(scenario),
      labeller = as_labeller(wrap_label),
      #scales = "free_x",
      switch = "y"
    ) +
    geom_point(position = position_dodge(width = 0.4)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2,
                  position = position_dodge(width = 0.4)) +
    geom_hline(aes(yintercept = 0.95), lty = "dotted") +
    theme_bw() +
    grid_theme
}


plot_significance_helper <- function(data) {
  ggplot(data,
         aes(
    x = group,
    y = est,
    color = peak_coeff,
    shape = shape,
  )) +
    facet_grid(
      rows = vars(param),
      cols = vars(scenario),
      labeller = as_labeller(wrap_label),
      #scales = "free_x",
      switch = "y"
    ) +
    geom_point(size = 2,
               position = position_dodge(width = 0.4)) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper),
                  position = position_dodge(width = 0.4),
                  width = 0.2) +
    theme_bw() +
    grid_theme
}

# MAIN FUNCTION -----------------------------------------------------------


# Plot scenarios: data & plot type
plot_scenarios <- function(data, type) {
  data_list <- lapply(unique(data$param), function(param_val) {
    data_subset <- subset(data, param == param_val)
  })

  # Type
  if (type == "est") {
    p_list <- lapply(data_list, function(data) {
      plot_est_helper(data)
    })
  } else if (type == "bias") {
    p_list <- lapply(data_list, function(data) {
      plot_bias_helper(data)
    })
  } else if (type == "95coverage") {
    p_list <- lapply(data_list, function(data) {
      plot_coverage_helper(data)
    })
  } else if (type == "significance") {
    p_list <- lapply(data_list, function(data) {
      plot_significance_helper(data)
    })
  } else{
    stop("type is either 'est', 'bias', '95coverage' or 'significance'")
  }


  # Layout Matrix:
  num_facets <- sapply(p_list, count_facets)
  layout_matrix <-
    matrix(NA, nrow = length(p_list), ncol = max(num_facets))
  layout_matrix[, 1] <- seq_len(length(p_list))

  for (i in 1:nrow(layout_matrix)) {
    layout_matrix[i, 1:num_facets[i]] <- i
  }
  layout_matrix <-
    cbind(rep(NA, nrow(layout_matrix)), layout_matrix)
  layout_matrix[1,] <- c(1, 1, rep(NA, ncol(layout_matrix) - 2))

  # p_list no legend
  p_list_noL <-
    lapply(p_list, function(p)
      p + theme(legend.position = "none"))
  grobs_list <- lapply(p_list_noL, ggplotGrob)

  # Extract legend from the first plot in p_list:
  grobs_list[[length(grobs_list) + 1]] <- get_legend(p_list[[1]])

  # Edit the layout matrix to include the legend in the median row of the first column
  layout_matrix[median(1:round(nrow(layout_matrix))), 1] <- length(grobs_list)

  # Plot the grobs
  grid.arrange(
    grobs = grobs_list,
    layout_matrix = layout_matrix,
    left = NULL,
    right = "Parameter Change"
  )
}

# Usage example
# Assuming 'data' is the data frame you want to plot
#plot_scenarios(data = m1, type = "est")


