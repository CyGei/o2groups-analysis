library(ggplot2)
library(gridExtra)

# Wrapping function that replaces underscores with linebreaks
wrap_label <- function(x) {
  gsub("_", "\n", x)
}

# Create individual plots for each scenario
p_list <- lapply(unique(m1$param), function(param_val) {
  m1_subset <- subset(m1, param == param_val)
  ggplot(m1_subset, aes(x = group, y = mean_est, col = peak_coeff)) +
    facet_grid(
      rows = vars(param),
      cols = vars(scenario),
      labeller = as_labeller(wrap_label),
      scales = "free_x"
    ) +
    geom_point(position = position_dodge(0.4)) +
    geom_errorbar(
      aes(ymin = mean_lower_ci, ymax = mean_upper_ci),
      width = 0.2,
      position = position_dodge(0.4)
    ) +
    geom_segment(
      aes(
        y = mean_delta,
        xend = as.integer(as.factor(group)) + 0.5,
        yend = mean_delta
      ),
      color = "black",
      position = position_dodge(0.9),
      alpha = 0.7,
      show.legend = FALSE
    ) +
    geom_segment(
      aes(
        y = mean_delta,
        xend = as.integer(as.factor(group)) - 0.5,
        yend = mean_delta
      ),
      color = "black",
      position = position_dodge(0.9),
      alpha = 0.7,
      show.legend = FALSE
    ) +
    geom_hline(aes(yintercept = 0), lty = "dotted") +

    scale_y_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1, 1)) +
    theme_bw() +

    theme(
      # legend style
     # legend.position = "bottom",
      legend.background = element_rect(
        fill = "#c0c0c0",
        linewidth = 0.5,
        linetype = "solid",
        colour = "black"
      ),
      #removing space between panels
      axis.text.x =  element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = rep(unit(0, "null"), 4),
      panel.spacing = unit(0.5, "lines"),
      axis.ticks.length = unit(0, "null"),
      axis.ticks.margin = unit(0, "null"),

      #editing strips
      strip.background.y =  element_rect(fill = "#4C4E52", color = "#4C4E52"),
      #element_blank(),
      strip.background.x = element_rect(fill = NA, color = "#4C4E52"),
      strip.text.y = element_text(
        angle = 0,
        color = "white",
        face = "bold",
        size = 10
      ),
      strip.text.x = element_text(
        angle = 0,
        color = "black",
        face = "italic",
        size = 10
      ),
      strip.placement = 'outside',
      strip.clip = "off"
    ) +
    labs(x = "", y = "")
})
p_list[[2]]

# Count the number of facets in a ggplot object
count_facets <- function(ggplot_obj) {
  plot_data <- ggplot2::ggplot_build(ggplot_obj)
  num_facets <- max(as.integer(plot_data$layout$layout$PANEL))
  return(num_facets)
}
num_facets <- sapply(p_list, count_facets)

# Produce initial layout matrix
layout_matrix <-
  matrix(NA, nrow = length(p_list), ncol = max(num_facets))
layout_matrix[, 1] <- seq_len(length(p_list))
for (i in 1:nrow(layout_matrix)) {
  layout_matrix[i, 1:num_facets[i]] <- i
}

layout_matrix <- cbind(rep(NA, nrow(layout_matrix)), layout_matrix)
layout_matrix[1, ] <- c(1,1, rep(NA, ncol(layout_matrix) - 2))

# p_list no legend
p_list_noL <- lapply(p_list, function(p) p + theme(legend.position = "none"))
grobs_list <- lapply(p_list_noL, ggplotGrob)

# Extract legend from the first plot in p_list:
grobs_list[[length(grobs_list)+1]] <- ggpubr::get_legend(p_list[[1]])

# Edit the layout matrix to include the legend in the 2nd row of the first column
layout_matrix[2, 1] <- length(grobs_list)

# Plot the grobs
grid.arrange(grobs = grobs_list,
layout_matrix = layout_matrix,
left = "Scaled Delta Estimate",
right = "Parameter Change")


