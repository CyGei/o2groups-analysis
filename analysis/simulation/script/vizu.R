#main figure#
# Load required libraries
library(rasterly)
library(cowplot)  # For arranging plots in a grid
library(patchwork)

# Function to create individual plots
univar_plot <- function(predictor, outcome) {
  p_raster <- rasterly::ggRasterly(
    data = outcomes_df %>% drop_na(all_of(c(predictor, outcome))),
    mapping = aes_string(x = predictor, y = outcome, color = "peak_coeff"),
    color = viridis_pal(alpha = 0.8, option = "plasma")(10)
  )
  p_raster <- p_raster +
    labs(title = NULL, x = NULL, y = NULL) +  # Remove axis labels and title
    theme(
      legend.position = "none",
      axis.text = element_blank(),  # Remove axis text
      axis.title = element_blank(), # Remove axis title
      strip.text = element_text(size = 10)  # Customize strip label text size
    ) +
    theme(plot.margin = margin(0, 0, 0, 0))  # Remove plot margin

  return(p_raster)
}

univar_plot("delta", "bias")

combinations <- expand.grid(predictor = predictors, outcome = metrics, stringsAsFactors = FALSE)
individual_plots <- pmap(combinations, univar_plot)

grid_plot <- plot_grid(
  plotlist = individual_plots,
  nrow = length(metrics),  # Number of rows in the grid
  ncol = length(predictors),  # Number of columns in the grid
  labels = "AUTO"
)

grid_plot

p_raster <- rasterly::ggRasterly(
  data = outcomes_long %>% drop_na(predictor_value, outcome_value),
  mapping = aes(x = predictor_value, y = outcome_value, color = peak_coeff),
  color = viridis_pal(alpha = 0.8, option = "plasma")(10)
)+
  facet_grid(outcome_name ~ predictor_name, scales = "free")
p_raster
