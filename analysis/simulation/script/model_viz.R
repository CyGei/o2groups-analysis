library(tidyverse)
library(here)
library(data.table)

# Helpers -----------------------------------------------------------------
source(here("analysis/simulation/script/plot_helpers.R"))

# Data --------------------------------------------------------------------
scenarios_df <-
  fread(here("analysis/simulation/data/model", "scenarios_df.csv"))
# model_df is the most granular data on simulation level
model_df <-
  fread(here("analysis/simulation/data/model", "model_df.csv"))
#outcomes_df summarises across all simulations
outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

predictors <-
  c("delta",
    "n_groups",
    "size",
    "intro_n",
    "r0",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd",
    "trials")
metrics <- c("coverage", "bias", "significance")

# Pivot the dataframe into long format.
# note for significance:
# significance should always be closest to 1
# When delta is 0 we do 1 - significance
outcomes_long <- outcomes_df %>%
  mutate(significance = ifelse(delta == 0, 1 - significance, significance)) %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value")

mean_line <- outcomes_long %>%
  group_by(peak_coeff, predictor_name, outcome_name) %>%
  mutate(bin = cut(predictor_value, breaks = 100)) %>%
  group_by(peak_coeff, predictor_name, outcome_name, bin) %>%
  summarise(mean_outcome_value = mean(outcome_value, na.rm = TRUE),
            mid = extract_and_calculate_midpoint(bin)
            # lowerQuantile = quantile(outcome_value, 0.25, na.rm = TRUE),
            # upperQuantile = quantile(outcome_value, 0.75, na.rm = TRUE)
  )

# PLOTS -------------------------------------------------------------------
#https://github.com/plotly/rasterly

# # scatter plot slow
# outcomes_long %>%
#   filter(predictor_name %in% c("delta", "size", "r0", "GT_mean", "trials")) %>%
#   ggplot() +
#   geom_point(aes(x = predictor_value ,
#                  y = outcome_value,
#                  col = peak_coeff),
#              alpha = 0.2) +
#   geom_line(
#     data = mean_line %>%  filter(
#       predictor_name %in% c("delta", "size", "r0", "GT_mean", "trials")
#     ),
#     aes(y = mean_outcome_value,
#         x = mid,
#         col = peak_coeff,
#         group = factor(peak_coeff))
#   ) +
#   facet_grid(outcome_name ~ predictor_name , scales = "free") +
#   scale_color_viridis_b() +
#   theme_classic() +
#   theme(legend.position = "bottom")



# If peak_coeff is to be treated as a numeric variable, and you want the average value of peak_coeff
# within each bin to determine the fill color, then you need to manually hexbin the data:



cell_df <- do.call('rbind',
                   split(
                     outcomes_long,
                     interaction(outcomes_long$outcome_name, outcomes_long$predictor_name)
                   ) |>
                     lapply(function(d) {
                       hb <- hexbin::hexbin(d$predictor_value,
                                            d$outcome_value,
                                            xbins = 100,
                                            IDs = TRUE)
                       cbind(
                         aggregate(d$peak_coeff, by = list(hb@cID), FUN = mean),
                         count = hb@count,
                         X = hexbin::hcell2xy(hb)$x,
                         Y = hexbin::hcell2xy(hb)$y,
                         outcome_name = d$outcome_name[1],
                         predictor_name = d$predictor_name[1]
                       )
                     }))


p_hexbin <- cell_df %>%
  ggplot() +
  facet_grid(outcome_name ~ predictor_name, scales = "free") +
  geom_hex(aes(
    x = X ,
    y = Y,
    fill = x,
    alpha = count
  ),
  stat = 'identity') +
  geom_line(data = mean_line,
            aes(
              y = mean_outcome_value,
              x = mid,
              col = peak_coeff,
              group = factor(peak_coeff)
            )) +
  scale_fill_viridis_c(option = "inferno") +
  scale_color_viridis_c(option = "inferno",
                        guide = "none") +
  scale_alpha_continuous(range = c(0.8, 1), guide = "none") +
  theme_classic() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "#d3d3d3"),
        strip.text.x = element_text(
          size = 12, family = "serif"
        ),
        strip.text.y = element_text(
          size = 12, family = "serif"
        )) +
  labs(
    title = "Relationship between predictors and outcomes",
    fill = "Peak Coefficient",
    x = "predictor value",
    y = "outcome value",
    caption = "Color: Represents the mean value of the peak coefficient within each hexagonal bin.
Transparency: Indicates data density, with more transparent bins having fewer data points.
       Line: average estimate"
  )

ggsave(here("analysis/simulation/plots", "hexbin.svg"),
       plot = p_hexbin,
       width = 16,
       height = 8,
       units = "in",
       dpi = 300)
#the plot represents a grid of hexagonal bins, where each bin's color (fill) is determined by the mean value of the factor variable within that bin. The transparency (alpha) of each bin is based on the count of data points within the bin, with more transparent bins representing areas with fewer data points.








formula <- y~x
plot_scatter(model_df,
             x = "r0",
             y = "bias")+
  stat_smooth(method = "lm") +
  ggpubr::stat_regline_equation(
    aes(label =  paste(..eq.label.., "   ", ..adj.rr.label.., sep = "~")),
    col = "steelblue")

formula <- y ~ poly(x, 2, raw = TRUE)
plot_scatter(model_df,
             x = "size",
             y = "bias",
             facet_vars = "peak_coeff")+
  stat_smooth(method = "lm", formula = formula) +
  ggpubr::stat_regline_equation(
    aes(label =  paste(..eq.label.., "   ", ..adj.rr.label.., sep = "~")),
    formula = formula,
    col = "steelblue")


plot_heatmap(
  outcomes_df,
  "delta",
  "bias",
  x_breaks = seq(-1, 1, 0.1),
  y_breaks = seq(-2, 2, 0.1),
  bin = FALSE,
  min_alpha = 0.8
)




# Correlations ------------------------------------------------------------

corr <- outcomes %>%
  select(!c(scenario, name, peak_coeff)) %>%
  drop_na() %>%
  cor()

p.mat <- outcomes_df %>%
  select(!c(scenario, name, peak_coeff)) %>%
  drop_na() %>%
  ggcorrplot::cor_pmat()

ggcorrplot::ggcorrplot(
  corr = corr,
  type = "lower",
  lab = TRUE,
  hc.order = TRUE,
  p.mat = p.mat,
  method = "circle"
)

corrplot::corrplot(
  corr,
  type = "upper",
  order = "hclust",
  col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu")
)
