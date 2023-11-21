library(tidyverse)
library(here)
# Helpers -----------------------------------------------------------------
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))


# Variables ---------------------------------------------------------------
predictors <-
  c(
    "delta",
    # "size",
    "size_freq",
    # "intro_n",
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

label <- c(
  "delta[G]",
  # "s[G]",
  "p[s[G]]",
  # "n[intro[G]]",
  "p[intro[G]]",
  "R[0[G]]",
  "n[cases[G]]",
  "p[susceptibles[G]]",
  "n[G %<-% G]",
  "n[. %<-% G]",
  "n[groups]",
  "sum(n[cases[G]])",
  "p[susceptibles]",
  "sigma[peak]",
  "sigma[n[cases[G]]]",
  "sigma[p[susceptibles[G]]]",
  "sigma[s[G]]",
  "sigma[p[s[G]]]",
  "sigma[delta[G]]",
  "sigma[R[0[G]]]",
  "sigma[n[intro[G]]]",
  "sigma[p[intro[G]]]",
  "mu[GT]",
  "sigma[GT]",
  "mu[incub]",
  "sigma[incub]"
)

metrics <-
  c("sensitivity",
    "specificity",
    "bias",
    "coverage")
extra_metrics <- c("ppv", "npv")
id_vars <- c("scenario", "peak_coeff", "name")
peak_coeffs_breaks <- sort(c(1, seq(0.5, 1.5, 0.2)))

group_level_predictors <- predictors[1:8]
epidemic_level_predictors <- predictors[9:length(predictors)]


# Data --------------------------------------------------------------------

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
  select(-c(peak_date,size,intro_n, all_of(extra_metrics)))

outcomes_long <- outcomes_df %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value") %>%
  mutate(
    predictor_level = ifelse(
      predictor_name %in% group_level_predictors,
      "Group Level",
      "Epidemic Level"
    )
  )


hline_df = data.frame(outcome_name =  metrics,
                      target = c(1, 1, 0, 0.95))

# FUNCTIONS ---------------------------------------------------------------

calculate_mode <- function(x) {
  uniq_x <- unique(x)
  freq_x <- tabulate(match(x, uniq_x))
  uniq_x[which.max(freq_x)]
}
calculate_mode_expected <- function(x) {
  tab <- table(x) %>%
    as.data.frame() %>%
    mutate(expected = 1 / n(),
           observed = Freq / sum(Freq))

  if (nrow(tab) < 1) {
    return(NA)
  } else if (any(tab$observed > 1.5 * tab$expected)) {
    return(tab$x[which.max(tab$observed)])
  } else {
    return(NA)
  }
}



# Hexbin Data --------------------------------------------------------------------

cell_df <- do.call("rbind",
                   split(
                     outcomes_long,
                     interaction(outcomes_long$outcome_name, outcomes_long$predictor_name)
                   ) |>
                     lapply(function(d) {
                       d <- d |> drop_na(predictor_value, outcome_value)

                       # Check if the conditions are met for xbnds
                       if (unique(d$outcome_name) == "specificity" &&
                           unique(d$predictor_name) == "delta") {
                         xbnds <- c(-1, 1)
                       } else {
                         xbnds <- range(d$predictor_value)
                       }

                       hb <- hexbin::hexbin(
                         d$predictor_value,
                         d$outcome_value,
                         xbins = 70,
                         IDs = TRUE,
                         xbnds = xbnds
                       )

                       # Calculate the mode (most frequent) value for peak_coeff
                       mode_peak_coeff <-
                         aggregate(d$peak_coeff, by = list(hb@cID), FUN = calculate_mode)

                       cbind(
                         mode_peak_coeff,
                         frequency = hb@count / sum(hb@count),
                         X = hexbin::hcell2xy(hb)$x,
                         Y = hexbin::hcell2xy(hb)$y,
                         outcome_name = d$outcome_name[1],
                         predictor_name = d$predictor_name[1]
                       )
                     }))

f_ref <-
  cell_df %>% filter(outcome_name == "bias") %>%
  .$frequency %>% max()

cell_df <- cell_df %>%
  rename(peak_coeff = x) %>%
  mutate(
    predictor_level = ifelse(
      predictor_name %in% group_level_predictors,
      "Group Level",
      "Epidemic Level"
    ),
    freq = ifelse(frequency >= f_ref, f_ref, frequency),
    log_freq = log(freq) - min(log(freq)),
    log_freq_cat = cut(
      log_freq,
      breaks = c(0, 2.5, 5, 7.5, 10, Inf),
      labels = c("0", "2.5", "5", "7.5", "10"),
      include.lowest = TRUE
    )
  )
percentile_max <- quantile(cell_df$log_freq, 0.87)

cell_df$predictor_name <-
  factor(cell_df$predictor_name,
         levels = predictors,
         labels = label)
cell_df$outcome_name <-
  factor(cell_df$outcome_name, levels = metrics, labels = metrics)

outcomes_long$predictor_name <-
  factor(outcomes_long$predictor_name,
         levels = predictors,
         labels = label)
outcomes_long$outcome_name <-
  factor(outcomes_long$outcome_name,
         levels = metrics,
         labels = metrics)




# PLOT --------------------------------------------------------------------


p_custom_hexbin <-
  cell_df %>%
  ggplot() +
  ggh4x::facet_nested(
    outcome_name ~ factor(
      predictor_level,
      levels = c("Group Level", "Epidemic Level"),
      labels = c("Group~Level", "Epidemic~Level")
    ) +
      predictor_name,
    labeller = label_parsed,
    scales = "free"
  ) +
  geom_hex(aes(
    x = X,
    y = Y,
    fill = as.double(peak_coeff),
    alpha = as.double(log_freq_cat)
  ),
  stat = "identity"
  ) +
  geom_hline(
    data = hline_df,
    aes(yintercept = target),
    col = "#E32227",
    linetype = "solid"
  ) +
  geom_smooth(
    data = outcomes_long,
    aes(
      x = predictor_value,
      y = outcome_value,
      color = as.double(peak_coeff),
      group = as.double(peak_coeff)
    ),
    se = TRUE,
    method = "gam",
    formula = y ~ s(x, bs = "cs", k = 7), #k = 7
    linewidth = 0.5,
    alpha = 0.5
  )

p_custom_hexbin <- p_custom_hexbin +
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 2), breaks = seq(-2, 2, 0.5)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
    )
  )+
  scale_color_gradientn(
    colours = c("#3679c0", "black", "#ff007f"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    guide = "none"
  ) +
  scale_fill_gradientn(
    colours = c("#3679c0", "black", "#ff007f"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    breaks = peak_coeffs_breaks
  ) +
  labs(fill = "Peak Coefficient",
       x = "Predictor value",
       y = "Metric value") +
  theme_publication(base_size = 6)


p_custom_hexbin <-
  p_custom_hexbin +
  scale_alpha_continuous(
    "Log Count",
    range = c(0.33, 2.9),
    labels = c("0", "2.5", "5", "7.5", "10"),
  )

ggsave(
  here("analysis/simulation/plots", "hexbin_all.png"),
  plot = p_custom_hexbin,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)









# Main Predictor Selection ------------------------------------------------

main_predictors <- c(
  "delta",
  "size_freq",
  "r0",
  "n_cases",
  "group_susceptibles",#
  "trials",
  "successes",
  "total_cases",
  "total_susceptibles",#
  "n_groups",
  "sd_r0",
  "sd_peak_date",
  "sd_n_cases",
  "sd_group_susceptibles",#
  "sd_size_freq",
  "sd_delta"
)
main_predictor_labels <- c(
  "delta[G]",
  "p[s[G]]",
  "R[0[G]]",
  "n[cases[G]]",
  "p[susceptibles[G]]",#
  "n[. %<-% G]",
  "n[G %<-% G]",
  "sum(n[cases[G]])",
  "p[susceptibles]",#
  "n[groups]",
  "sigma[R[0[G]]]",
  "sigma[peak]",
  "sigma[n[cases[G]]]",
  "sigma[p[susceptibles[G]]]",#
  "sigma[p[s[G]]]",
  "sigma[delta[G]]"
)

cell_df_main <- cell_df %>%
  filter(predictor_name %in% main_predictor_labels)
outcomes_long_main <- outcomes_long %>%
  filter(predictor_name %in% main_predictor_labels)


p_custom_hexbin_main <-
  cell_df_main %>%
  ggplot() +
  ggh4x::facet_nested(
    outcome_name ~ factor(
      predictor_level,
      levels = c("Group Level", "Epidemic Level"),
      labels = c("Group~Level", "Epidemic~Level")
    ) +
      predictor_name,
    labeller = label_parsed,
    scales = "free"
  ) +
  geom_hex(aes(
    x = X,
    y = Y,
    fill = as.double(peak_coeff),
    alpha = as.double(log_freq_cat)
  ),
  stat = "identity") +
  geom_hline(
    data = hline_df,
    aes(yintercept = target),
    col = "#E32227",
    linetype = "solid"
  ) +
  geom_smooth(
    data = outcomes_long_main,
    aes(
      x = predictor_value,
      y = outcome_value,
      color = as.double(peak_coeff),
      group = as.double(peak_coeff)
    ),
    se = TRUE,
    method = "gam",
    formula = y ~ s(x, bs = "cs", k = 7),
    linewidth = 0.5,
    alpha = 0.5
  ) +
  scale_color_gradientn(
    colours = c("#3679c0", "black", "#ff007f"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    guide = "none"
  ) +
  scale_fill_gradientn(
    colours = c("#3679c0", "black", "#ff007f"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    breaks = peak_coeffs_breaks
  ) +
  labs(fill = "Peak Coefficient",
       x = "Predictor value",
       y = "Metric value") +
  theme_publication(base_size = 10)

p_custom_hexbin_main <- p_custom_hexbin_main +
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 2), breaks = seq(-2, 2, 0.5)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
    )
    #  scale_x_continuous(labels = function(x) format(x, nsmall = 2))
  ) +
  scale_alpha_continuous(
    "Log Count",
    range = c(0.33, 2.9),
    labels = c("0", "2.5", "5", "7.5", "10"),
  )


ggsave(
  here("analysis/simulation/plots", "hexbin_main.png"),
  plot = p_custom_hexbin_main,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)




# Peak --------------------------------------------------------------------
p_hexbin <- outcomes_long %>%
  filter(peak_coeff == 1) %>%
  filter(predictor_name %in% main_predictor_labels) %>%
  ggplot(aes(x = predictor_value, y = outcome_value)) +
  ggh4x::facet_nested(
    outcome_name ~ factor(
      predictor_level,
      levels = c("Group Level", "Epidemic Level"),
      labels = c("Group~Level", "Epidemic~Level")
    ) +
      predictor_name,
    labeller = label_parsed,
    scales = "free"
  ) +
  geom_hex(
    aes(fill = stat(log(count))),
    bins = 100
  ) +
  # geom_smooth(
  #   aes(color = "Smoothed Trend"),
  #   se = TRUE,
  #   method = "gam",
  #   formula = y ~ s(x, bs = "cs", k = 7),
  #   linewidth = 0.5,
  #   alpha = 0.5
  # ) +
  geom_hline(data = hline_df, aes(yintercept = target, color = "target value")) +
  scale_fill_viridis_c("Log Count",
    option = "plasma"
  ) +
  scale_color_manual(
    values = c("#E32227"),
    labels = c("target value")
  ) +
  labs(
    x = "predictor value",
    y = "Metric value",
    linetype = "",
    color = ""
  ) +
  theme_publication(base_size = 10)

p_hexbin <- p_hexbin +
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 1),
                                                  breaks = seq(-2, 2, 0.5)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0.2, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0.2, 1))
    )
  )

ggsave(
  here("analysis/simulation/plots", "hexbin_peak1.png"),
  plot = p_hexbin,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)



