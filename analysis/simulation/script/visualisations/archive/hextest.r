library(tidyverse)
library(here)


hexbin_data <- function(predictors = NULL){

default_predictors <-
  c(
    "delta",
    "size",
    "size_freq",
    "intro_n",
    "intro_prop",
    "r0",
    "n_cases",
    "successes",
    "trials",
    "n_groups",
    "total_cases",
    "sd_peak_date",
    "sd_n_cases",
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

default_labels <- c(
  "delta[G]",
  "s[G]",
  "p[s[G]]",
  "n[intro[G]]",
  "p[intro[G]]",
  "R[0[G]]",
  "n[cases[G]]",
  "n[G %<-% G]",
  "n[. %<-% G]",
  "n[groups]",
  "sum(n[cases])",
  "sigma[peak]",
  "sigma[n[cases[G]]]",
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

if(is.null(predictors)){
  predictors <- default_predictors
}
dictionary <- setNames(default_labels, default_predictors)
labels  <- dictionary[predictors]


# Input Data --------------------------------------------------------------------

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
  select(all_of(c(metrics, predictors, "peak_coeff")))

outcomes_long <- outcomes_df %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value")


hline_df = data.frame(outcome_name =  metrics,
                      target = c(1, 1, 0, 0.95))


# Hexbin Data -------------------------------------------------------------------
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


# cell_df factor
cell_df <- cell_df %>%
    mutate(
        log_freq = log(frequency) - min(log(frequency)),
        scaled_alpha = (log_freq - min(log_freq)) / (max(log_freq) - min(log_freq))
    ) %>%
    rename(peak_coeff = x)
head(cell_df)



custom_locpoly <- function(data, x, y, bandwidth_factor = 0.2) {
  x <- data[[x]]
  y <- data[[y]]

  if (sd(x) == 0) {
    return(data.frame(x = rep(NA, length(x)), y = rep(NA, length(y))))
  }

  observed_range <- diff(range(x))
  custom_bandwidth <- observed_range * bandwidth_factor

  # Clip the bandwidth to ensure it doesn't extend beyond the observed range
  custom_bandwidth <- pmin(custom_bandwidth, observed_range)

  smoothed_data <- KernSmooth::locpoly(x, y, bandwidth = custom_bandwidth, degree = 1)

  data.frame(x = smoothed_data$x, y = smoothed_data$y)
}


# predict by group using locpoly
pred_df <- outcomes_long %>%
  group_by(peak_coeff, outcome_name, predictor_name) %>%
  nest() %>%
  # filter out when predictor_name is delta and outcome_name is specificity
  filter(!(predictor_name == "delta" & outcome_name == "specificity")) %>%
  mutate(data = map(data, ~ .x |> drop_na(predictor_value, outcome_value))) %>%
  mutate(smoothed_data = map(data, ~ custom_locpoly(.x, "predictor_value", "outcome_value")))

#create_approxfun <- possibly(function(dens) approxfun(dens), otherwise = NA)

# test_df <- pred_df %>%
#   mutate(density = map(data, ~ density(
#     .x$predictor_value,
#     from = min(.x$predictor_value),
#     to = max(.x$predictor_value),
#   ))) %>%
#   mutate(fun = map(density, ~create_approxfun(.x))) %>%
#   mutate(estimated_dens = map2(.x = smoothed_data, .y = fun, ~ .y(.x$x))) %>%
#   select(estimated_dens, smoothed_data) %>%
#   unnest(c(estimated_dens, smoothed_data)) %>%
#   group_by(peak_coeff, outcome_name, predictor_name) %>%
#   mutate(xend = lag(x),
#          yend = lag(y),
#          density = estimated_dens/ max(estimated_dens))

# test_df <- pred_df %>%
#   mutate(density = map(data, ~ density(
#     .x$predictor_value,
#     from = min(.x$predictor_value),
#     to = max(.x$predictor_value),
#   ))) %>%
#   mutate(fun = map(density, ~create_approxfun(.x))) %>%
#   mutate(density = map2(.x = smoothed_data, .y = fun, ~ .y(.x$x))) %>%
#   select(density, smoothed_data) %>%
#   unnest(c(density, smoothed_data)) %>%
#   group_by(peak_coeff, outcome_name, predictor_name) %>%
#   mutate(xend = lag(x),
#          yend = lag(y),
#          density = (density - min(density)) / (max(density) - min(density)))



# use hist instead of density
count_bins <- function(x, bins = 20) {
  hist_data <- hist(x, breaks = bins, plot = FALSE)
  return(data.frame(bin = hist_data$breaks[-1], frequency = hist_data$counts))
}

test_df <- pred_df %>%
  mutate(binned_data = map(.x = data, ~ count_bins(.x[["predictor_value"]])))

# # Function to categorize x values based on bins
# categorize_x <- function(smoothed_data, binned_data) {
#   smoothed_data %>%
#     mutate(bin_category = cut(x, breaks = binned_data$bin, labels = FALSE, include.lowest = TRUE))
# }


# # Function to assign the bin to x values based on closest distance
# assign_bin <- function(smoothed_data, binned_data) {
#   smoothed_data %>%
#     mutate(bin_category = sapply(x, function(x_val) {
#       closest_bin_index <- which.min(abs(x_val - binned_data$bin))
#       return(binned_data$bin[closest_bin_index])
#     }))
# }

# Function to assign the bin and frequency to x values based on closest distance
assign_bin <- function(smoothed_data, binned_data) {
  smoothed_data %>%
    mutate(bin_category = sapply(x, function(x_val) {
      closest_bin_index <- which.min(abs(x_val - binned_data$bin))
      closest_bin <- binned_data$bin[closest_bin_index]
      return(closest_bin)
    })) %>%
    mutate(frequency = sapply(bin_category, function(bin_val) {
      index <- which(binned_data$bin == bin_val)
      return(binned_data$frequency[index])
    }))
}

test2 = test_df %>%
  mutate(
    data_with_categories = map2(smoothed_data, binned_data, ~ assign_bin(.x, .y))
  )

smoothed_df <- test2 %>%
  select(peak_coeff, outcome_name, predictor_name, data_with_categories) %>%
  unnest(data_with_categories)
  mutate(xend = lag(x, default = min(x)),
         yend = lag(y, default = min(y)),
         frequency_norm = (frequency - min(frequency)) / (max(frequency) - min(frequency)))

# Plotting ---------------------------------------------------------------------


p = cell_df %>%
  ggplot() +
  facet_grid(outcome_name ~ predictor_name,
             labeller = label_parsed,
             scales = "free") +
  geom_hex(aes(
    x = X,
    y = Y,
    fill = as.double(peak_coeff),
    alpha = log_freq
  ),
  stat = "identity") +
  geom_hline(
    data = hline_df,
    aes(yintercept = target),
    col = "#3d3d3d",
    linetype = "solid"
  ) +
  geom_segment(
    aes(
      x = x,
      y = y,
      alpha = density,
      xend = xend,
      yend = yend,
      col =  as.double(peak_coeff)
    ),
    data = test_df,
    linewidth = 1
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
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 2), breaks = seq(-2, 2, 0.5)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
    )
  )+
  theme_bw()+
  coord_cartesian(clip = "on") +
  scale_alpha_continuous(range = c(0.5, 1))
p

}


predictors = c("r0", "delta", "size", "n_groups", "sd_peak_date")
hexbin_data(predictors)




p_hex <- cell_df %>%
  ggplot() +
  facet_grid(outcome_name ~ predictor_name,
             labeller = label_parsed,
             scales = "free") +
  geom_hex(aes(
    x = X,
    y = Y,
    fill = as.double(peak_coeff),
    alpha = log_freq
  ),
  stat = "identity") +
  geom_hline(
    data = hline_df,
    aes(yintercept = target),
    col = "#3d3d3d",
    linetype = "solid"
  )


  ggplot() +
  facet_grid(outcome_name ~ predictor_name,
             labeller = label_parsed,
             scales = "free")+
  geom_segment(
    aes(
      x = x,
      y = y,
      alpha = frequency_norm,
      xend = xend,
      yend = yend,
      col =  as.double(peak_coeff)
    ),
    linewidth = 1
  )+
  scale_alpha_continuous(range = c(0.2, 1))+
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
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 2), breaks = seq(-2, 2, 0.5)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
    )
  )+
  theme_bw()+
  coord_cartesian(clip = "on")





cell_df %>%
  ggplot() +
  facet_grid(outcome_name ~ predictor_name,
             labeller = label_parsed,
             scales = "free") +
  geom_hex(aes(
    x = X,
    y = Y,
    fill = as.double(peak_coeff),
    alpha = log_freq
  ),
  stat = "identity") +
  geom_hline(
    data = hline_df,
    aes(yintercept = target),
    col = "#3d3d3d",
    linetype = "solid"
  ) +
  # use spline geom_smooth
  geom_smooth(
    aes(
      x = predictor_value,
      y = outcome_value,
      col =  as.double(peak_coeff),
      group = as.double(peak_coeff)
    ),
    method = "gam",
    formula = y ~ s(x, bs = "cs", k = 7),
    linewidth = 0.5,
    data = outcomes_long
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
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 2), breaks = seq(-2, 2, 0.5)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
    )
  )+
  theme_bw()+
  coord_cartesian(clip = "on") +
  scale_alpha_continuous(range = c(0.5, 1))
