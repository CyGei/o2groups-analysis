library(tidyverse)
library(here)
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))


hexbin_plot <- function(predictors = NULL) {
  # Parameters --------------------------------------------------------------------
  default_predictors <-
    c(
      "delta",
      "size",
      "intro_n",
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
      "GT_mean",
      "GT_sd",
      "INCUB_mean",
      "INCUB_sd"
    )

  default_labels <- c(
    "delta[G]",
    "s[G]",
    "n[intro[G]]",
    "p[intro[G]]",
    "R[0[G]]",
    "n[cases[G]]",
    "p[susceptibles[G]]",
    "n[G %<-% G]",
    "n[. %<-% G]",
    "n[groups]",
    "sum(n[cases])",
    "p[susceptibles]",
    "sigma[peak]",
    "sigma[n[cases[G]]]",
    "sigma[p[susceptibles[G]]]",
    "sigma[s[G]]",
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

  if (is.null(predictors)) {
    predictors <- default_predictors
  }
  dictionary <- setNames(default_labels, default_predictors)
  labels <- dictionary[predictors]


  # Input Data --------------------------------------------------------------------

  outcomes_df <-
    readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
    select(all_of(c(metrics, predictors, "peak_coeff")))

  outcomes_long <- outcomes_df %>%
    pivot_longer(
      cols = all_of(predictors),
      names_to = "predictor_name",
      values_to = "predictor_value"
    ) %>%
    pivot_longer(cols = all_of(metrics),
                 names_to = "outcome_name",
                 values_to = "outcome_value")


  hline_df <- data.frame(outcome_name = metrics,
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

                         #frequency column is then calculated as the ratio of the count of data points in a specific hexbin
                         #to the total count of all data points in the entire hexbin plot.

                         cbind(
                           mode_peak_coeff,
                           frequency = hb@count / sum(hb@count),
                           X = hexbin::hcell2xy(hb)$x,
                           Y = hexbin::hcell2xy(hb)$y,
                           outcome_name = d$outcome_name[1],
                           predictor_name = d$predictor_name[1]
                         )
                       }))


  # f_ref <-
  #   cell_df %>% filter(outcome_name == "bias") %>%
  #   .$frequency %>% max()

  cell_df <- cell_df %>%
    rename(peak_coeff = x) %>%
    group_by(outcome_name, predictor_name) %>%
    mutate(
      #frequency = ifelse(frequency >= f_ref, f_ref, frequency),
      frequency = (frequency - min(frequency)) / (max(frequency) - min(frequency)),
      log_frequency = log(frequency) - min(log(frequency))
    )





  # Smoothed Lines ----------------------------------------------------------


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

    smoothed_data <-
      KernSmooth::locpoly(x, y, bandwidth = custom_bandwidth, degree = 1)

    data.frame(x = smoothed_data$x, y = smoothed_data$y)
  }

  count_bins <- function(x, bins = 20) {
    hist_data <- hist(x, breaks = bins, plot = FALSE)
    return(data.frame(bin = hist_data$breaks[-1], frequency = hist_data$counts))
  }

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


  pred_df <- outcomes_long %>%
    group_by(peak_coeff, outcome_name, predictor_name) %>%
    nest() %>%
    filter(!(predictor_name == "delta" &
               outcome_name == "specificity")) %>%
    mutate(
      data = map(data, ~ .x |> drop_na(predictor_value, outcome_value)),
      smoothed_data = map(
        data,
        ~ custom_locpoly(.x, "predictor_value", "outcome_value")
      ),
      binned_data = map(.x = data, ~ count_bins(.x[["predictor_value"]])),
      smoothed_data_binned = map2(smoothed_data, binned_data, ~ assign_bin(.x, .y))
    )

  smoothed_df <- pred_df %>%
    select(peak_coeff,
           outcome_name,
           predictor_name,
           smoothed_data_binned) %>%
    unnest(smoothed_data_binned) %>%
    mutate(
      xend = lag(x, default = min(x)),
      yend = lag(y, default = min(y)),
      frequency_norm = (frequency - min(frequency)) / (max(frequency) - min(frequency)),
      log_frequency = log(frequency_norm) - min(log(frequency_norm))
    )



  # Plot -----------------------------------------------------------------------
  percentile_max <- quantile(log(cell_df$frequency), 0.87)

  p_canvas <-
    ggplot(cell_df) +
    facet_grid(outcome_name ~ predictor_name,
               labeller = label_parsed,
               scales = "free")

  p_hexbin <-
    p_canvas +
    geom_hex(data = cell_df,
             aes(
               x = X,
               y = Y,
               fill = as.double(peak_coeff),
               alpha = log(frequency)
             ),
             stat = "identity") +
    geom_hline(
      data = hline_df,
      aes(yintercept = target),
      col = "#3d3d3d",
      linetype = "solid"
    )


  p_smoothed <-
    p_hexbin +
    geom_segment(
      data = smoothed_df,
      aes(
        x = x,
        y = y,
        #alpha = frequency_norm,
        xend = xend,
        yend = yend,
        col =  as.double(peak_coeff)
      ),
      linewidth = 0.6,
      alpha = 1
    )


  p_final <- p_smoothed +
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
         y = "Metric value")+
    ggh4x::facetted_pos_scales(
      y = list(
        outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 2), breaks = seq(-2, 2, 0.5)),
        outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
        outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
        outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
      )
    )+
    theme_classic() +
    scale_alpha_continuous(
      range = c(0, 1),
      limits = c(0, percentile_max),
      guide = "none"
    )

return(p_final)

}


p_hexbin <- hexbin_plot()

ggsave(
  here("analysis/simulation/plots", "hexbin_locpoly.png"),
  plot = p_hexbin,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)

