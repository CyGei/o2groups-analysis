library(tidyverse)
library(here)
# Helpers -----------------------------------------------------------------
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))


# Variables ---------------------------------------------------------------
predictors <-
  c(
    "delta",
    "size",
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
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd"
  )

label <- c(
  "delta[G]",
  "s[G]",
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


# Data --------------------------------------------------------------------

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

outcomes_long <- outcomes_df %>%
  group_by(scenario, peak_coeff) %>%
  mutate(
    sd_peak_date = sd(peak_date, na.rm = TRUE),
    sd_n_cases = sd(n_cases, na.rm = TRUE),
    sd_size = sd(size, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-c(peak_date, all_of(extra_metrics)))

outcomes_long <- outcomes_long %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value")


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

cell_df_edit <- cell_df %>%
  rename(peak_coeff = x) %>%
  mutate(
    freq = ifelse(frequency >= f_ref, f_ref, frequency),
    log_freq = log(freq) - min(log(freq)),
    scaled_alpha = (log_freq - min(log_freq)) / (max(log_freq) - min(log_freq))
  )
percentile_max <- quantile(cell_df_edit$log_freq, 0.87)


cell_df_edit$predictor_name <-
  factor(cell_df_edit$predictor_name,
         levels = predictors,
         labels = label)
cell_df_edit$outcome_name <-
  factor(cell_df_edit$outcome_name, levels = metrics, labels = metrics)

outcomes_long$predictor_name <-
  factor(outcomes_long$predictor_name,
         levels = predictors,
         labels = label)
outcomes_long$outcome_name <-
  factor(outcomes_long$outcome_name, levels = metrics, labels = metrics)

##########
# Smooth #
##########

outcomes_long_cut <- outcomes_long %>%
  group_by(outcome_name, predictor_name, peak_coeff) %>%
  mutate(xbin = cut(
    predictor_value,
    breaks = 20 )) %>%
  ungroup()

# Count the number of observations
count_data <- outcomes_long_cut %>%
  group_by(outcome_name, predictor_name, peak_coeff, xbin) %>%
  summarise(count = n()) %>%
  ungroup()


df <- left_join(outcomes_long_cut,
                count_data,
                by = c("outcome_name", "predictor_name", "peak_coeff", "xbin"))

# Calculate the density
library(mgcv)
predict_data <- tibble(predictor_value = density(
  df$predictor_value,
  from = min(df$predictor_value),
  to = max(df$predictor_value)
)$x)


#take 5minish
smooth_df <- df %>%
  group_by(outcome_name, predictor_name, peak_coeff) %>%
  reframe(
    pred_val = density(predictor_value, from = min(predictor_value), to = max(predictor_value))$x,
    density = density(predictor_value, from = min(predictor_value), to = max(predictor_value))$y,
    outcome_value = tryCatch(
      predict(mgcv::gam(
        outcome_value ~ s(predictor_value, k = 7),
        method = "REML"
      ),
      newdata = predict_data),
      error = function(e) NA
    )
  )
#saveRDS( smooth_df, here("analysis/simulation/data/model", "smooth_df.rds"))
readRDS(here("analysis/simulation/data/model", "smooth_df.rds")) -> smooth_df
smooth_df <- smooth_df %>%
  rename(predictor_value = pred_val) %>%
  group_by(outcome_name, predictor_name, peak_coeff) %>%
  mutate(
    xend = lag(predictor_value),
    yend = lag(outcome_value),
    density = density / max(density)
  )

##########
# Spline #
##########
library(splines)

df_segment<- df %>%
  group_by(outcome_name, predictor_name, peak_coeff) %>%
  summarise(
    density = list(density(predictor_value, from = min(predictor_value), to = max(predictor_value))$y),
    x_vals = list(predictor_value),
    y_vals = list(outcome_value)
  ) %>%
  mutate(
    spline_fit = map2(x_vals, y_vals, ~ {
      # Filter out missing or infinite values
      non_missing_indices <- complete.cases(.x, .y) & !is.infinite(.x) & !is.infinite(.y)
      .x <- .x[non_missing_indices]
      .y <- .y[non_missing_indices]

      if (length(.x) > 1) {
        smooth.spline(.x, .y, tol = 1e-5)  # Adjust 'tol' as needed
      } else {
        NULL
      }
    })
  ) %>%
  ungroup() %>%
  mutate(
    x_vals = map(spline_fit, ~ if (!is.null(.x)) .x$x else rep(NA_real_, length(.y))),
    y_vals = map(spline_fit, ~ if (!is.null(.x)) .x$y else rep(NA_real_, length(.y)))
    ) %>%
  select(outcome_name, predictor_name, peak_coeff, x_vals, y_vals)


df_segment_exploded <- df_segment %>%
  unnest(cols = c(x_vals, y_vals))



##########
# Average #
##########
mean_density <- outcomes_long %>%
  group_by(peak_coeff, predictor_name, outcome_name) %>%
  reframe(
    pred_val = density(predictor_value, from = min(predictor_value), to = max(predictor_value))$x,
    density = density(predictor_value, from = min(predictor_value), to = max(predictor_value))$y,
    cut = cut(pred_val, breaks = 100)) %>%
  group_by(peak_coeff, predictor_name, outcome_name, cut) %>%
  summarise(
   mean_density = mean(density, na.rm = TRUE)
  )

mean_line <- outcomes_long %>%
  group_by(peak_coeff, predictor_name, outcome_name) %>%
  mutate(cut = cut(predictor_value, breaks = 100)) %>%
  group_by(peak_coeff, predictor_name, outcome_name, cut) %>%
  summarise(mean_y = mean(outcome_value, na.rm = TRUE),
            midpoint_x = extract_and_calculate_midpoint(cut)) %>%
  ungroup() %>%
  group_by(peak_coeff, predictor_name, outcome_name) %>%
  mutate(smoothed_mean_y = zoo::rollmean(mean_y, k = 10L, align = "center", fill = NA))

avg_df <-
  left_join(mean_density,
            mean_line,
            by = c("peak_coeff", "predictor_name", "outcome_name", "cut")) %>%
  group_by(outcome_name, predictor_name, peak_coeff) %>%
  mutate(
    xend = lag(midpoint_x),
    yend = lag(smoothed_mean_y),
    density = mean_density  / max(mean_density )
  )


# PLOT --------------------------------------------------------------------

p_custom_hexbin <-
  cell_df_edit %>%
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
  scale_alpha_continuous(
    range = c(0, 1),
    limits = c(0, percentile_max),
    guide = "none"
  )+
  #ggnewscale::new_scale()+
  geom_hline(
    data = hline_df,
    aes(yintercept = target),
    col = "#3d3d3d",
    linetype = "solid"
  ) +
  geom_segment(
    aes(
      colour = peak_coeff,
      alpha = density,
      x = predictor_value,
      y = outcome_value,
      xend = xend,
      yend = yend
    ),
    data = smooth_df,
    linewidth = 1.5
  ) +
# geom_segment(
#   aes(
#     colour = peak_coeff,
#     alpha = mean_density,
#     x = midpoint_x,
#     y = smoothed_mean_y,
#     xend = xend,
#     yend = yend
#   ),
#   data = avg_df,
#   linewidth = 0.8
# )+
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

p_custom_hexbin <- p_custom_hexbin +
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 2), breaks = seq(-2, 2, 0.5)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
    )
  )



p_custom_hexbin <-
  p_custom_hexbin + scale_alpha_continuous(
    range = c(0, 1),
    guide = "none"
  )

p_custom_hexbin

# ggsave(
#   here("analysis/simulation/plots", "hexbin_all.png"),
#   plot = p_custom_hexbin,
#   width = 16,
#   height = 8,
#   units = "in",
#   dpi = 300
# )




