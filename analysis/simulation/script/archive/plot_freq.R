# plot_relationship -------------------------------------------------------
# A scatter plot summarizing the relationship between a predictor and the mean bias across simulations.
plot_relationship <- function(predictor) {
  aggregated_data <- model_df %>%
    group_by(peak_coeff, .data[[predictor]]) %>%
    summarise(mean_bias = mean(bias))

  ggplot(aggregated_data, aes(x = .data[[predictor]], y = mean_bias)) +
    geom_point(aes(col = as.character(peak_coeff)), alpha = 0.6) +
    geom_hline(aes(yintercept = 0), col = "steelblue") +
    labs(x = predictor, y = "Mean Bias", col = "Peak Coeff") +
    ggtitle(paste("Relationship between", predictor, "and Mean Bias"))
}





# plot_freq ------------------------------------------------------------
# A heatmap showing the frequency of observations in each bin of two variables.
plot_freq <- function(df,
                      x,
                      y,
                      x_breaks = NULL,
                      y_breaks = NULL,
                      x_length_out = 10,
                      y_length_out = 10,
                      facet_vars = NULL) {
  if (is.null(x_breaks)) {
    x_breaks <-
      seq(min(df[[x]], na.rm = TRUE),
          max(df[[x]], na.rm = TRUE),
          length.out = x_length_out
      )
  }
  if (is.null(y_breaks)) {
    y_breaks <-
      seq(min(df[[y]], na.rm = TRUE),
          max(df[[y]], na.rm = TRUE),
          length.out = y_length_out
      )
  }

  data <- df %>%
    select(!!!syms(c(x, y, facet_vars))) %>%
    mutate(
      x_bin = cut(.data[[x]], breaks = x_breaks),
      y_bin = cut(.data[[y]], breaks = y_breaks)
    ) %>%
    drop_na() %>%
    count(x_bin, y_bin, !!!syms(facet_vars)) %>%
    group_by(x_bin, !!!syms(facet_vars)) %>%
    mutate(
      denom = sum(n),
      freq = n / denom
    )

  plot <-
    ggplot(data, aes(x = x_bin, y = y_bin)) +
    geom_tile(aes(fill = freq), col = "white") +
    scale_fill_viridis_c(option = "D") +
    labs(fill = "Frequency") +
    ggnewscale::new_scale_fill() +

    # geom_label(aes(
    #   x = x_bin,
    #   y = -1.5,
    #   label = scales::comma_format()(denom),
    #   fill = denom
    # ), vjust = -0.5) +
    # scale_fill_viridis_c(option = "A") +
    # labs(fill = "Denominator") +

    geom_tile(aes(
      y = -1,
      fill = denom
    )) +
    scale_fill_viridis_c(option = "A", label = scales::comma) +
    labs(fill = "Denominator") +
    labs(
      x = paste(x, "Bin"),
      y = paste(y, "Bin")
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (!is.null(facet_vars)) {
    plot <- plot + facet_wrap(~ .data[[facet_vars]])
  }

  return(plot)
}



# plot_freq_c -------------------------------------------------------------
# a heatmap showing the frequency of observations in each bin of two variables with continuous axes.
extract_and_calculate_midpoint <- function(bin_description) {
  digits <-
    as.numeric(unlist(str_extract_all(bin_description, "-?\\d+\\.?\\d*")))

  # Calculate midpoint
  midpoint <- mean(digits)

  return(midpoint)
}

plot_freq_c <- function(df,
                        x,
                        y,
                        x_breaks = NULL,
                        y_breaks = NULL,
                        x_length_out = 10,
                        y_length_out = 10,
                        facet_vars = NULL) {
  if (is.null(x_breaks)) {
    x_breaks <-
      seq(min(df[[x]], na.rm = TRUE),
          max(df[[x]], na.rm = TRUE),
          length.out = x_length_out
      )
  }
  if (is.null(y_breaks)) {
    y_breaks <-
      seq(min(df[[y]], na.rm = TRUE),
          max(df[[y]], na.rm = TRUE),
          length.out = y_length_out
      )
  }

  data <- df %>%
    select(!!!syms(c(x, y, facet_vars))) %>%
    mutate(
      x_bin = cut(.data[[x]], breaks = x_breaks),
      y_bin = cut(.data[[y]], breaks = y_breaks)
    ) %>%
    drop_na() %>%
    count(x_bin, y_bin, !!!syms(facet_vars)) %>%
    group_by(x_bin, !!!syms(facet_vars)) %>%
    mutate(
      denom = sum(n),
      freq = n / denom
    )

  data$x_midpoint <-
    sapply(data$x_bin, extract_and_calculate_midpoint)
  data$y_midpoint <-
    sapply(data$y_bin, extract_and_calculate_midpoint)


  plot <- ggplot(
    data,
    aes(x = x_midpoint, y = y_midpoint)
  ) +
    geom_tile(aes(fill = freq), col = "white") +
    scale_fill_viridis_c(option = "D") +
    labs(fill = "Frequency") +
    ggnewscale::new_scale_fill() +
    geom_tile(aes(
      y = min(y_midpoint) * 1.25,
      fill = denom,
      height = 0.1
    )) +
    scale_fill_viridis_c(
      option = "A",
      label = scales::comma
    ) +
    labs(fill = "Denominator") +
    labs(
      x = paste(x, "Bin"),
      y = paste(y, "Bin")
    ) +
    theme_classic()

  if (!is.null(facet_vars)) {
    plot <- plot + facet_wrap(~ .data[[facet_vars]])
  }

  return(plot)
}



