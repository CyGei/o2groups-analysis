library(ggplot2)
library(ggnewscale)
library(furrr)

# Helper Functions ----------------------------------------------------------------
n_workers <- future::availableCores() - 2
plan(multisession, workers = n_workers)

read_files <- function(path, sample_n = Inf) {
  files <-
    list.files(path = path,
               pattern = "*.rds",
               full.names = TRUE)

  # Sample files if sample_n is provided
  if (sample_n < length(files)) {
    set.seed(123)  # You can use any seed value
    files <- sample(files, size = sample_n)
  }

  list <- furrr::future_map(
    .x = files,
    .f = ~ readRDS(.x) %>% mutate(scenario = basename(.x)),
    .options = furrr_options(seed = NULL)
  )

  return(list)
}


med <- function(x) {
  m <- median(x, na.rm = TRUE)
  return(c(y = m))
}
mean_y <- function(x) {
  m <- mean(x, na.rm = TRUE)
  return(c(y = m))
}


get_midpoint <- function(bin_description) {
  digits <-
    as.numeric(unlist(stringr::str_extract_all(bin_description, "-?\\d+\\.?\\d*")))

  # Calculate midpoint
  midpoint <- mean(digits)

  return(midpoint)
}


color_pal <- function(col_low, col_high, breaks){
  pal <- colorRampPalette(c(col_low, col_high))
  return(pal(length(breaks)))
}

color_pal <- function(col_low,
                      col_high,
                      breaks,
                      focus_value = NULL,
                      focus_color = "black") {
  pal <- colorRampPalette(c(col_low, col_high))
  colors <- pal(length(breaks))

  if (!is.null(focus_value)) {
    colors[which(breaks == focus_value)] <- focus_color
  }

  return(colors)
}

# Themes -------------------------------------------------------------------------
theme_publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "plain",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.5, "cm"),
            legend.key.width = unit(0.7, "cm"),
            legend.margin = margin(0,0,0,0),
            legend.title = element_text(face = "bold", size = rel(0.85)),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="black"),
            strip.text = element_text(face="bold", size = rel(1.2))
    ))

}

theme_publication_hexbin <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "plain",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.5, "cm"),
            legend.key.width = unit(0.7, "cm"),
            legend.margin = margin(0,0,0,0),
            legend.title = element_text(face = "bold", size = rel(0.85)),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="black")
    ))

}

guide_colour_bar <- function(key_color = "white"){
  guides(
    colour = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      label.theme = element_text(color = key_color, margin = margin(t = -17.5)),
      keywidth = 2,
      nrow = 1,
      byrow = TRUE
    )
  )
}

guide_fill_bar <- function(key_color = "white"){
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      label.theme = element_text(color = key_color, margin = margin(t = -17.5)),
      keywidth = 2,
      nrow = 1,
      byrow = TRUE
    )
  )
}




rounder <- function(x) {
  round(x, 2)
}



# No gridlines
# theme_apa <- theme_bw()+
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         panel.border=element_blank(),
#         axis.line=element_line(),
#         text=element_text(family='Times'),
#         legend.title=element_blank(),
#         axis.text.y=element_text(size = 12),
#         axis.text.x=element_text(size = 12))
#
#
#
#
#
#
#
#
#
#
#
#
# # Plot Functions ----------------------------------------------------------------
#
# # A heatmap showing the frequency of observations in each bin of two variables.
# plot_heatmap <- function(df,
#                          x,
#                          y,
#                          x_breaks = NULL,
#                          y_breaks = NULL,
#                          x_length_out = 10,
#                          y_length_out = 10,
#                          facet_vars = NULL,
#                          bin = FALSE,
#                          min_freq = 0,
#                          min_alpha = 1) {
#   if (is.null(x_breaks)) {
#     x_breaks <-
#       seq(min(df[[x]], na.rm = TRUE),
#         max(df[[x]], na.rm = TRUE),
#         length.out = x_length_out
#       )
#   }
#   if (is.null(y_breaks)) {
#     y_breaks <-
#       seq(min(df[[y]], na.rm = TRUE),
#         max(df[[y]], na.rm = TRUE),
#         length.out = y_length_out
#       )
#   }
#
#   data <- df %>%
#     select(!!!syms(c(x, y, facet_vars))) %>%
#     mutate(
#       x_bin = cut(.data[[x]], breaks = x_breaks, include.lowest = TRUE),
#       y_bin = cut(.data[[y]], breaks = y_breaks, include.lowest = TRUE)
#     ) %>%
#     drop_na() %>%
#     count(x_bin, y_bin, !!!syms(facet_vars)) %>%
#     group_by(x_bin, !!!syms(facet_vars)) %>%
#     mutate(
#       denom = sum(n),
#       freq = n / denom
#     ) %>%
#     filter(freq >= min_freq)
#
#   if (isTRUE(bin)) {
#     data$x_point <- data$x_bin
#     data$y_point <- data$y_bin
#   } else {
#     data$x_point <- sapply(data$x_bin, extract_and_calculate_midpoint)
#     data$y_point <- sapply(data$y_bin, extract_and_calculate_midpoint)
#   }
#
#   plot <- ggplot(
#     data,
#     aes(x = x_point, y = y_point)
#   ) +
#     # Frequencies
#     geom_tile(aes(fill = freq, alpha = freq), col = "white") +
#     scale_fill_viridis_c(option = "D") +
#     scale_alpha_continuous(range = c(min_alpha, 1))+
#     labs(fill = "Frequency") +
#     ggnewscale::new_scale_fill() +
#
#     # Denominator
#     geom_tile(aes(
#       y = ifelse(isTRUE(bin), -1, min(data$y_point) * 1.25),
#       fill = denom,
#       height = ifelse(isTRUE(bin), 1, 0.1)
#     )) +
#     scale_fill_viridis_c(
#       option = "A",
#       label = scales::comma
#     ) +
#     labs(fill = "Denominator") +
#     labs(
#       x = x,
#       y = y
#     ) +
#     theme_classic()+
#     guides(alpha = "none")
#
#   if (!is.null(facet_vars)) {
#     plot <- plot + facet_wrap(~ .data[[facet_vars]])
#   }
#
#   return(plot)
# }
#
# # Example usage:
# # For the binned version:
# # plot_heatmap(your_data_frame, "x_column", "y_column", facet_vars = "facet_variable", bin = TRUE)
#
# # For the continuous version:
# # plot_heatmap(your_data_frame, "x_column", "y_column", facet_vars = "facet_variable", bin = FALSE)
#
#
# plot_scatter <- function(df,
#                          x,
#                          y,
#                          level_vars = c("scenario", "name"),
#                          facet_vars = NULL,
#                          alpha = 0.5) {
#   aggregated_data <- df %>%
#     group_by(!!!syms(c(x, level_vars, facet_vars))) %>%
#     summarise(y = mean(!!!syms(y), na.rm = TRUE))
#
#   plot <- ggplot(aggregated_data, aes(x = .data[[x]], y = y)) +
#     geom_point(alpha = alpha) +
#     labs(
#       x = x,
#       y = paste("Mean", y)
#     ) +
#     theme_classic()
#
#   if(!is.null(facet_vars)) {
#     plot <- plot + facet_wrap(~ .data[[facet_vars]])
#   }
#
#     return(plot)
# }
