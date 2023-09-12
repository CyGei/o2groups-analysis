# Scenario Dataframe ------------------------------------------------------

scenarios <- readRDS("analysis/data/scenarios.rds")

# x is a distcrete object
pmf_to_mu <- function(x) {
  sum(x$d(0:100) * (0:100))
}
pmf_to_sd <- function(x) {
  sqrt(sum(x$d(0:100) * (0:100)^2) - pmf_to_mu(x)^2)
}


# Create an empty data frame
scenario_table <- data.frame()

# Iterate over each scenario in the list
for (scenario in scenarios) {
  # Create a data frame for the current scenario
  scenario_df <- data.frame(
    scenario = scenario$scenario,
    n_groups = scenario$n_groups,
    size = paste0(scenario$size, collapse = ", "),
    name = paste0(scenario$name, collapse = ", "),
    delta = paste0(round(scenario$delta, digits = 2), collapse = ", "),
    intro_group = paste0(scenario$intro_group, collapse = ", "),
    intro_n = paste0(scenario$intro_n, collapse = ", "),
    r0 = paste0(scenario$r0, collapse = ", "),
    generation_time = paste0(
      "[",
      round(pmf_to_mu(scenario$generation_time), digits = 2),
      ", ",
      round(pmf_to_sd(scenario$generation_time), digits = 2),
      "]"
    ),
    incubation_period = paste0(
      "[",
      round(pmf_to_mu(scenario$incubation_period), digits = 2),
      ", ",
      round(pmf_to_sd(scenario$incubation_period), digits = 2),
      "]"
    )
  )

  # Append the scenario data frame to the simulation table
  scenario_table <- rbind(scenario_table, scenario_df)
}



# Functions for Table -----------------------------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)

# Custom html formatter function to apply colors
colorise <- function(x) {
  colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  split_values <- strsplit(x, ", ")
  colored_values <- lapply(split_values, function(vals) {
    colored <- sapply(seq_along(vals), function(i) {
      val <- paste0('<span style="color:', colors[i], ';">', vals[i], "</span>")
      val
    })
    paste(colored, collapse = ", ")
  })
  unlist(colored_values)
}

remove_html_tags <- function(x) {
  gsub("<.*?>", "", x)
}

# replace_repetitive <- function(x) {
#   y_list <- strsplit(x, ", ")

#   replace_repetitive_single <- function(y) {
#     y_text <- remove_html_tags(y)  # Extract text values by removing HTML tags
#     if (length(y_text) > 3) {
#       is_repeated <- y_text[-1] == y_text[-length(y_text)]
#       if (any(is_repeated)) {
#         repeat_start <- min(which(is_repeated))
#         repeat_end <- max(which(is_repeated)) + 1 #!!
#         z <- c(c(y[1:repeat_start]), "...", c(y[repeat_end]))
#         paste(z, collapse = ", ")
#       } else {
#         paste(y, collapse = ", ")
#       }
#     } else {
#       paste(y, collapse = ", ")
#     }
#   }

#   result <- sapply(y_list, replace_repetitive_single)
#   return(result)
# }
replace_and_colorise <- function(x) {
  replace_and_colorise_single <- function(x) {
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

    y <- strsplit(x, ", ")[[1]]
    # Detect consecutive duplicates using rle
    rle_result <- rle(y)

    modified_y <- c()
    for (i in 1:length(rle_result$values)) {
      value <- rle_result$values[i]
      length <- rle_result$lengths[i]
      idx_end <- sum(rle_result$lengths[1:i])
      idx_start <- idx_end - length + 1

      # If there are less than 3 consecutive duplicates, leave as is
      if (length < 3) {
        # indices for colouring
        color_tags <- sapply(
          idx_start:idx_end,
          function(idx) paste0('<span style="color:', colors[idx], '">', value, "</span>")
        )
        add <- paste(color_tags, collapse = ", ")
        modified_y <- c(modified_y, add)
      } else {
        # separate the run with "..." and add the color tags
        a <- paste0('<span style="color:', colors[idx_start], '">', value, "</span>")
        b <- paste0('<span style="color:', colors[idx_end], '">', value, "</span>")
        modified_y <- c(modified_y, a, "...", b)
      }
    }
    out <- paste(modified_y, collapse = ", ")
    return(out)
  }
  sapply(x, replace_and_colorise_single, USE.NAMES = FALSE)
}


replace_names <- function(x) {
  y_list <- strsplit(x, ", ")

  replace_names_single <- function(y) {
    if (length(y) > 3) {
      paste(y[1], "...", y[length(y)], collapse = ", ")
    } else {
      paste(y, collapse = ", ")
    }
  }

  result <- sapply(y_list, replace_names_single, USE.NAMES = FALSE)
  return(result)
}



replace_names(colorise(scenario_table$name))
replace_and_colorise(colorise(scenario_table$size))
test <- c("100, 100, 100, 100", "100, 100, 50, 50, 50, 100, 100, 100, 100")
x = replace_and_colorise(test)

# KABLE -------------------------------------------------------------------
kbl <- scenario_table %>%
  mutate(
    # remove duplicates if they appeared previously
    across(everything(), ~ ifelse(duplicated(.), "-", .)),

    # replace repeats within the cell
    across(
      c(size, delta, intro_n, r0),
      ~ replace_and_colorise(.)
    ),

    # replace name e.g. A,...,D
    across(
      c(name, intro_group),
      ~ replace_names(colorise(.))
    ),
  ) %>%
  rename_all(~ stringr::str_replace_all(.x, "_", " ")) %>%
  rename_all(~ stringr::str_to_title(.x)) %>%
  knitr::kable(escape = FALSE, format = "html") %>%
  # kable_classic_2() %>%
  kable_styling(
    full_width = FALSE,
    fixed_thead = TRUE,
    bootstrap_options = NULL,
    position = "center",
    font_size = 12,
    latex_options = "hold_position"
  ) %>%
  add_header_above(
    header = c(
      " " = 1,
      "Group Parameters" = 7,
      "Biological Parameters" = 2
    ),
    underline = TRUE,
    line = TRUE,
    font_size = 14
  ) %>%
  collapse_rows(
    columns = 1,
    valign = "middle",
    latex_hline = "major",
  ) %>%
  row_spec(0, bold = TRUE) %>%
  kableExtra::column_spec(1, bold = TRUE)

kbl

kbl %>% save_kable("analysis/data/scenarios.png", density = 300)
kbl %>% save_kable("analysis/data/scenarios.html")
