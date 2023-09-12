# Scenario Dataframe ------------------------------------------------------
# x is a distcrete object
pmf_to_mu <- function(x) {
    sum(x$d(0:100) * (0:100))
}
pmf_to_sd <- function(x) {
    sqrt(sum(x$d(0:100) * (0:100)^2) - pmf_to_mu(x)^2)
}

read_files <- function(path) {
  files <- list.files(path = path, pattern = "*.rds", full.names = TRUE)
  files <- furrr::future_map(files, readRDS, .options = furrr_options(seed = NULL))
  return(files)
}

scenarios <- read_files(here("analysis/manual/data", "scenarios"))

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
        scaled_delta = paste0(round(o2groups::scale(scenario$delta), digits = 2), collapse = ", "),
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


# Processing Functions -----------------------------------------------------------

replace_and_colorise <- function(x) {
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

    process_single_element <- function(element) {
        # Split the element by comma
        values <- strsplit(element, ", ")[[1]]
        n <- length(values)

        # Check if the element contains only letters
        if (all(grepl("^[[:alpha:]]+$", values, ignore.case = TRUE))) {
            # If length is less than 3, return as is with colour
            if (n < 3) {
                color_tags <- sapply(
                    seq_along(values),
                    function(idx) paste0('<span style="color:', colors[idx], '">', values[idx], "</span>")
                )
                out <- paste(color_tags, collapse = ", ")
                return(out)
            } else {
                # If length is more than 3, return first and last with "..." and colour
                a <- paste0('<span style="color:', colors[1], '">', values[1], "</span>")
                b <- paste0('<span style="color:', colors[n], '">', values[n], "</span>")
                out <- paste(a, "...", b, collapse = ", ")
                return(out)
            }
        }
        # else process as numbers
        else {
            # Detect consecutive duplicates using rle
            rle_result <- rle(values)

            modified_values <- c()
            for (i in seq_along(rle_result$values)) {
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
                    modified_values <- c(modified_values, add)
                } else {
                    # Separate the run with "..." and add the color tags
                    a <- paste0('<span style="color:', colors[idx_start], '">', value, "</span>")
                    b <- paste0('<span style="color:', colors[idx_end], '">', value, "</span>")
                    modified_values <- c(modified_values, a, "...", b)
                }
            }
            out <- paste(modified_values, collapse = ", ")
            return(out)
        }
    }
    sapply(x, process_single_element, USE.NAMES = FALSE)
}


# Return Kable Table -----------------------------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)

kbl <- scenario_table %>%
    mutate(
        # remove duplicates if they appeared previously
        across(everything(), ~ ifelse(duplicated(.), "-", .)),

        # replace repeats within the cell
        across(
            c(size, name, delta, scaled_delta, intro_group, intro_n, r0),
            ~ replace_and_colorise(.)
        )
    ) %>%
    rename_all(~ stringr::str_replace_all(.x, "_", " ")) %>%
    rename_all(~ stringr::str_to_title(.x)) %>%
    knitr::kable(escape = FALSE, format = "html") %>%
    # kable_classic_2() %>%
    kableExtra::kable_styling(
        full_width = FALSE,
        fixed_thead = TRUE,
        bootstrap_options = NULL,
        position = "center",
        font_size = 12,
        latex_options = "hold_position"
    ) %>%
    kableExtra::add_header_above(
        header = c(
            " " = 1,
            "Group Parameters" = 8,
            "Biological Parameters" = 2
        ),
        underline = TRUE,
        line = TRUE,
        font_size = 14
    ) %>%
    kableExtra::collapse_rows(
        columns = 1,
        valign = "middle",
        latex_hline = "major",
    ) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE)
kbl

# kbl %>% kableExtra::save_kable("analysis/data/scenarios.png", density = 300)
# kbl %>% kableExtra::save_kable("analysis/data/scenarios.html")
