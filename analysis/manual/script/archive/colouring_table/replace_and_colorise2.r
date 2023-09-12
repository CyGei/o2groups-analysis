
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
