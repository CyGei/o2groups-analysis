#replace and colorise

colors <-c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

x = c("100, 100, 100, 100, 50, 100, 100, 100")
y = c("new", "carrot", "new", "apple", "apple", "orange", "orange", "orange", "orange", "banana", "banana", "apple", "apple", "apple", "carrot", "apple", "apple")
z = c(x, y)




replace_and_colorise_single <- function(x) {
    
    colors <-c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

  y <- strsplit(x, ", ")[[1]]
  # Detect consecutive duplicates using rle
  rle_result <- rle(y)

  # Initialize an empty vector to store the modified character vector
  modified_y <- c()

  # Loop through each run in the rle_result
  for (i in 1:length(rle_result$values)) {
    # Extract the value and length of the current element
    value <- rle_result$values[i]
    length <- rle_result$lengths[i]

    # If there are less than 3 consecutive duplicates, leave as is
    if (length < 3) {
        # indices for colouring
        idx_end <- sum(rle_result$lengths[1:i])
        idx_start <- idx_end - length + 1
        color_tags <- sapply(
            idx_start:idx_end,
            function(idx) paste0('<span style="color:', colors[idx], '">', value, "</span>")
        )
        add <- paste(color_tags, collapse = ", ")
        modified_y <- c(modified_y, add)
    } else { 
    # separate the run with "..." and add the color tags
    idx_end <- sum(rle_result$lengths[1:i])
    idx_start <- idx_end - length + 1
    a <- paste0('<span style="color:', colors[idx_start], '">', value, '</span>')
    b <- paste0('<span style="color:', colors[idx_end], '">', value, '</span>')
    modified_y <- c(modified_y, a, "...", b)
    }
  }
  out <- paste(modified_y, collapse = ", ")
  return(out)
}
replace_and_colorise(y)



replace_and_colorise<-function(x) {
    sapply(x, replace_and_colorise_single)
    }


replace_and_colorise(scenario_table$size)
