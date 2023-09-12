
x <- c("new", "carrot", "new", "apple", "apple", "orange", "orange", "orange", "orange", "banana", "banana", "apple", "apple", "apple", "carrot", "apple", "apple")

duplicates_to_ellipsis <- function(x) {
  # Detect consecutive duplicates using rle
  rle_result <- rle(x)

  # Initialize an empty vector to store the modified character vector
  modified_x <- c()

  # Loop through each run in the rle_result
  for (i in 1:length(rle_result$values)) {
    # Extract the value and length of the current element
    value <- rle_result$values[i]
    length <- rle_result$lengths[i]

    # If there are less than 3 consecutive duplicates, leave as is
    if (length < 3) {
      add <- rep(value, times = length)
      modified_x <- c(modified_x, add)
    } else { # else add one on the left and one on the right of "..."
      modified_x <- c(modified_x, value, "...", value)
    }
  }
  out <- paste(modified_x, collapse = ", ")
  return(out)
}

duplicates_to_ellipsis(x)



x = c("100, 100, 100, 100, 50, 100, 100, 100")

x <- colorise(x)

replace_repetitive_single <- function(x) {
  y <- remove_html_tags(x)
  y <- strsplit(y, ", ")[[1]]
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
      add <- rep(value, times = length)
      modified_y <- c(modified_y, add)
    } else { # else add one on the left and one on the right of "..."
      modified_y <- c(modified_y, value, "...", value)
    }
  }
  modified_y
  out <- paste(modified_y, collapse = ", ")
  return(out)
}
replace_repetitive_single(x)
colorise(modified_y)
modified_y
colors <-c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

lapply(modified_y, function(val) {
    index <- match(val, y)
    color <- ifelse(is.na(index), "black", colors[index])
    paste0('<span style="color:', color, ';">', val, '</span>')
  })
