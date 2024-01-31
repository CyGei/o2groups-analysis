
# Function that splits data into k folds
split_data_kfold <- function(data, k, seed) {

  # Set the seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Get the number of rows in the dataframe
  n <- nrow(data)

  # Create an index vector with shuffled row numbers
  shuffled_indices <- sample(1:n)

  # Calculate the size of each fold
  fold_size <- floor(n / k)

  # Initialize an empty list to store the folds
  folds <- vector("list", length = k)

  # Split the data into k folds
  for (i in 1:k) {
    start_index <- (i - 1) * fold_size + 1
    end_index <- i * fold_size

    # For the last fold, include any remaining rows
    if (i == k) {
      end_index <- n
    }

    # Extract the indices for the current fold
    fold_indices <- shuffled_indices[start_index:end_index]

    # Assign the corresponding rows to the current fold
    folds[[i]] <- data[fold_indices, , drop = FALSE]
  }

  return(folds)
}


# Function that assigns the training and testing set from folded data
assign_train_test <- function(folds) {
  k <- length(folds)

  iter <- vector("list", length = k)

  for (i in 1:k) {
    test <- folds[[i]]
    train <- do.call(rbind, folds[-i])
    iter[[i]] <- list(train = train, test = test)
  }

  return(iter)
}

# Function that calculates the r2 on test data (not obvious!)
lm_metrics <- function(train, test, formula) {
  #https://stackoverflow.com/questions/25691127/r-squared-on-test-data
  dependent_var <- as.character(formula)[2]
  model <- lm(as.formula(formula), data = train)

  test.predictions <- predict(model, newdata = test)
  test.actual <- test[[dependent_var]]
  train.actual <- train[[dependent_var]]

  SS.test.total      <- sum((test.actual - mean(train.actual)) ^ 2)
  SS.test.residual   <- sum((test.actual - test.predictions) ^ 2)
  #SS.test.regression <- sum((predictions - mean(train.actual) ^ 2)
  test.rsq <- 1 - SS.test.residual / SS.test.total

  return(list(
    r2 = test.rsq
  ))
}


# Function that calculates metrics on test data for logistic regression
logistic_metrics <- function(train, test, formula) {
  #stop if 'weights' column doesn't exist
  if(!"weights" %in% colnames(train)){
    stop(" 'weights' column is missing")
  }

  dependent_var <- as.character(formula)[2]
  model <-
    glm(
      as.formula(formula),
      weights = weights,
      data = train,
      family = binomial(link = "logit")
    )

  test.predictions <- predict(model, newdata = test, type = "response")
  test.predictions_binary <- ifelse(test.predictions > 0.5, 1, 0)
  test.actual <- test[[dependent_var]]
  test.actual_binary <- ifelse(test.actual > 0.5 , 1, 0)

  confusion_matrix <- table(test.actual_binary, test.predictions_binary)
  # caret::confusionMatrix(factor(test.predictions_binary, levels = c(0, 1)),
  #                        factor(test.actual_binary, levels = c(0, 1)),
  #                        mode = "everything",
  #                        positive = "1")

  # Calculate metrics (if confusion matrix is 2 by 2)
  if(dim(confusion_matrix)[1] == 2 & dim(confusion_matrix)[2] == 2){
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
    recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
    f1_score <- 2 * precision * recall / (precision + recall)
  } else {
    warning("Confusion matrix is not 2 by 2")
    accuracy <- NA
    precision <- NA
    recall <- NA
    f1_score <- NA
  }
  McFaddenR2 <- 1 - (model$deviance / model$null.deviance)

  return(list(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    McFaddenR2 = McFaddenR2
  ))
}

# Cross validate for a single formula
cross_validate_helper <- function(formula, splits, method) {
  results <- vector("list", length = length(splits))
  # For each fold, fit the model and calculate the metric
  for (i in 1:length(splits)) {
    # Extract the training and testing data
    train <- splits[[i]]$train
    test <- splits[[i]]$test

    # Fit the model & return metrics
    if(method == "lm"){
      metric <- lm_metrics(train, test, formula)
    } else if(method == "logistic"){
      metric <- logistic_metrics(train, test, formula)
    } else {
      stop("method must be lm or logistic")
    }
    # Calculate the metric
    results[[i]] <- data.frame(
      iteration = i,
      metric = metric,
      formula = deparse1(formula)
    )

  }

  return(results)
}


# Cross validate for a list of formulas
cross_validate <- function(formulas, data, k = 10, seed = NULL, method = "lm") {
  # Split the data into k folds
  folds <- split_data_kfold(data, k, seed)

  # Assign the training and testing data
  splits <- assign_train_test(folds)

  # For each formula, cross validate
  results <- lapply(formulas, cross_validate_helper, splits = splits, method = method)

  # Combine the results
  results <- do.call(rbind, unlist(results, recursive = FALSE))
  results$formula_index <- match(results$formula, unique(results$formula))

  return(results)
}



