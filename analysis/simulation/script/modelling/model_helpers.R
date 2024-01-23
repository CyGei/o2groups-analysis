library(tidymodels)
library(purrr)

# function for model comparison
compare_models <- function(formulas, model_spec, folds, metrics) {
  results_list <- vector("list", length(formulas))
  names(results_list) <- formulas

  # Loop over candidate formulas
  for (i in 1:length(formulas)) {
    # Create a new workflow
    workflow <- workflow() %>%
      add_formula(formulas[[i]]) %>%
      add_model(model_spec)

    # Fit and evaluate the model on folds
    results <- fit_resamples(workflow, resamples = folds, metrics = metrics)

    # Store the results
    results_list[[i]] <- results
  }

  performances <- map(results_list, collect_metrics) %>%
    bind_rows(., .id = "Formula")

  return(performances)
}

# # Example usage:
# formulas <- list(
#   Sepal.Length ~ .,                     # Base formula (all predictors)
#   Sepal.Length ~ Sepal.Width + Petal.Length,  # Reduced formula (some predictors)
#   Sepal.Length ~ Sepal.Width * Petal.Length,  # Formula with interaction
#   Sepal.Length ~ poly(Petal.Length, 2)     # Formula with polynomial term
# )
# set.seed(345)
# split <- initial_split(iris, prop = 0.75, strata = Species)
# train <- training(split)
# test <- testing(split)
# folds <- vfold_cv(train, v = 10)
# metrics <- metric_set(rmse, rsq)
#
# performances <- compare_models(formulas, folds, metric)
#
# # Plot the results
# performances %>%
#   ggplot(aes(x = Formula, y = mean, color = Formula)) +
#   facet_wrap(~.metric, scales = "free_y") +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
#   theme(axis.text.x = element_blank())



make_formula <- function(data, outcome, predictors, poly = 1) {
  if (poly == 1) {
    formula_str <- paste(outcome, "~", paste(predictors, collapse = "+"))
  } else {
    poly_predictors <- lapply(predictors, function(predictor) {
      if (is.numeric(data[[predictor]])) {
        paste("poly(", predictor, ",", poly, ")", sep = "")
      } else {
        predictor
      }
    })
    formula_str <- paste(outcome, "~", paste(poly_predictors, collapse = "+"))
  }
  as.formula(formula_str)
}

#make_formula(data = data, outcome = "abs_bias", predictors = predictors, poly = 2)
