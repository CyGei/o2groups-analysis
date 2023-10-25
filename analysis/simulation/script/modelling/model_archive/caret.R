library(caret)
library(tidyverse)
library(here)
library(tidymodels)
library(yardstick)
library(kableExtra)

run_linear_model <- function(outcome_var) {
  # Load the data
  outcomes_df <- readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
    drop_na(trials)

  # Define predictors and outcome
  predictors <- c(
    "delta",
    "n_groups",
    "size",
    "intro_prop",
    "r0",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd",
    "trials",
    "successes",
    "beta",
    "peak_coeff"
  )

  outcome <- outcome_var

  df <- outcomes_df %>% select(all_of(c(outcome, predictors)))

  # Set seed and split data
  set.seed(100)
  split <- initial_split(df)
  train <- training(split)
  test <- testing(split)

  # Define train control
  trainControl <- caret::trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE
  )

  # Create recipe
  recipe <- recipe(
    as.formula(paste0(outcome, "~ .")),
    data = df
  ) %>%
    step_naomit(trials) %>%
    step_normalize(all_predictors()) %>%
    step_corr(all_predictors(), threshold = 0.9) %>%
    step_poly(all_predictors(), degree = 2) %>%
    step_interact(term = ~ ends_with("poly_1"):ends_with("poly_1"))

  # Fit the stepwise model
  stepModel <- caret::train(
    x = recipe,
    data = train,
    method = "leapSeq",
    trControl = trainControl,
    tuneGrid = data.frame(nvmax = 5:25)
  )

  # Extract selected variables
  selected_vars <- names(coef(stepModel$finalModel, 21))
  selected_vars <- selected_vars[-1]

  # Create the final formula
  finalFormula <- as.formula(paste0(
    outcome, " ~ ", paste(selected_vars, collapse = " + ")
  ))

  # Fit the final linear model
  train_baked <- recipe %>% prep() %>% bake(new_data = NULL)
  finalModel <- lm(finalFormula, data = train_baked)

  # Checking model assumptions
  check_normality <- performance::check_normality(finalModel)
  check_heteroscedasticity <- performance::check_heteroscedasticity(finalModel)

  # Create a plot
  p_preds <- bind_rows(train_pred, test_pred) %>%
    ggplot(aes(truth, pred, color = split)) +
    geom_point() +
    geom_abline() +
    labs(
      x = "Truth",
      y = "Predicted",
    )

  # Model performance
  perf_tab <- bind_rows(train_pred, test_pred) %>%
    group_by(split) %>%
    yardstick::metrics(truth = truth, estimate = pred) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 3)))

  # Coefficients table
  coeff_tab <- broom::tidy(finalModel) %>%
    mutate(term = gsub("_poly_1", "", term),
           term = gsub("_poly_2", "2", term)) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 3)))

  # Create HTML tables
  coeff_kbl <- kableExtra::kable(
    coeff_tab,
    format = "html",
    caption = "Linear Regression Coefficients"
  ) %>%
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)

  perf_kbl <- kableExtra::kable(
    perf_tab,
    format = "html",
    caption = "Performance Validation"
  ) %>%
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)

  # Return the final model and tables
  return(list(
    finalModel = finalModel,
    check_normality = check_normality,
    check_heteroscedasticity = check_heteroscedasticity,
    plot = p_preds,
    performance_table = perf_kbl,
    coefficients_table = coeff_kbl
  ))
}

# Example usage:
result <- run_linear_model("coverage")
