library(caret)
library(tidyverse)
library(here)
library(tidymodels)
library(yardstick)
library(kableExtra)

run_linear_model <- function(outcome) {
  cat(outcome, "\n")
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
  df <- outcomes_df %>% select(all_of(c(outcome, predictors))) %>%
    drop_na(all_of(outcome))

  # Set seed and split data
  set.seed(100)
  split <- initial_split(df)
  train <- training(split)
  test <- testing(split)

  # Define train control
  trainControl <- caret::trainControl(
    method = "cv",
    number = 10,
    verboseIter = FALSE
  )

  # Create recipe
  recipe <- recipe(
    as.formula(paste0(outcome, "~ .")),
    data = train
  ) %>%
    step_naomit(trials) %>%
    step_normalize(all_predictors()) %>%
    step_zv(all_predictors()) %>%
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
#binomal check for sensitivity family binomial
  # Extract selected variables
  selected_vars <- names(stats::coef(stepModel$finalModel, pull(stepModel$bestTune)))
  selected_vars <- selected_vars[-1] #removes intercept

  # Create the final formula
  finalFormula <- as.formula(paste0(
    outcome, " ~ ", paste(selected_vars, collapse = " + ")
  ))

  # Fit the final linear model to training data
  train_baked <- recipe %>% prep() %>% bake(new_data = NULL)
  finalModel <- lm(finalFormula, data = train_baked)

  # Predictions
  train_pred <- tibble(truth = pull(train_baked[, outcome]),
                       pred = predict(finalModel, newdata = train_baked)) %>%
    mutate(split = "train")
  test_baked <- recipe %>% prep() %>% bake(new_data = test)
  test_pred <- tibble(truth = pull(test_baked[, outcome]),
                      pred = predict(finalModel, newdata = test_baked)) %>%
    mutate(split = "test")

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
  out <- list(
    outcome = outcome,
    stepModel = stepModel,
    finalModel = finalModel,
    finalFormula = finalFormula,
    plot_preds = p_preds,
    performance_table = perf_kbl,
    coefficients_table = coeff_kbl
  )
  #save
  saveRDS(out, here("analysis/simulation/data/model", paste0(outcome,"_lm", ".rds")))
  return(out)
}
#specificity <- run_linear_model("specificity")

outcomes <- c("coverage", "sensitivity", "specificity", "bias")
# lm_models <- purrr::map(outcomes, run_linear_model)
# saveRDS(lm_models, here("analysis/simulation/data/model", "lm_models.rds"))

library(furrr)
plan(multisession, workers = length(outcomes))
lm_models <- furrr::future_map(outcomes, run_linear_model,
                               .options=furrr_options(seed=TRUE))
saveRDS(lm_models, here("analysis/simulation/data/model", "lm_models.rds"))


# plot(coverage_lm[["stepModel"]])
# library(ggfortify)
# autoplot(coverage_lm[["finalModel"]])
# coverage_lm[["plot_preds"]]
# coverage_lm[["performance_table"]]
