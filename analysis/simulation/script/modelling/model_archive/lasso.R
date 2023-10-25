library(tidyverse)
library(tidymodels)
library(here)

# Function to create a model for a given metric
create_metric_model <- function(metric_name) {
  outcomes_df <-
    readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

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
    "beta"
  )

  metric_formula <-
    as.formula(paste0(metric_name, "~", paste0(predictors, collapse = " + ")))

  # Split data
  metric_data <-
    outcomes_df %>% select(all_of(predictors), !!sym(metric_name))
  split <- initial_split(metric_data, prop = 0.6)
  train <- training(split)
  test <- testing(split)
  folds <- vfold_cv(train, v = 5)

  # Create a recipe
  metric_rec <- recipe(metric_formula, data = metric_data) %>%
    step_naomit(trials, all_outcomes()) %>%
    step_zv(all_predictors()) %>% # removes delta (delta = 0) for specificity
    step_normalize(all_predictors()) %>%
    step_corr(all_predictors(), threshold = 0.9) %>%
    step_poly(all_predictors(), degree = 2) %>%
    step_interact(term = ~ ends_with("poly_1"):ends_with("poly_1"))

  # Create a model specification
  lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")

  # Create a workflow
  metric_wf <- workflow() %>%
    add_recipe(metric_rec) %>%
    add_model(lasso_spec)

  lambda_grid <- grid_regular(penalty(), levels = 50)

  doParallel::registerDoParallel()
  set.seed(2020)

  # Tune the model
  metric_grid <- tune_grid(
    metric_wf,
    resamples = folds,
    grid = lambda_grid,
    control = control_resamples(save_pred = TRUE)
  )

  optimal_penalty <- metric_grid %>%
    select_by_one_std_err(metric = "rmse", -penalty)

  final_metric <- finalize_workflow(metric_wf,
                                    optimal_penalty)

  final_fit <- last_fit(final_metric, split = split)

  # Return the final fit and optimal penalty
  return(list(final_fit = final_fit, optimal_penalty = optimal_penalty))
}

# List of metrics
metrics <- c("coverage", "sensitivity", "specificity", "bias")
model_results <- list()
for (metric in metrics) {
  metric_model <- create_metric_model(metric)
  model_results[[metric]] <- metric_model
}


map(metrics, function(x){
  final_fit <- model_results[[x]]$final_fit
  performance_metrics <- final_fit %>% collect_metrics() %>%
  mutate(model = x)
}) %>%
  bind_rows() %>%
  select(.metric, .estimate, model) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 3))) %>%
  flextable::flextable()

map(metrics, function(x) {
  final_fit <- model_results[[x]]$final_fit
  optimal_penalty <- model_results[[x]]$optimal_penalty

  performance_metrics <- final_fit %>%
    extract_fit_parsnip() %>%
    vip::vi(lambda = optimal_penalty$penalty) %>%
    mutate(
      model = x,
      Importance = abs(Importance),
      Variable = fct_reorder(Variable, Importance)
    )
}) %>% bind_rows() %>%
  mutate(Variable_clean = gsub("_poly_1", "", Variable),
         Variable_clean = fct_reorder(Variable_clean, Importance)) %>%
  ggplot(aes(x = Importance, y = Variable_clean, fill = Sign)) +
  facet_wrap(~model, scales = "free_x")+
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  theme_classic()
