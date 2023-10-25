library(tidyverse)
library(tidymodels)
library(here)

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

predictors <-
  c(
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
    "successes"
  )
metrics <- c("coverage", "bias", "sensitivity", "specificity")
formulas <- list(
  coverage = as.formula(paste0(
    "coverage", "~", paste0(predictors, collapse = " + ")
  )),
  sensitivity = as.formula(paste0(
    "sensitivity", "~", paste0(predictors, collapse = " + ")
  )),
  specificity = as.formula(paste0(
    "specificity", "~", paste0(predictors, collapse = " + ")
  ))
)
# total terms should be:
length(predictors) * 2 + choose(n = length(predictors), k = 2)

# Coverage ----------------------------------------------------------------
coverage <- outcomes_df %>% select(coverage, all_of(predictors))
split <- initial_split(coverage, prop = 0.6)
train <- training(split)
test  <- testing(split)
folds <- vfold_cv(train, v = 5)

coverage_rec <- recipe(coverage ~ . , data = coverage) %>%
  step_naomit(trials) %>%
  step_normalize(all_predictors()) %>%
  step_corr(all_predictors(), threshold = 0.9) %>%
  step_poly(all_predictors(), degree = 2) %>%
  step_interact(term = ~ ends_with("poly_1"):ends_with("poly_1"))

lm_spec <-
  linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

coverage_wf <- workflow() %>%
  add_recipe(coverage_rec) %>%
  add_model(lm_spec)


doParallel::registerDoParallel()
set.seed(2020)
#fit resamples
coverage_fit <- fit_resamples(
  coverage_wf,
  resamples = folds,
  metrics = metric_set(rmse, rsq),
  control = control_resamples(save_pred = TRUE)
)
coverage_fit
coverage_fit %>% collect_metrics()
