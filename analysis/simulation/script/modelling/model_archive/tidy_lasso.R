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

lasso_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

coverage_wf <- workflow() %>%
  add_recipe(coverage_rec) %>%
  add_model(lasso_spec)

lambda_grid <- grid_regular(penalty(), levels = 50)

doParallel::registerDoParallel()
set.seed(2020)
lasso_grid <- tune_grid(
  coverage_wf,
  resamples = folds,
  grid = lambda_grid,
  control = control_resamples(save_pred = TRUE)
)
autoplot(lasso_grid)

optimal_penalty <- lasso_grid %>%
  select_by_one_std_err(metric = "rmse", -penalty)


final_lasso <- finalize_workflow(
  coverage_wf,
  optimal_penalty
)

final_fit <- last_fit(final_lasso, split = split)
final_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = .pred, y = coverage)) +
  geom_point() +
  geom_abline()

final_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(-estimate)

library(vip)

final_fit%>%
  extract_fit_parsnip() %>%
  vi(lambda = optimal_penalty$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)+
  theme_classic()


