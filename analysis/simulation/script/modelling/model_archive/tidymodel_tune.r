# Tidymodelling
library(tidymodels)
library(here)
library(doParallel)

predictors <-
  c(
    "n_groups",
    "size",
    "intro_n",
    "r0",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd",
    "trials",
    "peak_coeff"
    #delta
  )
metrics <- c("coverage", "bias", "significance")
outcome <- "delta"

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
  select(all_of(c(outcome, predictors))) %>%
  drop_na()
str(outcomes_df)

outcomes_df %>%
  sample_n(1000) %>%
  summary()

# Split Data, rsample:: ----------------------------------------------------------------
set.seed(123)
init_split <- initial_split(outcomes_df, prop = 0.7, strata = peak_coeff)
init_train <- training(init_split)
init_test <- testing(init_split)


set.seed(234)
folds <- vfold_cv(init_train, v = 10, strata = peak_coeff)
folds


# Feature Engineering, recipies:: -----------------------------------------------------------
base_recipe <- recipe(
  formula = delta ~ .,
  data = init_train
) %>%
  step_normalize(all_predictors())
# step_range(all_predictors(), min = 0, max = 1)

base_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>%
  summary()

# Model Specification, parsnip:: ------------------------------------------------
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

glmnet_spec <- linear_reg(
  penalty = tune(),
  mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

rf_spec <- rand_forest(
  trees = 500,
  min_n = tune()
  ) %>%
  set_engine("ranger",
  num.threads = 20, importance = "impurity") %>%
  set_mode("regression")

xgb_spec <- boost_tree(
  trees = 500,
  min_n = tune(),
  mtry = tune(),
  stop_iter = tune(),
  learn_rate = 0.01
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


wflow_set <- workflow_set(
  preproc = list(base_recipe),
  models = list(
    linear = lm_spec,
    glmnet = glmnet_spec,
    rf = rf_spec,
    xbg = xgb_spec
  )
)

process_time <- system.time({
doParallel::registerDoParallel(cores = 20)
set.seed(345)
wflow_results <- wflow_set %>%
  workflow_map(
    fn = "tune_race_anova",
    resamples = folds,
    grid = 10,
    metrics = metric_set(rmse, rsq),
    seed = 123,
    verbose = TRUE
  )
saveRDS(wflow_results, here("analysis/simulation/data/model", "wflow_results.rds"))
})
process_time

autoplot(wflow_results)
autoplot(wflow_results, id = "recipe_rf", metric = "rmse")
rank_results(wflow_results, rank_metric = "rsq", select_best = TRUE)
wflow_results %>% collect_metrics() %>% print(n = 22)

autoplot(wflow_results, id = "recipe_rf", metric = "rmse")


best_results <- wflow_results %>% 
   extract_workflow_set_result("recipe_rf") %>% 
   select_best(metric = "rmse")

rf_test_results <- 
   wflow_results %>% 
   extract_workflow("recipe_rf") %>% 
   finalize_workflow(best_results) %>% 
   last_fit(split = init_split)

collect_metrics(rf_test_results)


rf_test_results %>% 
   collect_predictions() %>% 
   ggplot(aes(x = delta, y = .pred)) + 
   geom_abline(color = "gray50", lty = 2) + 
   geom_point(alpha = 0.5) + 
   coord_obs_pred() + 
   labs(x = "observed", y = "predicted")

#variable importance
rf_test_results %>% 
   pull_workflow_fit() %>% 
   vip::vip(geom = "point", fill = "lightblue", alpha = 0.5)
 rf_test_results %>% 
 extract_fit_parsnip() %>% 
 vip::vip(num_features = 20)
