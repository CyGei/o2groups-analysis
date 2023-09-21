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
set.seed(123)
init_split <- initial_validation_split(outcomes_df, prop = c(0.6, 0.2), strata = peak_coeff)
init_train <- training(init_split)
init_valid <- validation(init_split)
init_test <- testing(init_split)



# Build a workflowset ---------------------------------------------------------------
# workflowset combines pre-processors and models into a single object

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

glmnet_spec <- linear_reg() %>%
    set_engine("glmnet") %>%
    set_mode("regression")

# svm_spec <- svm_rbf() %>%
#     set_engine("kernlab") %>%
#     set_mode("regression")

rf_spec <- rand_forest() %>%
    set_engine("ranger") %>%
    set_mode("regression")

# xgb_spec <- boost_tree() %>%
#     set_engine("xgboost") %>%
#     set_mode("regression")


wflow_set <- workflow_set(
    preproc = list(
        normalization = base_recipe),
    models = list(
        lm = lm_spec,
        glmnet = glmnet_spec,
        rf = rf_spec
    )
)

process_time <- system.time({
clusters <- parallel::makePSOCKcluster(20)
doParallel::registerDoParallel(clusters)
set.seed(345)
wflow_tune_results <- wflow_set %>%
    workflow_map(
        fn = "fit_resamples", # computes a set of performance metrics across one or more resamples (no tuning)
        resamples = folds,
        metrics = metric_set(rmse, rsq),
        seed = 123
    )
parallel::stopCluster(clusters)
})

saveRDS(wflow_tune_results, here("analysis/simulation/data/model", "wflow_tune_results.rds"))
wflow_tune_results <- readRDS(here("analysis/simulation/data/model", "wflow_tune_results.rds"))

autoplot(wflow_tune_results)
wflow_tune_results %>% collect_metrics()

last_fit <-
