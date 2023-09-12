library(tidymodels)
library(here)
outcomes_df <-
    readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

outcomes_df %>%
    sample_n(1000) %>%
    summary()
str(outcomes_df)

# Split Data ----------------------------------------------------------------
split <- initial_split(outcomes_df, prop = 0.75) # strata = peak_coeff
train <- training(split)
test <- testing(split)

# Preprocess Data -----------------------------------------------------------
recipe <- recipe(delta ~ ., data = train) %>%
    step_normalize(all_predictors()) %>%
    prep()

# set sampling procedure using vfold_cv (k fold)
kfold <- vfold_cv(train, v = 10, strata = peak_coeff)
kfold

# specify model
random_forest <-
    rand_forest(
        mtry = tune(),
        trees = 1000,
        min_n = tune(),
        mode = "regression",
        engine = "ranger"
    ) %>%
    set_engine("ranger") %>%
    set_mode("regression")

# mpdel procedure
workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(random_forest)

# tune model
tuned <- tune_grid(workflow, resamples = kfold, grid = 10)

best_tune <- tuned %>% select_best("rmse")

# Finalise model ------------------------------------------------------------
finalize_model(random_forest,
best_tune)
