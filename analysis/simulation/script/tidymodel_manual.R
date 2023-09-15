# 1. Complete modeling workflow

# 2. Data resampling
#  initial_split() function to create a data split object from our leads_df data.
# Then we use the training() and testing() functions to create our training and test datasets.

# 3. Model specification
# Next, we specify our logistic regression model with the logistic_reg() function, setting the engine to glm and mode to classification.

# 4. Feature engineering
# The next step is to build our feature engineering pipeline. For the lead scoring data, we specify a recipe object that labels purchased as the outcome variable and the remaining columns in leads_training as predictors. For our preprocessing steps, we add a correlation filter with a threshold of 0 point 9, normalize all numeric predictors, and create dummy variables for all nominal predictors.

# 5. Recipe training
# We then train our recipe with the prep() function and the leads_training data. Now it can be used to transform our training and test datasets for modeling.

# 6. Preprocess training data
# We apply our trained recipe to the training data and store the results in leads_training_prep. We see from the output, that the transformations were processed correctly. Numeric variables are normalized, pages_per_visit has been removed, and dummy variables have been created.

# 7. Preprocess test data
# Next, we transform our test dataset using our trained recipe and store the results in leads_test_prep.

# 8. Model fitting and predictions
# We then train our logistic regression model using the leads_training_prep data. Once the model is fit, we can obtain model predictions with the predict() function. For predicted outcome values we provide type is equal to class to the predict function. For estimated probabilities, we provide type is equal to prob. In both cases, however, we must set new_data equal to the preprocessed test dataset, leads_test_prep.

# 9. Combining prediction results
# As in our prior modeling workflows, we combine the actual outcome variable from the test dataset, leads_test, with the datasets of predictions using bind_cols(). This produces a model results data frame with all the required columns for yardstick metric functions.

# 10. Model evaluation
# The final step is model evaluation. Using our leads_results data we can calculate a confusion matrix, sensitivity, specificity, or any other metrics that we covered in chapter 2. The difference in this modeling workflow, is that we were able to incorporate feature engineering and use all available predictor variables in the lead scoring dataset.


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
    drop_na()
str(outcomes_df)
outcomes_clean <- outcomes_df %>%
    select(all_of(c(outcome, predictors)))

set.seed(123)
init_split <- initial_split(outcomes_clean, prop = 0.6, strata = peak_coeff)
init_train <- training(init_split)
init_test <- testing(init_split)

set.seed(234)
folds <- vfold_cv(init_train, v = 10, strata = peak_coeff)


recipe <- recipe(
    delta ~ .,
    data = init_train
) %>%
    step_normalize(all_predictors())
recipe

rf_spec <- rand_forest() %>%
    set_engine("ranger") %>%
    set_mode("regression")

xgb_spec <- boost_tree() %>%
    set_engine("xgboost") %>%
    set_mode("regression")



rf_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(rf_spec)

xgb_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(xgb_spec)

rf_results <- rf_workflow %>%
last_fit(
    split = init_split,
    metrics = metric_set(rmse, rsq))

xgb_results <- xgb_workflow %>%
last_fit(
    split = init_split,
    metrics = metric_set(rmse, rsq))




outcomes_df%>%
  mutate(est = delta - bias) %>%
  rmse(truth = delta, estimate = est)

outcomes_df%>%
  mutate(est = delta - bias) %>%
  rsq(truth = delta, estimate = est)

rf_preds %>%
    ggplot(aes(x = delta, y = .pred)) +
    geom_point() +
    geom_abline() +
    labs(x = "Actual", y = "Rf Predicted")

rf_preds %>%
    ggplot(aes(x = delta - .pred)) +
    geom_histogram() +
    labs(x = "Residuals", y = "Count")


outcomes_df%>%
  mutate(est = delta - bias) %>%
    ggplot(aes(x = delta, y = est)) +
    geom_point() +
    geom_abline() +
    labs(x = "Actual", y = "Cyril Predicted")


