# Load the caret package
library(caret)
library(tidyverse)
library(here)
library(tidymodels)

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
  drop_na(trials)

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
    "successes",
    "beta",
    "peak_coeff"
  )

outcome <- "coverage"

df <- outcomes_df %>% select(all_of(c(outcome, predictors)))


set.seed(100)
split <- initial_split(df)
train <- training(split)
test <- testing(split)
trainControl <-
  caret::trainControl(method = "cv",
                      number = 10,
                      verboseIter = TRUE)

recipe <- recipe(as.formula(paste0(outcome, "~ .")) ,
                 data = df) %>%
  step_naomit(trials) %>%
  step_normalize(all_predictors()) %>%
  step_corr(all_predictors(), threshold = 0.9) %>%
  step_poly(all_predictors(), degree = 2) %>%
  step_interact(term = ~ ends_with("poly_1"):ends_with("poly_1"))

pred_idx <-
  recipe %>% prep() %>% juice() %>% select(-all_of(outcome)) %>% colnames()
outcome_idx <-
  recipe %>% prep() %>% juice() %>% select(all_of(outcome)) %>% colnames()

stepModel <- caret::train(
  x = recipe,
  data = train,
  method = "leapSeq",
  #leaps
  trControl = trainControl,
  tuneGrid = data.frame(nvmax = 5:25)
)
#nvmax informs on the max number of variables to include in the model
plot(stepModel)
stepModel
summary(stepModel)
stepModel$results
stepModel$finalModel
stepModel$bestTune
summary(stepModel$finalModel)
coef(stepModel$finalModel, 21)

selected_vars <- names(coef(stepModel$finalModel, 21))
selected_vars <- selected_vars[-1]
finalFormula <- as.formula(paste0(
  outcome, " ~ ", paste(selected_vars, collapse = " + ")
))

# rerun the model finalFormula on baked train data
train_baked <- recipe %>% prep() %>% bake(new_data = NULL)
finalModel <- lm(finalFormula, data = train_baked)
summary(finalModel)
# checking model assumptions
#performance::check_model(finalModel)




test_baked <- recipe %>% prep() %>% bake(new_data = test)
train_pred <- tibble(truth = train_baked$coverage,
                     pred = predict(finalModel, newdata = train_baked)) %>%
  mutate(split = "train")

test_pred <- tibble(truth = test_baked$coverage,
       pred = predict(finalModel, newdata = test_baked)) %>%
  mutate(split = "test")

p_preds <- bind_rows(train_pred, test_pred) %>%
  ggplot(aes(truth, pred, color = split)) +
  geom_point()+
  geom_abline() +
  labs(
    x = "Truth",
    y = "Predicted",
  )

performance::model_performance(finalModel)

perf_tab <- bind_rows(train_pred, test_pred) %>%
  group_by(split) %>%
  yardstick::metrics(truth = truth, estimate = pred) %>%
  mutate(across(where(is.numeric), ~round(., digits = 3)))


coeff_tab <- broom::tidy(finalModel) %>%
  mutate(term = gsub("_poly_1", "", term),
         term = gsub("_poly_2", "2", term)) %>%
  mutate(across(where(is.numeric), ~round(., digits = 3)))

coeff_kbl <- kableExtra::kable(coeff_tab,
                  format = "html",
                  caption = "Linear Regression Coefficients") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)

perf_kbl <- kableExtra::kable(perf_tab,
                  format = "html",
                  caption = "Performance Validation") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)







# # Create a custom modeling function
# customModel <- function(data, indices, predictor, outcome, ...) {
#   # Subset the data based on indices
#   data <- data[indices, ]

#   # Create the formula
#   formula <- as.formula(paste(outcome, "~", predictor))

#   # Fit the linear model
#   model <- lm(formula, data = data)

#   # Return the model
#   return(model)
# }

# # Set up the control parameters for cross-validation
# ctrl <- trainControl(
#   method = "cv",  # Cross-validation method
#   number = 5,     # Number of cross-validation folds
#   verboseIter = TRUE
# )

# # Perform stepwise selection with cross-validation
# set.seed(123)  # Set a random seed for reproducibility
# stepwise_result <- train(
#   x = df[, predictors],
#   y = df[, outcome_variable],
#   method = "none",  # We use "none" because the customModel function handles modeling
#   trControl = ctrl,
#   tuneGrid = data.frame(predictor = predictors),  # Grid of predictors to consider
#   allowParallel = TRUE,  # Enable parallel processing (if available)
#   methodArgs = list(outcome = outcome_variable),  # Pass the outcome variable to the custom model
#   preProcess = c("center", "scale")  # Optional: Preprocess predictors
# )

# # Print the results
# print(stepwise_result)
