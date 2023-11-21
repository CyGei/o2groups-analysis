library(tidymodels)

data(iris)
set.seed(345)
split <- initial_split(iris, prop = 0.75, strata = Species)
train <- training(split)
test <- testing(split)
folds <- vfold_cv(train, v = 10)


# Recipe where we specify the formula
base_recipe <-
  recipe(Sepal.Length ~ ., data = iris)

# Specify the linear regression model
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Create a workflow
lm_workflow <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(lm_spec)

# Performance metrics
metric <- metric_set(rmse, rsq)

# Fit and evaluate the model on folds
results <-
  fit_resamples(lm_workflow, resamples = folds, metrics = metric)

collect_metrics(results)
results %>% performance::check_model()

# Fit the model to the whole training data & test on testing data
final_fit <- last_fit(lm_workflow, split)
final_fit %>%
  collect_metrics()
extract_fit_engine(final_fit) %>%
  summary()





# TRY DIFFERENT FORMULAS --------------------------------------------------

# Define candidate formulas
formulas <- list(
  Sepal.Length ~ .,                     # Base formula (all predictors)
  Sepal.Length ~ Sepal.Width + Petal.Length,  # Reduced formula (some predictors)
  Sepal.Length ~ Sepal.Width * Petal.Length,  # Formula with interaction
  Sepal.Length ~ poly(Petal.Length, 2)     # Formula with polynomial term
)

results_list <- vector("list", length(formulas))
names(results_list) <- formulas

set.seed(345)
split <- initial_split(iris, prop = 0.75, strata = Species)
train <- training(split)
test <- testing(split)
folds <- vfold_cv(train, v = 10)
metric <- metric_set(rmse, rsq)


# Loop over candidate formulas
for (i in 1:length(formulas)) {
  # Create a new workflow
  workflow <- workflow() %>%
    add_formula(formulas[[i]]) %>%
    add_model(lm_spec)

  # Fit and evaluate the model on folds
  results <-
    fit_resamples(workflow, resamples = folds, metrics = metric)

  # Store the results
  results_list[[i]] <- results

}

performances <- map(results_list, collect_metrics) %>%
  bind_rows(., .id = "Formula")

# Plot the results
performances %>%
  ggplot(aes(x = Formula, y = mean, color = Formula)) +
  facet_wrap(~.metric, scales = "free_y") +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  theme(axis.text.x = element_blank())
