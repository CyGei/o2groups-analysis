library(tidyverse)
library(here)
library(tidymodels)
#https://workshops.tidymodels.org/
#https://learn.microsoft.com/en-us/training/paths/machine-learning-with-r/
#https://www.coursera.org/learn/tidyverse-modelling-data

# Original Data
outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

# Data Prep
data <- outcomes_df %>%
  filter(peak_coeff == 1) %>%
  drop_na(trials) %>%  # drops observations where groups doesn't generate transmissions
  mutate(
    rel_prec = abs(bias / delta) * 100,
    delta_type = case_when(
      delta == 0 ~ "neutral",
      delta > 0 ~ "assortative",
      delta < 0 ~ "disassortative",
      TRUE ~ "NA"
    ),
    abs_delta = abs(delta),
    n_trans = trials,
    n_trans_cat = ifelse(n_trans >= 30, ">=30", "<30"),
    n_cases_cat = ifelse(n_cases >= 30, ">=30", "<30"),
    size_cat = ifelse(size >= 50, ">=50", "<50")
  ) %>%
  group_by(scenario) %>%
  mutate(
    size_freq_cat = ifelse(size_freq >= 0.15, ">=0.15", "<0.15")
    ) %>%
  ungroup() %>%
  select(
    -c(
      peak_coeff,
      name,
      scenario,
      ppv,
      npv,
      trials ,
      successes,
      GT_mean,
      GT_sd,
      INCUB_mean,
      INCUB_sd
    )
  )


outcomes <-
  c("sensitivity", "specificity", "bias", "bias", "bias_type")

predictors <-c(
    "delta",
    "sd_delta",
    "size",
    "size_freq",
    "sd_size_freq",
    "intro_n",
    "r0",
    "n_cases",
    "total_cases",
    "n_groups",
    "sd_peak_date",
    "group_susceptibles",
    "total_susceptibles",
    "sd_group_susceptibles"
  )


# Modelling ---------------------------------------------------------------
set.seed(123)
split <- initial_split(data, prop = 0.8)
train <- training(split)
test <- testing(split)
folds <- vfold_cv(train, v = 10)
metrics <- metric_set(rmse, rsq)
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

source(here::here("analysis/simulation/script/modelling",
                  "compare_models.R"))


#################
# Bias
#################


bias_formulas <- list(
  bias ~  n_trans,
  bias ~ n_cases,
  bias ~ size_freq,
  bias ~ size,
  bias ~ log(size_freq) * size_freq_cat + log(n_trans) * n_trans_cat,
  bias ~ log(size_freq) * size_freq_cat + log(n_cases) * n_cases_cat,
  bias ~ log(size) * size_cat + log(n_cases) * n_cases_cat,
  bias ~ log(size_freq) * size_freq_cat + log(n_cases) * n_cases_cat + n_groups,
  make_formula(
    data = data,
    outcome = "bias",
    predictors = predictors,
    poly = 2
  )
)


bias_results <- compare_models(
  formulas = bias_formulas,
  model_spec = lm_spec,
  folds = folds,
  metrics = metrics
)

p_models <-
  bias_results %>%
  ggplot(aes(x = Formula, y = mean, color = Formula)) +
  facet_wrap( ~ .metric, scales = "free_y") +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  theme(axis.text.x = element_blank(),
        legend.position = "none")
plotly::ggplotly(p_models)

final_formula <- bias_results %>%
  filter(.metric == "rsq") %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(Formula)
final_formula <- bias ~ log(size_freq) * size_freq_cat + log(n_cases) * n_cases_cat

last_fit <- workflow() %>%
  add_model(lm_spec) %>%
  add_formula(as.formula(final_formula)) %>%
  last_fit(split)

last_fit %>%
  collect_metrics() # metrics from the test set

last_fit %>%
  extract_fit_engine() %>%
  summary() #metrics from the training set

# estimate plot
last_fit %>%
  extract_workflow() %>%
  tidy(conf.int = TRUE) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_flip()

last_fit %>%
  extract_fit_engine() %>% sjPlot::tab_model()


# plot predictions
last_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = .pred, y = bias)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Predicted", y = "Observed")

#################
# Sensitivity
#################


sensitivity_formulas <- list(
  car::logit(sensitivity) ~ group_susceptibles,
  car::logit(sensitivity) ~ total_susceptibles,
  car::logit(sensitivity) ~ delta,
  car::logit(sensitivity) ~ poly(delta, 2),
  car::logit(sensitivity) ~ delta_type + abs_delta,
  car::logit(sensitivity) ~ delta_type * abs_delta
  )
#logit_trans()
#glm binomial

data %>%
  select(sensitivity,
         group_susceptibles,
         total_susceptibles,
         sd_group_susceptibles,
         delta_type,
         abs_delta,
         delta) %>%
  pivot_longer(-c(sensitivity, delta_type),
               names_to = "predictor") %>%
  drop_na() %>%
  ggplot(aes(x = value, y = car::logit(sensitivity))) +
  facet_grid( ~ predictor, scales = "free") + #delta_type
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2)) +
  labs(x = "Predictor", y = "Sensitivity")

#plot the relationship between x value and logit(x)
tibble(x = seq(0, 1, 0.01)) %>%
  mutate(y = car::logit(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line()+
  labs(x = "x", y = "logit(x)")



sensitivity_results <- compare_models(
  formulas = sensitivity_formulas,
  model_spec = lm_spec,
  folds = folds,
  metrics = metrics
)

p_models <- sensitivity_results %>%
  ggplot(aes(x = Formula, y = mean, color = Formula)) +
  facet_wrap( ~ .metric, scales = "free_y") +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  theme(axis.text.x = element_blank(),
        legend.position = "none")
plotly::ggplotly(p_models)

# take best rsq model
final_formula <- sensitivity_results %>%
  filter(.metric == "rsq") %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(Formula)


last_fit <- workflow() %>%
  add_model(lm_spec) %>%
  add_formula(as.formula(final_formula)) %>%
  last_fit(split)

last_fit %>%
  collect_metrics()

last_fit %>%
  extract_fit_engine() %>% sjPlot::tab_model()

# plot predictions
last_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = .pred, y = `car::logit(sensitivity)`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Predicted", y = "Observed")

boot::inv.logit(-5)

# lm(as.formula(final_formula), data = train) %>%
#   summary()


#################
# Specificity
#################

specificity_formulas <- list(
  specificity ~ n_trans,
  specificity ~ n_cases,
  specificity ~ size_freq,
  specificity ~ sd_peak_date,
  specificity ~ group_susceptibles,
  specificity ~ total_susceptibles,
  specificity ~ sd_group_susceptibles
)

specificity_results <- compare_models(
  formulas = specificity_formulas,
  model_spec = lm_spec,
  folds = folds,
  metrics = metrics
)

p_models <- specificity_results %>%
  ggplot(aes(x = Formula, y = mean, color = Formula)) +
  facet_wrap( ~ .metric, scales = "free_y") +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  theme(axis.text.x = element_blank(),
        legend.position = "none")
plotly::ggplotly(p_models)

# take best rsq model
final_formula <- specificity_results %>%
  filter(.metric == "rsq") %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(Formula)


last_fit <- workflow() %>%
  add_model(lm_spec) %>%
  add_formula(as.formula(final_formula)) %>%
  last_fit(split)

last_fit %>%
  collect_metrics()

last_fit %>%
  extract_fit_engine() %>% sjPlot::tab_model()

# plot predictions
last_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = .pred, y = specificity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Predicted", y = "Observed")





#################
# Coverage
#################

coverage_formulas <- list(
  coverage ~ n_trans,
  coverage ~ n_cases,
  coverage ~ size_freq,
  coverage ~ sd_peak_date,
  coverage ~ group_susceptibles,
  coverage ~ total_susceptibles,
  coverage ~ sd_group_susceptibles
)

coverage_results <- compare_models(
  formulas = coverage_formulas,
  model_spec = lm_spec,
  folds = folds,
  metrics = metrics
)

p_models <- coverage_results %>%
  ggplot(aes(x = Formula, y = mean, color = Formula)) +
  facet_wrap( ~ .metric, scales = "free_y") +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  theme(axis.text.x = element_blank(),
        legend.position = "none")
plotly::ggplotly(p_models)

# take best rsq model
final_formula <- coverage_results %>%
  filter(.metric == "rsq") %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(Formula)


last_fit <- workflow() %>%
  add_model(lm_spec) %>%
  add_formula(as.formula(final_formula)) %>%
  last_fit(split)

last_fit %>%
  collect_metrics()

last_fit %>%
  extract_fit_engine() %>% sjPlot::tab_model()

# plot predictions
last_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = .pred, y = coverage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Predicted", y = "Observed")



#################
# relative precision
#################

# abs(delta - est / delta) * 100

# TREE --------------------------------------------------------------------
tree_predictors <-
  c(
    "n_trans",
    "size_freq",
    "n_groups",
    "beta",
    "abs_delta",
    "delta_type",
    "r0",
    "intro_prop"
  )
formula <- make_formula(data = train,
             outcome = "sensitivity",
             predictors = tree_predictors)

tree_spec <- decision_tree(
  cost_complexity = tune(), # Smaller values encourage more complex trees, while larger values favor simpler trees.
  tree_depth = tune(), # how many nodes
  min_n = tune() # how many required data points to split
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# create a grid of hyperparameters
# each row is a combination of hyperparameters
tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)
tree_grid



doParallel::registerDoParallel()
set.seed(345)
tree_rs <- tune_grid(
  tree_spec,
  formula,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(rmse, rsq, mae)
)

tree_rs

collect_metrics(tree_rs)

autoplot(tree_rs) + theme_light(base_family = "IBMPlexSans")


tree_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  arrange(mean)
select_best(tree_rs, "rmse")

best_model <- tree_rs %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  arrange(desc(mean)) %>%
  slice(1:20) %>%
  arrange(desc(cost_complexity), min_n, tree_depth)  %>%
  slice(1)

final_tree <- finalize_model(tree_spec, best_model)

final_rs <- last_fit(final_tree,  sensitivity ~ ., split)
final_rs %>% extract_workflow()
final_rs %>% extract_fit_engine() %>% summary()

final_rs %>%
  collect_metrics()

final_rs %>%
  collect_predictions() %>%
  ggplot(aes(x = .pred, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Predicted", y = "Observed")


library(vip)
fit(final_tree, sensitivity ~., train) %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))


library(parttree)

# Fit the tree model

# Filter data
filtered_train <- train %>%
  filter(delta != 0)
ex_fit <- fit(final_tree, sensitivity ~ size_freq + n_trans, filtered_train)

# Create the plot
filtered_train %>%
  ggplot(aes(n_trans, size_freq)) +
  geom_point(alpha = 0.7, aes(color = sensitivity)) +
  geom_parttree(data = ex_fit, aes(fill = sensitivity), alpha = 0.3) +
  scale_colour_viridis_c(aesthetics = c("color", "fill")) +
  coord_cartesian(xlim = range(filtered_train$n_trans),
                  ylim = range(filtered_train$size_freq))


