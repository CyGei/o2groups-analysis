library(here)
library(tidyverse)
library(tidymodels)

# load data
model_df <-
  readRDS(here("analysis/simulation/data/model", "model_df.rds")) %>%
  drop_na() # drops observations where the simulation didn't generate cases

# Model Bias using multilevel modeling (levels are scenario, simulation, group)

# Define the workflow
bias_wf <- workflow() %>%
  add_formula(bias ~ .) %>%
  add_model(lme4::glmer, formula = bias ~ (1 | scenario) + (1 | simulation) + (1 | group), data = model_df)

# Define the model grid
model_grid <- grid_lmer(
  penalty = c(0, 0.1, 0.01),
  mixture = c(0, 0.5, 1),
  optimizer = c("bobyqa", "Nelder_Mead")
)

# Define the workflow set
bias_wfs <- workflow_set(
  preprocessor = NULL,
  models = model_grid,
  cross = 5
)

# Fit the workflow set
bias_res <- bias_wfs %>%
  add_workflow(bias_wf) %>%
  fit_resamples(
    resamples = vfold_cv(model_df, v = 5),
    metrics = metric_set(rmse, rsq),
    control = control_resamples(save_pred = TRUE)
  )

# Get the best model
best_model <- select_best(bias_res, "rmse")

# Get the predictions
predictions <- collect_predictions(best_model)



# LME4 --------------------------------------------------------------------

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
    "peak_coeff"
  )
metrics <- c("coverage", "bias", "significance")

# lmer(y ~ x + (1 | random-effect), data = my_data) -> an intercept for each rand effect group
# (countinuous_predictor | random_effect_group) for a random-effect slope.-> a different slope for each rand effect group
# a random-effect slope may be estimated for each group using (slope | group)
test_data <- model_df %>%
  filter(scenario %in% unique(scenario)[1:12]) %>%
  mutate(intro_prop = intro_n / size,
    across(all_of(predictors), ~ as.numeric(scale(.x)))) %>%
  as_tibble()

# show any rows with NA values in any column
test_data %>%
  filter(if_any(everything(), is.na))

#plot y = bias, x = delta, color = scenario
ggplot(test_data, aes(x = delta, y = bias,
                      color = peak_coeff)) +
  facet_wrap(~scenario, scales = "free")+
  geom_hline(aes(yintercept = 0))+
  geom_point() +
  theme_classic()+
  theme(legend.position = "none")

library(lme4)
library(lmerTest)
#This model treats name as a nested random effect within scenario, allowing for different sets of names within each scenario.


m1 <-
  lmerTest::lmer(
    bias ~ delta + n_groups + size + intro_prop + r0 + GT_mean + GT_sd +
      INCUB_mean + INCUB_sd + trials + successes + peak_coeff +
      (1 | scenario:name) +
      (1 | peak_coeff),
    data = test_data
  )
summary(m1)


m2 <-
  lmerTest::lmer(
    bias ~ delta + size + r0  + trials + successes + peak_coeff +
      (1 | scenario:name) +
      (1 | peak_coeff),
    data = test_data
  )
summary(m2)

anova(m1, m2)


#use model 2 to predict bias and ggplot predicted vs true
#group by interaction of name and peak_coeff to have one line

test_data %>%
  mutate(pred = predict(m2)) %>%
  ggplot(aes(x = pred, y = bias,
             shape = name,
             color = as.factor(peak_coeff))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm",
              se = FALSE,
              aes(group = interaction(scenario, name, peak_coeff)))+

  #geom_line(aes(group = interaction(scenario, name, peak_coeff))) +
  facet_wrap(~scenario, scales = "free") +
  theme_classic() +
  theme(legend.position = "none")


test_data %>%
  mutate(pred = predict(m2)) %>%
  filter(scenario %in% unique(scenario)[1]) %>%
  filter(name == "A") %>%
  ggplot(aes(x = pred, y = bias,
             color = as.factor(peak_coeff))) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  theme(legend.position = "none")

