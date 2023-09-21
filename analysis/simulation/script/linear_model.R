library(here)
library(car)        # extracts model results
library(MASS)       # provides "birthwt" dataset
library(ISLR)       # provides "Wage" dataset
library(tictoc)     # checks running time
library(sjPlot)     # visualizes model results
library(glmulti)    # finds the BEST model
library(flextable)  # beautifies tables
library(tidyverse)  # provides a lot of useful stuff !!!
library(performance)# checks and compares quality of models


predictors <-
  c(
    "delta",
    "n_groups",
    "size",
    "intro_n",
    "r0",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd",
    "trials",
    "successes"
  )
metrics <- c("coverage", "bias", "significance")

interactions <- apply(expand.grid(predictors, predictors), 1, paste, collapse = ":")
#interactions remove var:var + polynomes x2
# DATA --------------------------------------------------------------------
library(tidyverse)

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

# check if there are any NAs: typically because no transmissions occured
outcomes_df %>%
  filter(if_any(everything(), is.na))

outcomes_clean <- outcomes_df %>%
  drop_na() %>%
  filter(peak_coeff == 1.3)


# PLOTS -------------------------------------------------------------------



outcomes_clean %>%
  ggplot(aes(x =delta, y = bias))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")


outcomes_clean %>%
  ggplot(aes(x = 1/trials, y = bias))+
  geom_point()+
  geom_smooth(method= "lm")+
  ggpubr::stat_regline_equation()

outcomes_df %>%
  ggplot(aes(x = 1/trials, y = bias))+
  geom_point(alpha = 0.3)+
  geom_smooth(method= "lm")

outcomes_clean %>%
  ggplot(aes(x = cos(successes), y = bias))+
  geom_point(alpha = 0.3)+
  geom_smooth(method= "lm")



outcomes_clean %>% select(trials, successes, delta, bias) %>%
  mutate(beta = trials/successes,
         beta_scaled1 = base::scale(beta),
         beta_scaled2 = o2groups::scale(beta)) %>%
  ggplot(aes(x = beta_scaled2, y = bias))+
  geom_point()+
  geom_smooth(method= "lm")

outcomes_lm <- outcomes_clean %>%
  dplyr::select(all_of(c("bias", predictors))) #metrics

#%>%
  # mutate(across(
  #   .cols = c(size,successes, trials, r0),
  #   .fns = function(x)
  #     1 / x,
  #   .names = "inv_{col}"
  # ),
  # across(
  #   .cols = c(size,successes, trials, r0),
  #   .fns = function(x)
  #     cos(x),
  #   .names = "cos_{col}"
  # ),
  # beta = trials/successes, #ratio of within group transmissions
  # scaled_beta = o2groups::scale(beta))


# MODELLING ---------------------------------------------------------------



# step-wise ---------------------------------------------------------------
full_model <- lm(coverage ~ ., data = outcomes_lm)
null_model <- lm(coverage ~ 1, data = outcomes_lm)

#run stepwise regression with a set of 2 by 2 interactions as predictors
step_back <- step(full_model, direction = "backward", scope = list(upper = full_model, lower = null_model))
step_forward <- step(null_model, direction = "forward", scope = list(upper = full_model, lower = null_model))
step_back %>% summary()
step_forward %>% summary()
anova(step_back, step_forward, test = "Chisq") #%>% broom::tidy()
performance::compare_performance(step_back, step_forward)
tab_model(step_back)

poly(outcomes_lm$delta, 2
     )
# heuristic interactiion between r0 and trials
model1 <- lm(bias ~ poly(delta, 2) + r0:trials + successes + intro_n + n_groups + GT_mean + GT_sd + INCUB_mean + INCUB_sd, data = outcomes_lm)
anova(model1, step_forward, test = "Chisq") #%>% broom::tidy()
performance::compare_performance(model1, step_forward)
tab_model(model1)

#interaction when combined effct have and additive effect on the response
# tidymodels ----------------------------------------------------------------
library(tidymodels)

split <- initial_split(outcomes_lm, prop = 0.6)
train <- training(split)
test  <- testing(split)
folds <- vfold_cv(train, v = 5, strata = bias)

lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")
#engine poly
lm_recipe <- recipe(bias ~ ., data = train) %>%
  step_normalize(all_predictors())

lm_workflow <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(lm_recipe)

# fit to folds
lm_fit <- fit_resamples(
  lm_workflow,
  folds,
  metrics = metric_set(rmse, rsq),
  control = control_resamples(save_pred = TRUE)
)

collect_metrics(lm_fit)

# add interaction
lm_recipe_interaction <- lm_recipe %>%
  step_interact(terms = ~ r0:trials)

lm_workflow_interaction <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(lm_recipe_interaction)

lm_fit_interaction <- fit_resamples(
  lm_workflow_interaction,
  folds,
  metrics = metric_set(rmse, rsq),
  control = control_resamples(save_pred = TRUE)
)

collect_metrics(lm_fit_interaction)

#predict on folds
lm_fit %>%
  collect_predictions() %>%
  unnest(.pred) %>%
  ggplot(aes(x = bias, y = .pred, color = id)) +
  geom_point(alpha = 0.5) +
  geom_abline()

lm_fit_interaction %>%
  collect_predictions() %>%
  ggplot(aes(x = bias, y = .pred, color = id)) +
  geom_point(alpha = 0.5) +
  geom_abline()

# predict on test
last_fit(lm_workflow, split = split) %>%
  #collect_metrics()
  collect_predictions() %>%
  ggplot(aes(x = bias, y = .pred, color = id)) +
  geom_point(alpha = 0.5) +
  geom_abline()


# glmulti -----------------------------------------------------------------
library(glmulti)

glmulti(bias   ~ delta + r0 + trials + successes + intro_n +  n_groups + GT_mean + GT_sd + INCUB_mean + INCUB_sd,
        data   = outcomes_clean,
        crit   = aic,
        level  = 1,          # 2 with interactions, 1 without #check to do 2 by 2 interactions
        method = "d",        # "d", or "h", or "g"
        family = gaussian,
        fitfunction = glm,   # Type of model (LM, GLM etc.)
        confsetsize = 100)   # Keep 100 best models

g_level1 <- glmulti(bias   ~ delta + r0 + trials + successes + intro_n + n_groups + GT_mean + GT_sd + INCUB_mean + INCUB_sd,
        data   = outcomes_clean,
        crit   = aic,
        level  = 1,          # 2 with interactions, 1 without
        method = "h",        # "d", or "h", or "g"
        family = gaussian,
        fitfunction = lm,   # Type of model (LM, GLM etc.)
        confsetsize = 100)   # Keep 100 best models
summary(g_level1)
best_level1 <- g_level1@objects[[1]]
best_level1
compare_performance(best_level1, model1, step_forward)



# #genetic algo
# g_level2 <- glmulti(bias   ~ delta + r0 + trials + successes + intro_n + n_groups + GT_mean + GT_sd + INCUB_mean + INCUB_sd,
#                     data   = outcomes_clean,
#                     crit   = aic,
#                     level  = 2,          # 2 with interactions, 1 without
#                     method = "h",        # "d", or "h", or "g"
#                     family = gaussian,
#                     fitfunction = glm,   # Type of model (LM, GLM etc.)
#                     confsetsize = 100)   # Keep 100 best models
#
# g_level2


weightable(g_level1)[1:6,] %>%
  regulartable() %>%
  autofit()


plot(g_level1, type = "s")
