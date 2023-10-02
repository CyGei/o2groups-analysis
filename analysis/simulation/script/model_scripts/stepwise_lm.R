library(here)
library(tidyverse)
library(tidymodels)
# library(car)        # extracts model results
# library(MASS)       # provides "birthwt" dataset
# library(ISLR)       # provides "Wage" dataset
# library(tictoc)     # checks running time
# library(sjPlot)     # visualizes model results
# library(glmulti)    # finds the BEST model
# library(flextable)  # beautifies tables
# library(tidyverse)  # provides a lot of useful stuff !!!
# library(performance)# checks and compares quality of models

# Description --------------------------------------------------------------
# We use outcomes_df (model_df data aggreagated across simulations) to model
# our 3 aggregated metrics: coverage, sensitivity and specificity using a linear model.

# Data ---------------------------------------------------------------------

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
  drop_na(trials)

# Model Vars --------------------------------------------------------------

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
    "beta"
  )
metrics <- c("coverage", "bias", "sensitivity", "specificity")
interactions <-
  apply(combn(predictors, 2), 2, function(x)
    paste(sort(x), collapse = ":"))
num_interactions <- choose(n = length(predictors), k = 2)

polynomes <- paste0("I(", predictors, "^2)")

all_preds <-
  paste0(
    paste0(predictors, collapse = " + "),
    " + ",
    paste0(polynomes, collapse = " + "),
    " + ",
    paste0(interactions, collapse = " + ")
  )

formulas <- list(
  coverage = as.formula(paste0("coverage", "~", all_preds)),
  sensitivity = as.formula(paste0("sensitivity", "~", all_preds)),
  specificity = as.formula(paste0("specificity", "~", all_preds))
)

# PLOTS -------------------------------------------------------------------

# outcomes_df %>%
#   ggplot(aes(x = 1 / trials, y = bias)) +
#   geom_point() +
#   geom_smooth(method = "lm")
#
# outcomes_df %>%
#   ggplot(aes(x = 1 / trials, y = bias)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "lm")
#
# outcomes_df %>%
#   ggplot(aes(x = cos(successes), y = bias)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "lm")
#
#
#
# outcomes_df %>% select(trials, successes, delta, bias) %>%
#   mutate(
#     beta = trials / successes,
#     beta_scaled1 = base::scale(beta),
#     beta_scaled2 = o2groups::scale(beta)
#   ) %>%
#   ggplot(aes(x = beta_scaled2, y = bias)) +
#   geom_point() +
#   geom_smooth(method = "lm")

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


# Data Split ---------------------------------------------------------------

split <- initial_split(outcomes_df, prop = 0.6)
train <- training(split)
test  <- testing(split)
folds <- vfold_cv(train, v = 5)


# Coverage ----------------------------------------------------------------
full_model <- lm(formulas$coverage, data = train)
null_model <- lm(coverage ~ 1, data = train)
base_model <-
  lm(as.formula(paste0(
    "coverage~", paste0(predictors, collapse = " + ")
  )), data = train)

set.seed(123)
system.time({
  step_back <-
    stats::step(
      full_model,
      direction = "backward",
      scope = list(upper = full_model, lower = null_model)
    )
})

system.time({
  step_forward <-
    stats::step(
      null_model,
      direction = "forward",
      scope = list(upper = full_model, lower = null_model)
    )
})
saveRDS(step_back, here("analysis/simulation/data/model", "coverage_step_back.rds"))
saveRDS(step_forward, here("analysis/simulation/data/model", "coverage_step_forward.rds"))

step_back %>% summary()
step_forward %>% summary()
anova(step_back, step_forward, test = "Chisq") #%>% broom::tidy()
performance::compare_performance(step_back, step_forward)
sjPlot::tab_model(step_forward)



# weightable(g_level1)[1:6,] %>%
#   regulartable() %>%
#   autofit()
#
#
# plot(g_level1, type = "s")
