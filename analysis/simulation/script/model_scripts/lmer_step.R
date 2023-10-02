library(here)
library(tidyverse)
library(lmerTest)
library(tidymodels)

# Load data
model_df <- readRDS(here("analysis/simulation/data/model", "model_df.rds")) %>%
  drop_na(trials)

# Define outcome
outcome <- "bias"

# Define predictors
predictors <- c(
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

# Define levels
levels <- c("scenario", "name")

df <- model_df %>%
  select(all_of(c(levels, outcome, predictors))) %>%
  drop_na(all_of(outcome))

# Set seed and split data
set.seed(100)
split <- initial_split(df)
train <- training(split)
test <- testing(split)

# Create the full formula
full_formula <-
  as.formula(paste(outcome, "~", paste0(predictors, collapse = "+"), "+ (1 + peak_coeff | scenario:name)"))

# Fit the full mixed-effects model
full_model <- lmerTest::lmer(full_formula, data = train)

# Perform stepwise regression (backward selection) using step function
step_model <- lmerTest::step(full_model)
final <- get_model(step_res)
anova(final)


# Summary of the selected model
summary(step_model)
