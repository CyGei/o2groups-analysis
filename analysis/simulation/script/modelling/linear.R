library(tidyverse)
library(here)
library(tidymodels)

# Data ---------------------------------------------------------------------
outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
  drop_na(trials) %>%
  mutate(
    delta_type = case_when(
      delta == 0 ~ "neutral",
      delta > 0 ~ "assortative",
      delta < 0 ~ "disassortative",
      TRUE ~ "NA"
    ),
    delta_type = factor(
      delta_type,
      levels = c("neutral", "assortative", "disassortative")
    ),
    abs_delta = abs(delta)
  ) %>%
  group_by(scenario, peak_coeff) %>%
  mutate(N_pairs = sum(trials, na.rm = TRUE))


head(outcomes_df)

predictors <- c(
  "abs_delta",
  "delta_type",
  "n_groups",
  "size",
  "intro_prop",
  "r0",
  "beta",
  "peak_coeff"
)

outcomes <- c("bias", "coverage", "specificity", "sensitivity")

# Preprocess data ----------------------------------------------------------
data <- outcomes_df %>%
  select(all_of(c(predictors, outcomes)))

split <- initial_split(data, prop = 0.75)
train <- training(split)
test <- testing(split)


# Formula ------------------------------------------------------------------

rec_bias <- recipe(bias ~
                     abs_delta +
                     delta_type +
                     n_groups +
                     size +
                     intro_prop +
                     r0 +
                     beta +
                     peak_coeff,
                   data = train) %>% step_naomit(bias)

rec_coverage <-
  recipe(coverage ~ ., data = train) %>% step_naomit(coverage)
rec_specificity <-
  recipe(specificity ~ ., data = train) %>% step_naomit(specificity)
rec_sensitivity <-
  recipe(sensitivity ~ ., data = train) %>% step_naomit(sensitivity)

lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")
lm_spec

bias_fit <- workflow() %>%
  add_recipe(rec_bias) %>%
  add_model(lm_spec) %>%
  fit(data = train)

bias_fit
