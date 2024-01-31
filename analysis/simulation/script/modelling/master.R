library(tidyverse)
library(here)
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))
source(here("analysis/simulation/script/modelling","modelling_helpers.R"))

# Data --------------------------------------------------------------------
# Summary Data
summary_df <-
  read_files(path = here("analysis/simulation/data", "summary")) %>%
  bind_rows() %>%
  mutate(across(c(peak_coeff, alpha), as.factor))


# Model Data
data <- summary_df %>%
  filter(alpha == 0.05 & peak_coeff == 1) %>%
  drop_na(trials) %>%  # drops observations where groups doesn't generate transmissions
  mutate(
    delta_type = case_when(
      delta == 0 ~ "neutral",
      delta > 0 ~ "assortative",
      delta < 0 ~ "disassortative",
      TRUE ~ "NA"
    ),
    abs_delta = abs(delta),
    n_trans = trials,
    n_cases_cat30 = ifelse(n_cases >= 30, ">=30", "<30"),
    n_cases_cat15 = ifelse(n_cases >= 15, ">=15", "<15"),
    size_freq_cat10 = ifelse(size_freq >= 0.10, ">=0.10", "<0.10"),
    size_freq_cat05 = ifelse(size_freq >= 0.05, ">=0.05", "<0.05"),
    size_freq_cat = case_when(
      size_freq <= 0.3 ~ "small",
      size_freq > 0.3 & size_freq < 0.7 ~ "medium",
      size_freq >= 0.7 ~ "large",
    )
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


# Bias --------------------------------------------------------------------
# Create a list of formulas to test
formulas <- list(
  bias ~  n_trans,
  bias ~ n_cases,
  bias ~ size_freq,
  bias ~ size,
  bias ~ log(size_freq) * size_freq_cat10 + log(n_cases) * n_trans,
  bias ~ log(size_freq) * size_freq_cat10 + log(n_cases) * n_cases_cat30,
  bias ~ log(size_freq) * size_freq_cat10 + log(n_cases) * n_cases_cat15
)
results <- cross_validate(formulas, data, k = 10, seed = 123)
as_tibble(results)
# Model Selection
results %>%
  ggplot(aes(x = as.character(formula_index), y = r2)) +
  geom_boxplot() +
  theme_minimal()

# Fit the model
model <- lm(formulas[[7]], data = data)
model %>% summary() %>% sjPlot::tab_model()


# Sensitivity -------------------------------------------------------------

# add weights (i.e.number of simulations) for the glm model
# drop na for sensitivity
data_sensitivity <- data %>%
  drop_na(sensitivity) %>%
  mutate(weights = 100)

formulas <- list(
  sensitivity ~ n_cases,
  sensitivity ~ n_cases + size_freq,
  sensitivity ~ n_cases * size_freq,
  sensitivity ~ poly(delta, 2),
  sensitivity ~ delta_type + abs_delta,
  sensitivity ~ delta_type * abs_delta,
  sensitivity ~ delta_type * abs_delta + n_cases,
  sensitivity ~ delta_type * abs_delta + n_cases_cat15,
  sensitivity ~ delta_type * abs_delta + size_freq_cat05,
  sensitivity ~ delta_type * abs_delta + size_freq_cat10,
  sensitivity ~ delta_type * abs_delta + n_cases + size_freq_cat10,
  sensitivity ~ delta_type * abs_delta + n_cases + size_freq
)
results <-
  cross_validate(
    formulas,
    data = data_sensitivity,
    method = "logistic",
    k = 10,
    seed = 123
  )
as_tibble(results)
results %>%
  ggplot(aes(x = factor(formula_index, level = 1:length(formulas)), y = metric.accuracy)) +
  geom_boxplot() +
  theme_minimal()

results %>%
  filter(formula_index == 7) %>%
  summarise(
    across(starts_with("metric"), mean)
  )
formulas[[7]]

# Fit the model
model <-
  glm(formulas[[7]],
      data = data_sensitivity,
      family = binomial(link = "logit"),
      weights = weights)

#predicted vs observed
data_preds <- data_sensitivity %>%
  mutate(predictions = predict(model, type = "response")) %>%
  mutate(predictions = ifelse(predictions > 0.5, 1, 0)) %>%
  mutate(actual = ifelse(sensitivity > 0.5, 1, 0)) %>%
  mutate(predictions = as.factor(predictions)) %>%
  mutate(actual = as.factor(actual)) %>%
  select(predictions, actual)

# Metrics
cm <- caret::confusionMatrix(data_preds$predictions, data_preds$actual,
                             mode = "everything", positive="1")
cm


# Specificity -------------------------------------------------------------
data_specificity <- data %>%
  drop_na(specificity) %>%
  mutate(weights = 100)


formulas <- list(
  specificity ~ n_trans,
  specificity ~ n_cases,
  specificity ~ size_freq,
  specificity ~ sd_peak_date,
  specificity ~ group_susceptibles,
  specificity ~ total_susceptibles,
  specificity ~ sd_group_susceptibles,
  specificity ~ n_cases + size_freq,
  specificity ~ n_cases * size_freq
)

results <-
  cross_validate(
    formulas,
    data = data_specificity,
    method = "logistic",
    k = 10,
    seed = 123
  )

results %>%
  ggplot(aes(x = factor(formula_index, level = 1:length(formulas)), y = metric.McFaddenR2)) +
  geom_boxplot() +
  theme_minimal()

results %>%
  filter(formula_index == 4) %>%
  summarise(
    across(starts_with("metric"), mean)
  )


model <- glm(formulas[[4]],
    data = data_specificity,
    family = binomial(link = "logit"),
    weights = weights)

#predicted vs observed
data_preds <- data_specificity %>%
  mutate(predictions = predict(model, type = "response")) %>%
  mutate(predictions = ifelse(predictions > 0.5, 1, 0)) %>%
  mutate(actual = ifelse(specificity > 0.5, 1, 0)) %>%
  mutate(predictions = as.factor(predictions)) %>%
  mutate(actual = as.factor(actual)) %>%
  select(predictions, actual)

# Metrics
cm <- caret::confusionMatrix(data_preds$predictions, data_preds$actual,
                             mode = "everything", positive="1")

model %>% sjPlot::tab_model()
glmtoolbox::adjR2(model)

# Coverage ----------------------------------------------------------------

data_coverage <- data %>%
  drop_na(coverage) %>%
  mutate(weights = 100)

formulas <- list(
  coverage ~ n_trans,
  coverage ~ n_cases,
  coverage ~ size_freq,
  coverage ~ sd_peak_date,
  coverage ~ group_susceptibles,
  coverage ~ total_susceptibles,
  coverage ~ sd_group_susceptibles,
  coverage ~ n_cases + size_freq,
  coverage ~ n_cases * size_freq
)

results <-
  cross_validate(
    formulas,
    data = data_coverage,
    method = "logistic",
    k = 10,
    seed = 123
  )

results %>%
  ggplot(aes(x = factor(formula_index, level = 1:length(formulas)), y = metric.McFaddenR2)) +
  geom_boxplot() +
  theme_minimal()

results %>%
  filter(formula_index == 4) %>%
  summarise(
    across(starts_with("metric"), mean)
  )

model <- glm(formulas[[4]],
    data = data_coverage,
    family = binomial(link = "logit"),
    weights = weights)
model %>% sjPlot::tab_model()
