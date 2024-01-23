library(tidyverse)
library(here)
library(tidymodels)
#https://workshops.tidymodels.org/
#https://learn.microsoft.com/en-us/training/paths/machine-learning-with-r/
#https://www.coursera.org/learn/tidyverse-modelling-data
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))
source(here("analysis/simulation/script/modelling","model_helpers.R"))

set.seed(123)
data <-
  read_files(path = here("analysis/simulation/data", "estimates"),
             sample_n = 200) %>%
  bind_rows() %>%
  mutate(across(c(peak_coeff, alpha), as.factor)) %>%
  filter(peak_coeff == "1" & alpha == "0.05") %>%
  filter(delta != 0) %>%
  drop_na(trials) %>%  # drops observations where groups doesn't generate transmissions
  mutate(
    true_positive = ifelse(true_positive == 1, "yes", "no"),
    true_positive = factor(true_positive, levels = c("yes", "no")),
    rel_prec = abs(bias / delta) * 100,
    delta_type = case_when(
      delta > 0 ~ "assortative",
      delta < 0 ~ "disassortative",
      TRUE ~ "NA"
    ),
    abs_delta = abs(delta),
    n_trans = trials,
    n_trans_cat = ifelse(n_trans >= 30, ">=30", "<30"),
    n_cases_cat30 = ifelse(n_cases >= 30, ">=30", "<30"),
    n_cases_cat25 = ifelse(n_cases >= 25, ">=25", "<25"),
    n_cases_cat15 = ifelse(n_cases >= 15, ">=15", "<15"),
    size_freq_cat15 = ifelse(size_freq >= 0.15, ">=0.15", "<0.15"),
    size_freq_cat10 = ifelse(size_freq >= 0.10, ">=0.10", "<0.10"),
    size_freq_cat05 = ifelse(size_freq >= 0.05, ">=0.05", "<0.05")
  ) %>%
  ungroup() %>%
  select(
    -c(
      peak_coeff,
      name,
      scenario,
      trials ,
      successes,
      GT_mean,
      GT_sd,
      INCUB_mean,
      INCUB_sd
    )
  )

set.seed(123)
split <- initial_split(data, prop = 0.8)
train <- training(split)
test <- testing(split)
folds <- vfold_cv(train, v = 10)
metrics <- metric_set(yardstick::roc_auc,
                      yardstick::accuracy,
                      yardstick::sens)
glm_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")



formulas <- list(
  true_positive ~ n_cases,
  true_positive ~ n_cases + size_freq,
  true_positive ~ n_cases * size_freq,
  true_positive ~ poly(delta, 2),
  true_positive ~ delta_type + abs_delta,
  true_positive ~ delta_type * abs_delta,
  true_positive ~ delta_type * abs_delta + n_cases,
  true_positive ~ delta_type * abs_delta + n_cases_cat15
)


results <- compare_models(
  formulas = formulas,
  model_spec = glm_spec,
  folds = folds,
  metrics = metrics
)


p_models <-
  results %>%
  filter(.metric == "sens") %>%
  ggplot(aes(x = Formula, y = mean, color = Formula)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  theme(axis.text.x = element_blank(),
        legend.position = "none")
plotly::ggplotly(p_models)

final_formula <-  true_positive ~ delta_type * abs_delta + n_cases

last_fit <- workflow() %>%
  add_model(glm_spec) %>%
  add_formula(as.formula(final_formula)) %>%
  last_fit(split)

last_fit %>%
  collect_metrics()

last_fit %>%
  extract_fit_engine() %>% sjPlot::tab_model()

m <- last_fit %>% extract_fit_engine()
exp(cbind(OR = coef(m), confint(m)))



sensitivity_model <-
  glm(true_positive ~ delta_type * abs(delta) + n_cases,
      data = estimates_df,
      family = binomial)
summary(model)
exp(cbind(OR = coef(sensitivity_model), confint(sensitivity_model)))
