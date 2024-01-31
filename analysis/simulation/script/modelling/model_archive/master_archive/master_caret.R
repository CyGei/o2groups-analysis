library(tidyverse)
library(here)
library(caret)
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))
# source(here("analysis/simulation/script/modelling","model_helpers.R"))


# Summary Data
summary_df <-
  read_files(path = here("analysis/simulation/data", "summary")) %>%
  bind_rows() %>%
  mutate(across(c(peak_coeff, alpha), as.factor))


# Data Prep
data <- summary_df %>%
  filter(alpha == 0.05 & peak_coeff == 1) %>%
  drop_na(trials) %>%  # drops observations where groups doesn't generate transmissions
  mutate(
    sensitivity2 = case_when(
      sensitivity == 0 ~ 0.001,
      sensitivity == 1 ~ 0.999,
      TRUE ~ sensitivity
    ),
    specificity2 = case_when(
      specificity == 0 ~ 0.001,
      specificity == 1 ~ 0.999,
      TRUE ~ specificity
    ),
    coverage2 = case_when(
      coverage == 0 ~ 0.001,
      coverage == 1 ~ 0.999,
      TRUE ~ coverage
    ),
    rel_prec = abs(bias / delta) * 100,
    delta_type = case_when(
      delta == 0 ~ "neutral",
      delta > 0 ~ "assortative",
      delta < 0 ~ "disassortative",
      TRUE ~ "NA"
    ),
    abs_delta = abs(delta),
    n_trans = trials,
    n_trans_cat30 = ifelse(n_trans >= 30, ">=30", "<30"),
    n_cases_cat30 = ifelse(n_cases >= 30, ">=30", "<30"),
    n_cases_cat25 = ifelse(n_cases >= 25, ">=25", "<25"),
    n_cases_cat15 = ifelse(n_cases >= 15, ">=15", "<15"),
    size_freq_cat15 = ifelse(size_freq >= 0.15, ">=0.15", "<0.15"),
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


outcomes <-
  c("sensitivity", "specificity", "bias", "bias_type")

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



# Exploratory Plots -------------------------------------------------------

# Bias
data %>%
  ggplot(aes(x = n_cases, y = exp(bias))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

data %>%
  ggplot(aes(x = size_freq_cat, y = car::logit(sensitivity2))) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme_bw()


hist(car::logit(data$specificity2), breaks = 40)

# Helper Functions --------------------------------------------------------
resamples_to_df <- function(models, metric = "Rsquared") {
  resamples <- resamples(models)

  results <-
    as.data.frame(resamples$values[, grepl(metric, names(resamples$values))]) %>%
    pivot_longer(cols = starts_with("Model"),
                 names_to = "Model",
                 values_to = metric) %>%
    mutate(Model = str_remove(Model, paste0("~", metric))) %>%
    group_by(Model) %>%
    mutate(CV = row_number())
  return(results)
}


# Cross Validation --------------------------------------------------------
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10,verboseIter = TRUE)


# Bias --------------------------------------------------------
bias_formulas <- list(
  bias ~  n_trans,
  bias ~ n_cases,
  bias ~ size_freq,
  bias ~ size,
  bias ~ log(size_freq) * size_freq_cat15 + log(n_trans) * n_trans_cat30,
  bias ~ log(size_freq) * size_freq_cat10 + log(n_cases) * n_cases_cat25,
  bias ~ log(size_freq) * size_freq_cat10 + log(n_cases) * n_cases_cat15,
  exp(bias) ~ log(size_freq) * size_freq_cat10 + log(n_cases) * n_cases_cat15
)

bias_models <- lapply(bias_formulas, function(formula) {
  train(
    formula,
    data = data,
    method = "lm",
    trControl = ctrl,
    na.action = na.exclude
  )
})

results <- resamples_to_df(bias_models, metric = "Rsquared")

results %>%
  ggplot(aes(x = Model, y = Rsquared, color = Model)) +
  geom_violin()+
  stat_summary(fun = mean, geom = "point", size = 3) +
  theme_bw() +
  theme(legend.position = "none")
formula(bias_models[[8]][["terms"]])
bias_models[[8]][["finalModel"]] %>%
  sjPlot::tab_model()

# plot(bias_models[[8]][["finalModel"]])
# plot(bias_models[[8]][["finalModel"]], 4)
#
# cooksD <- cooks.distance(bias_models[[8]][["finalModel"]])
# influential  <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
# influential <- as.numeric(gsub("X", "", names(influential)))
# screened_data <- data[-influential,]
# new_model <-
#   update(bias_models[[8]], data = screened_data)
# train_model(
#   formula = formula(bias_models[[8]][["terms"]]),
#   data = screened_data,
#   trControl = ctrl,
#   method = "lm"
# )


# Sensitivity --------------------------------------------------------
sensitivity_formulas <- list(
  car::logit(sensitivity2) ~ n_cases,
  car::logit(sensitivity2) ~ n_cases + size_freq,
  car::logit(sensitivity2) ~ n_cases * size_freq,
  car::logit(sensitivity2) ~ poly(delta, 2),
  car::logit(sensitivity2) ~ delta_type + abs_delta,
  car::logit(sensitivity2) ~ delta_type * abs_delta,
  car::logit(sensitivity2) ~ delta_type * abs_delta + n_cases,
  car::logit(sensitivity2) ~ delta_type * abs_delta + n_cases_cat15,
  car::logit(sensitivity2) ~ delta_type * abs_delta + size_freq_cat10,
  car::logit(sensitivity2) ~ delta_type * abs_delta + n_cases + size_freq_cat10,
  car::logit(sensitivity2) ~ delta_type * abs_delta + n_cases + size_freq_cat
)




sensitivity_models <- lapply(sensitivity_glm_formula, function(formula) {
  train(
    formula,
    data = data,
    method = "lm",
    trControl = ctrl,
    na.action = na.exclude
  )
})

# sensitivity_models <- lapply(sensitivity_glm_formula, function(formula) {
#   glm(formula, data = data, family = binomial)
# })


results <- resamples_to_df(sensitivity_models, metric = "Rsquared")

results %>%
  ggplot(aes(x = Model, y = Rsquared, color = Model)) +
  geom_violin()+
  stat_summary(fun = mean, geom = "point", size = 3) +
  theme_bw() +
  theme(legend.position = "none")
formula(sensitivity_models[[07]][["terms"]])

sensitivity_models[[07]][["finalModel"]] %>%
  sjPlot::tab_model()

par(mfrow = c(2, 2))
plot(sensitivity_models[[07]][["finalModel"]])


# Specificity --------------------------------------------------------
specificity_formulas <- list(
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

specificity_models <- lapply(specificity_formulas, function(formula) {
  train(
    formula,
    data = data,
    method = "glm",
    family = binomial,
    weights = rep(100, nrow(data)),
    trControl = ctrl,
    na.action = na.exclude
  )
})

results <- resamples_to_df(specificity_models, metric = "Rsquared")

results %>%
  ggplot(aes(x = Model, y = Rsquared, color = Model)) +
  geom_violin()+
  stat_summary(fun = mean, geom = "point", size = 3) +
  theme_bw() +
  theme(legend.position = "none")
formula(specificity_models[[4]][["terms"]])
specificity_models[[4]][["finalModel"]] %>%
  sjPlot::tab_model()


# Coverage ----------------------------------------------------------------

coverage_formulas <- list(
  car::logit(coverage2) ~ n_trans,
  car::logit(coverage2) ~ n_cases,
  car::logit(coverage2) ~ size_freq,
  car::logit(coverage2) ~ sd_peak_date,
  car::logit(coverage2) ~ group_susceptibles,
  car::logit(coverage2) ~ total_susceptibles,
  car::logit(coverage2) ~ sd_group_susceptibles
)

coverage_models <- lapply(coverage_formulas, function(formula) {
  train(
    formula,
    data = data,
    method = "lm",
    trControl = ctrl,
    na.action = na.exclude
  )
})

results <- resamples_to_df(coverage_models, metric = "Rsquared")

results %>%
  ggplot(aes(x = Model, y = Rsquared, color = Model)) +
  geom_violin()+
  stat_summary(fun = mean, geom = "point", size = 3) +
  theme_bw() +
  theme(legend.position = "none")
formula(coverage_models[[4]][["terms"]])
coverage_models[[4]][["finalModel"]] %>%
  sjPlot::tab_model()
