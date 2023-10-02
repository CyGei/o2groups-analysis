library(here)
library(tidyverse)
library(tidymodels)

# load data
model_df <-
  readRDS(here("analysis/simulation/data/model", "model_df.rds"))

# Model Bias using multilevel modeling (levels are scenario, simulation, group)
predictors <- c(
  "peak_coeff",
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

# lmer(y ~ x + (1 | random-effect), data = my_data) -> an intercept for each rand effect group
# (countinuous_predictor | random_effect_group) for a random-effect slope.-> a different slope for each rand effect group
# a random-effect slope may be estimated for each group using (slope | group)
test_data <- model_df %>%
  filter(n_groups <= 3) %>%
  filter(scenario %in% unique(scenario)[1:12]) %>%
  mutate(bias = abs(bias),
         across(all_of(predictors), ~ as.numeric(scale(.x)))) %>%
  select(all_of(c("scenario", "name", "bias", predictors))) %>%
  drop_na() %>%
  as_tibble()

library(lme4)
library(lmerTest)
# This model treats name as a nested random effect within scenario,
# allowing for different sets of names within each scenario.
m0 <-
  lmerTest::lmer(
    bias ~ delta + n_groups + size + intro_prop + r0 + GT_mean + GT_sd +
      INCUB_mean + INCUB_sd + trials + successes + peak_coeff +
      (1 | scenario:name),
    data = test_data
  )
summary(m0)
ranef(m0)

performance::r2_nakagawa(m0)

m1 <-
  lmerTest::lmer(
    bias ~ delta + n_groups + size + intro_prop + r0 + GT_mean + GT_sd +
      INCUB_mean + INCUB_sd + trials + successes + peak_coeff +
      (1 | scenario:name) +
      (1 | peak_coeff),
    data = test_data
  )
summary(m1)
ranef(m1)
anova(m0, m1)
performance::r2_nakagawa(m1)


m2 <-
  lmerTest::lmer(
    bias ~ delta + n_groups + size + intro_prop + r0 + GT_mean + GT_sd +
      INCUB_mean + INCUB_sd + trials + successes + peak_coeff +
      (1 + peak_coeff | scenario:name),
    data = test_data
  )
summary(m2)
ranef(m2)
anova(m1, m2)
performance::r2_nakagawa(m2)
performance::icc(m2)
#performance::check_model(m2)
performance::model_performance(m2)

m3 <- lmerTest::lmer(as.formula(
  paste0("bias", "~", all_preds, "+ (1 + peak_coeff | scenario:name)")
), data = test_data)

summary(m3)
ranef(m3)
anova(m2, m3)
performance::r2_nakagawa(m3)
performance::icc(m3)

m4_terms <- broom.mixed::tidy(m3) %>%
  filter(p.value < 0.05) %>%
  pull(term) %>%
  paste0(collapse = " + ")

m4 <- lmerTest::lmer(as.formula(
  paste0("bias", "~", m4_terms, "+ (1 + peak_coeff | scenario:name)")
), data = test_data)

summary(m4)
anova(m2, m4)
performance::r2_nakagawa(m4)
performance::icc(m4)

performance::compare_performance(m1, m2, m3, m4, rank = TRUE) %>%
  flextable::flextable()


test_data %>%
  mutate(pred = predict(m4)) %>%
  ggplot(aes(
    x = pred,
    y = bias,
    color = as.factor(peak_coeff)
  )) +
  geom_point(alpha = 0.8, size = 0.5) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(group = interaction(scenario, name, peak_coeff)),
    linewidth = 0.5
  ) +

  #geom_line(aes(group = interaction(scenario, name, peak_coeff))) +
  facet_grid(name ~ scenario) +
  theme_classic() +
  scale_color_viridis_d() +
  theme(legend.position = "none")


test_data %>%
  mutate(pred = predict(m2)) %>%
  filter(scenario %in% unique(scenario)[4]) %>%
  filter(name == "A") %>%
  ggplot(aes(
    x = pred,
    y = bias,
    color = as.factor(peak_coeff)
  )) +
  geom_point(alpha = 0.8, size = 0.5) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(group = interaction(scenario, name, peak_coeff)),
    linewidth = 0.5
  ) +

  #geom_line(aes(group = interaction(scenario, name, peak_coeff))) +
  facet_grid(name ~ scenario) +
  theme_classic() +
  scale_color_viridis_d() +
  theme(legend.position = "none")



# use m4 to predict bias for test data
test_data %>%
  mutate(pred = predict(m4)) %>%
  ggplot(aes(
    x = pred,
    y = bias,
    color = as.factor(peak_coeff)
  )) +
  geom_point(alpha = 0.8, size = 0.5) +
  # geom_smooth(
  #   method = "lm",
  #   se = FALSE,
  #   aes(group = interaction(scenario, name, peak_coeff)),
  #   linewidth = 0.5
  # ) +
  theme_classic() +
  scale_color_viridis_d() +
  theme(legend.position = "none")



# # Perform backward selection
# while (TRUE) {
#   # Get p-values for fixed effects
#   results <-  broom.mixed::tidy(m4)
#
#   # Find the variable with the largest p-value (least significant)
#   max_p_value <- max(results$p.value, na.rm = T)
#
#   # Check if the largest p-value is above your significance level (e.g., 0.05)
#   if (max_p_value > 0.05) {
#     # Remove the least significant variable
#     least_significant_variable <-
#       results$term[which.max(results$p.value)]
#     m4 <-
#       stats::update(m4, as.formula(paste0(". ~ . -", least_significant_variable)))
#   } else {
#     # If all variables are significant, break the loop
#     break
#   }
# }
