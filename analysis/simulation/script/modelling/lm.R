library(tidyverse)
library(here)
library(tidymodels)

outcomes_df <-
    readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
    filter(peak_coeff == 1) %>%
    select(-peak_coeff) %>%
    drop_na(trials) # drops observations where groups doesn't generate transmissions


lm_df <- outcomes_df %>%
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
        abs_delta = abs(delta),
        bias_type = case_when(
            bias > 0 ~ "positive",
            bias < 0 ~ "negative",
            TRUE ~ "NA"
        ),
        abs_bias = abs(bias),
        size_cat = ifelse(size >= 50, ">=50", "<50"),
        n_pairs = trials,
        n_pairs_cat = ifelse(n_pairs >= 25, ">=25", "<25")
    ) %>%
    group_by(scenario) %>%
    mutate(
        size_freq = size / sum(size, na.rm = TRUE),
        size_freq_cat = ifelse(size_freq >= 0.15, ">=0.15", "<0.15"),
        size_diff = max(size) - min(size),
        beta_diff = max(beta) - min(beta),
        delta_diff = max(delta) - min(delta)
    ) %>%
    ungroup()
lm_df %>% names()


outcomes <- c("sensitivity", "specificity", "bias", "abs_bias", "bias_type")

predictors <- c("n_pairs", "n_pairs_cat", "size", "size_cat", "size_freq", "size_freq_cat", "size_diff", "n_groups", "beta", "beta_diff", "delta", "abs_delta", "delta_type", "delta_diff", "r0", "intro_prop")



make_formula <- function(outcome, predictors, poly_degree = 1, data) {
    if (poly_degree > 1) {
        poly_predictors <- map(predictors, function(predictor) {
            if (is.numeric(data[[predictor]])) {
                map(1:poly_degree, ~ paste0("I(", predictor, "^", .x, ")"))
            } else {
                predictor
            }
        }) %>% unlist()

        formula <- paste0(
            outcome,
            " ~ ",
            paste0(poly_predictors, collapse = " + ")
        )
    } else {
        formula <- paste0(
            outcome,
            " ~ ",
            paste0(predictors, collapse = " + ")
        )
    }

    return(as.formula(formula))
}


step_lm <- function(outcome, predictors, poly_degree = 1, data) {
    formula <- make_formula(outcome, predictors, poly_degree = poly_degree, data = data)
    m <- lm(formula, data = data)
    step <- MASS::stepAIC(m, direction = "both", trace = FALSE)
    return(step)
}



################
# Bias
################

step <- step_lm("abs_bias", predictors, poly_degree = 2, data = lm_df)
summary(step)
tidy(step) %>%
    filter(p.value < 0.05 & term != "(Intercept)") %>%
    arrange(desc(abs(estimate))) %>%
    filter(abs(estimate) >= 0.1)

m <- lm(abs_bias ~ size_freq * size_freq_cat + n_pairs * n_pairs_cat, data = lm_df)
summary(m)

m <- lm(abs_bias ~ log(size_freq) * size_freq_cat + log(n_pairs) * n_pairs_cat, data = lm_df)
summary(m)

m <- lm(abs_bias ~ log(size_freq) * size_freq_cat +
    log(n_pairs) * n_pairs_cat, data = lm_df)
summary(m)

plot(m)


ggplot(lm_df, aes(y = abs_bias, x = size_freq)) +
    geom_point() +
    stat_smooth()

ggplot(lm_df, aes(y = abs_bias, x = n_pairs)) +
    geom_point() +
    stat_smooth()

ggplot(lm_df, aes(y = bias, x = n_pairs)) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ log(x))


################
# Sensitivity
################

step <- step_lm("sensitivity", predictors, poly_degree = 2, data = lm_df)
summary(step)
tidy(step) %>%
    filter(p.value < 0.05 & term != "(Intercept)") %>%
    arrange(desc(abs(estimate))) %>%
    filter(abs(estimate) >= 0.1)

m <- lm(sensitivity ~ poly(delta, 4), data = lm_df)
summary(m)

m <- lm(sensitivity ~ abs_delta * delta_type, data = lm_df)
summary(m)
m <- lm(sensitivity ~ abs_delta + delta_type + n_pairs, data = lm_df)
summary(m)
m <- lm(sensitivity ~ abs_delta + delta_type + n_pairs * n_pairs_cat + size_freq * size_freq_cat, data = lm_df)
summary(m)


ggplot(lm_df, aes(y = sensitivity, x = delta)) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ poly(x, 4))
ggplot(lm_df, aes(y = sensitivity, x = abs_delta, alpha = 0.5)) +
    facet_wrap(~delta_type) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ x)

ggplot(lm_df, aes(y = sensitivity, x = size_freq, alpha = 0.5)) +
    facet_wrap(~size_freq_cat) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ x)



################
# Coverage
################
lm_df <- lm_df %>%
    mutate(
        coverage_error = coverage - 0.95,
        abs_coverage_error = abs(coverage_error)
    )

step <- step_lm("abs_coverage_error", predictors, poly_degree = 2, data = lm_df)
summary(step)
tidy(step) %>%
    filter(p.value < 0.05 & term != "(Intercept)") %>%
    arrange(desc(abs(estimate))) %>%
    filter(abs(estimate) >= 0.05)


################
# Specificity
################
# remove anything that contains delta from predictors using grep
specificity_predictors <- predictors[!grepl("delta", predictors)]



step <- step_lm("specificity", specificity_predictors, poly_degree = 3, data = lm_df)
summary(step)
tidy(step) %>%
    filter(p.value < 0.05 & term != "(Intercept)") %>%
    arrange(desc(abs(estimate))) %>%
    filter(abs(estimate) >= 0.1)

m <- lm(specificity ~ n_pairs + size_freq, data = lm_df)
summary(m)


# Tidymodels --------------------------------------------------------------

split <- initial_split(lm_df, prop = 0.75)
train <- training(split)
test <- testing(split)

m_bias <- lm(
    abs_bias ~ log(size_freq) * size_freq_cat +
        log(n_pairs),
    data = train
)
summary(m_bias)
par(mfrow = c(2, 2))
plot(m_bias)
car::ncvTest(m_bias) # heteroskedasticity
car::vif(m_bias) %>% tidy() # multicollinearity

test %>%
    mutate(pred = predict(m_bias, newdata = test)) %>%
    ggplot(aes(y = abs_bias, x = pred)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)
test %>%
    mutate(pred = predict(m_bias, newdata = test)) %>%
    metrics(truth = abs_bias, estimate = pred)


m_sensitivity <- lm(sensitivity ~ abs_delta + delta_type + n_pairs, data = train)
summary(m_sensitivity)
par(mfrow = c(2, 2))
plot(m_sensitivity)
car::ncvTest(m_sensitivity) # heteroskedasticity
car::vif(m_sensitivity) # multicollinearity

test %>%
    filter(delta_type != "neutral") %>%
    mutate(delta_type = factor(delta_type,
        levels = c("assortative", "disassortative")
    )) %>%
    mutate(pred = predict(m_sensitivity, newdata = .)) %>%
    ggplot(aes(y = sensitivity, x = pred)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)

test %>%
    filter(delta_type != "neutral") %>%
    mutate(delta_type = factor(delta_type,
        levels = c("assortative", "disassortative")
    )) %>%
    mutate(pred = predict(m_sensitivity, newdata = .)) %>%
    metrics(truth = sensitivity, estimate = pred)




# Final Function ----------------------------------------------------------
m_bias <- lm(
    abs_bias ~ log(size_freq) * size_freq_cat +
        log(n_pairs),
    data = train
)
m_sensitivity <- lm(sensitivity ~ abs_delta + delta_type + n_pairs, data = train)

lm_list <- list(abs_bias = m_bias, sensitivity = m_sensitivity)

library(gtsummary)
outcomes <- c("bias", "sensitivity")
map(lm_list, ~tbl_regression(.x)) %>%
  tbl_merge(
    tab_spanner = outcomes
  )

library(stargazer)
table <- stargazer(
  lm_list,
  type = "latex",  # Use "latex" for LaTeX output
  title = "Regression Results",
  align = TRUE,  # Align variables and coefficients
  dep.var.labels.include = FALSE,  # Include outcome labels
  covariate.labels.include = FALSE,  # Include predictor labels
  keep.stat = c("n", "adj.rsq", "AIC", "BIC"),  # Specify statistics to include
  column.labels = outcomes,  # Label columns with outcome names
  dep.var.caption = "Dependent Variable",  # Caption for dependent variable
  style = "qje",  # Choose a style (you can customize this)
  digits = 2,  # Number of digits to display
  single.row = TRUE,
  column.sep.width = "0pt",
  font.size = "footnotesize",
  no.space = TRUE
)
cat(table)
cat(table, file = here("analysis/simulation/data/model/lm_table2.tex"))
