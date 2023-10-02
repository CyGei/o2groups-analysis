library(here)
library(tidyverse)
library(tidymodels)

outcomes_df <-
  readRDS(here("analysis/simulation/data/model", "outcomes_df.rds")) %>%
  drop_na(trials)

# Define predictors and outcome
predictors <- c(
  "delta",
  "n_groups",
  "size",
  "intro_prop",
  "r0",
  "trials",
  "successes",
  "beta",
  "peak_coeff"
)
####################################
# Extract results from caret_lm.R #
####################################

run_recipe <- function(){
df <- outcomes_df %>% select(all_of(c(outcome, predictors)))

recipe <- recipe(as.formula(paste0(outcome, "~ .")) ,
                 data = df) %>%
  step_naomit(trials) %>%
  step_normalize(all_predictors()) %>%
  step_corr(all_predictors(), threshold = 0.9) %>%
  step_poly(all_predictors(), degree = 2) %>%
  step_interact(term = ~ ends_with("poly_1"):ends_with("poly_1")) %>%
  prep()

m_dat <- bake(recipe, new_data = NULL)
names(m_dat) <- gsub("_poly_1","", names(m_dat))
names(m_dat) <- gsub("_poly_2","2", names(m_dat))
return(m_dat)
}

clean_formula <- function(original_formula) {
  cleaned_formula <- gsub("_poly_1", "", original_formula)
  cleaned_formula <- gsub("_poly_2", "2", cleaned_formula)
  cleaned_formula <- as.formula(paste0(cleaned_formula[2], "~", cleaned_formula[3]))
  return(cleaned_formula)
}

# bias --------------------------------------------------------------------
bias_lm <- readRDS("~/PROJECTS_LOCAL/o2groups-analysis/analysis/simulation/data/model/bias_lm.rds")
outcome <- "bias"
m_dat <- run_recipe()
cleaned_formula <- gsub("_poly_1", "", original_formula)
cleaned_formula <- gsub("_poly_2", "2", cleaned_formula)

mbias <- lm(clean_formula(bias_lm$finalFormula), data = m_dat)
summary(mbias)
sjPlot::plot_model(mbias)


#remove the interactiongs from (bias_lm$finalFormula) by removing any term containing _x_:
final_formula_no_interactions <- gsub("\\s*\\+\\s*\\w+_x_\\w+", "", bias_lm$finalFormula)
m1 <- lm(as.formula(paste0(outcome, "~", final_formula_no_interactions[3])), data = m_dat)
summary(m1)


# sensitivity --------------------------------------------------------------------
sensitivity_lm <- readRDS("~/PROJECTS_LOCAL/o2groups-analysis/analysis/simulation/data/model/sensitivity_lm.rds")
outcome <- "sensitivity"
m_dat <- run_recipe()

msens <- lm(clean_formula(sensitivity_lm$finalFormula), data = m_dat)
summary(msens)
sjPlot::plot_model(msens)

final_formula_no_interactions <- gsub("\\s*\\+\\s*\\w+_x_\\w+", "", sensitivity_lm$finalFormula)
m1 <- lm(as.formula(paste0(outcome, "~", final_formula_no_interactions[3])), data = m_dat)
summary(m1)


# specificity --------------------------------------------------------------------
specificity_lm <- readRDS("~/PROJECTS_LOCAL/o2groups-analysis/analysis/simulation/data/model/specificity_lm.rds")
outcome <- "specificity"
m_dat <- run_recipe()

mspec <- lm(clean_formula(specificity_lm$finalFormula), data = m_dat)
summary(mspec)
sjPlot::plot_model(mspec)

final_formula_no_interactions <- gsub("\\s*\\+\\s*\\w+_x_\\w+", "", specificity_lm$finalFormula)
m1 <- lm(as.formula(paste0(outcome, "~", final_formula_no_interactions[3])), data = m_dat)
summary(m1)


# coverage --------------------------------------------------------------------
coverage_lm <- readRDS("~/PROJECTS_LOCAL/o2groups-analysis/analysis/simulation/data/model/coverage_lm.rds")
outcome <- "coverage"
m_dat <- run_recipe()

mcov <- lm(clean_formula(coverage_lm$finalFormula), data = m_dat)
summary(mcov)
sjPlot::plot_model(mcov)

final_formula_no_interactions <- gsub("\\s*\\+\\s*\\w+_x_\\w+", "", coverage_lm$finalFormula)
m1 <- lm(as.formula(paste0(outcome, "~", final_formula_no_interactions[3])), data = m_dat)
summary(m1)


library(gtsummary)
models <- list(mbias, msens, mspec, mcov)
outcomes <- c("bias", "sensitivity", "specificity", "coverage")

map(models, ~tbl_regression(.x)) %>%
tbl_merge(
    tab_spanner = outcomes
  )


# Load required libraries
library(stargazer)
models <- list(mbias, mcov, msens, mspec)
outcomes <- c("bias", "coverage","sensitivity", "specificity")
# Combine model summaries into a single table
table <- stargazer(
  models,
  type = "text",  # Use "latex" for LaTeX output
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

View(table)
# Print the table
cat(table, file = here("analysis/simulation/data/model/lm_table.tex"))

library(gtsummary)
