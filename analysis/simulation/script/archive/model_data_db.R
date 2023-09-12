# Set-Up ------------------------------------------------------------------
library(here)
library(tidyverse)
library(dbplyr)
library(DBI)
library(dbplot)

standardise <- function(x) {
  x <- as.numeric(x)
  d <- ifelse(is.finite(x),
              (x - 1) / (x + 1),
              1.0)
  return(d)
}


reverse_standardise <- function(d) {
  x <- (1 + d) / (1 - d)
  return(x)
}

normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

db_conn <-
  dbConnect(RSQLite::SQLite(), dbname = here("analysis/simulation/data", "database.db"))
results <- dplyr::tbl(db_conn, "results")
scenarios <- dplyr::tbl(db_conn, "scenarios")

# Model Df ---------------------------------------------------------------
model_df <- results %>%
  left_join(scenarios, by = c("scenario", "group" = "name")) %>%
  select(
    scenario,
    simulation,
    peak_coeff,
    group,
    n_groups,
    size,
    intro_n,
    r0,
    GT_mean,
    GT_sd,
    INCUB_mean,
    INCUB_sd,
    duration,
    est,
    lower_ci,
    upper_ci,
    successes,
    trials,
    delta
  )


model_df <- model_df %>%
  mutate(
    scenario = as.character(scenario),
    simulation = as.character(simulation),
    peak_coeff = as.character(peak_coeff),
    group = as.character(group),
    delta = case_when(
      delta == Inf  ~ 1.0,
      TRUE ~ (delta - 1) / (delta + 1)
    ),
    est = case_when(
      est == Inf  ~ 1.0,
      TRUE ~ (est - 1) / (est + 1)
    ),
    lower_ci = case_when(
      lower_ci == Inf  ~ 1.0,
      TRUE ~ (lower_ci - 1) / (lower_ci + 1)
    ),
    upper_ci = case_when(
      upper_ci == Inf  ~ 1.0,
      TRUE ~ (upper_ci - 1) / (upper_ci + 1)
    ),
    bias = est - delta
  )

# VISU ----------------------------------------------------------------------------------------

# Define the predictors
predictors <- c("delta", "n_groups", "size", "intro_n", "r0", "GT_mean", "GT_sd")

# Plot relationship function using dbplot
plot_relationship <- function(predictor) {
  gg <- model_df %>%
    dbplot_raster(.data[[predictor]], bias,
                  fill = peak_coeff,
                  resolution = 100) +
    geom_hline(aes(yintercept = 0)) +
    labs(
      title = paste("Relationship between", predictor, "and Bias"),
      x = predictor,
      y = "bias"
    )

  ggsave(filename = here(
    "analysis/simulation/plots",
    paste0(predictor, "_plot.png")
  ),
  plot = gg)
}
system.time(
  lapply(predictors, plot_relationship)
)

library(corrplot)

correlation_matrix <- cor(model_df[, c("bias", predictors)])
corrplot(correlation_matrix, method = "color")




# Modelling -----------------------------------------------------------------------------------

predictors <- c("delta", "n_groups", "size", "intro_n", "r0", "GT_mean", "GT_sd")
outcome <- "bias"

summary_df <- model_df %>%
  filter(as.numeric(scenario) <= 400) %>%
  group_by(scenario, simulation, peak_coeff) %>%
  summarise(across(all_of(c(predictors, outcome)), list(mean = ~mean(.), sd = ~sd(.)),
                   .names = "{.col}_{.fn}"))

# plot th relationship between bias and delta using density plots
ggplot(summary_df, aes(x = delta_mean, y = bias_mean)) +
  geom_point(aes(col = peak_coeff)) +
  geom_hline(aes(yintercept = 0), col = "steelblue")+
  labs(x = "Delta", y = "Bias") +
  ggtitle("Relationship between Delta and Bias")


ggplot(summary_df, aes(x = delta_mean, y = bias_mean)) +
  geom_density_2d(aes(fill = ..level..), alpha = 0.6) +
  geom_hline(yintercept = 0, col = "steelblue") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Delta", y = "Bias") +
  ggtitle("Relationship between Delta and Bias using Density Plots")


# Melting the summary_df for easier plotting
melted_summary <- summary_df %>%
  pivot_longer(cols = starts_with(predictors), names_to = "predictor", values_to = "value")

ggplot(melted_summary, aes(x = value, y = bias_mean )) +
  geom_violin() +
  facet_wrap(~predictor, scales = "free_x")+
  labs(x = "Predictor", y = "Bias") +
  ggtitle("Distribution of Bias by Predictor")


# Creating density distribution plots
ggplot(melted_summary, aes(x = value, y = ..density.., fill = predictor)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ predictor, scales = "free") +
  labs(x = "Value", y = "Density", fill = "Predictor") +
  ggtitle("Density Distribution of Bias by Predictor")



# MODELLING -------------------------------------------------------------------
#model_df <- readRDS("analysis/simulation/data/model_df.rds")
library(lme4)

model_formula <- "bias ~ size + delta + n_groups + (1 | scenario) + (1 | simulation)"
model <- lmer(model_formula, data = model_df)
model <- lmer(bias ~  delta  + n_groups + size + intro_n + r0 + GT_mean + GT_sd
              + (1 | scenario/simulation/peak_coeff),
              data = model_df)




# Fit the model
fit <- summary(model)

# View the model summary
print(fit)



library(randomForest)
# Convert factors to numeric for random forest
model_df$scenario <- as.numeric(model_df$scenario)
model_df$simulation <- as.numeric(model_df$simulation)
model_df$peak_coeff <- as.numeric(model_df$peak_coeff)
model_df$group <- as.numeric(model_df$group)

# Define the Random Forest model
rf_model <- randomForest(bias ~ delta + n_groups + size + intro_n + r0 + GT_mean + GT_sd, data = model_df)

# View the model summary
print(rf_model)
summary(rf_model)

# Feature importance plot
varImpPlot(rf_model, main = "Variable Importance Plot")
