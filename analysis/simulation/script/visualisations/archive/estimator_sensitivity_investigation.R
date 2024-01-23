library(tidyverse)
library(here)
source(here("analysis/simulation/script/visualisations/plot_helpers.R"))

# CI variance -------------------------------------------------------------

binom_variance <- function(n, p) {
  n * p * (1 - p)
}

grid <- expand.grid(n = 1:100, p = seq(0, 1, 0.01))
grid$variance <- binom_variance(grid$n, grid$p)

ggplot(grid, aes(x = p, y = n, fill = variance)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "p", y = "n")

binom_variance(100, 0.5)
binom_variance(10, 0.5)
binom_variance(100, 0.1)
binom_variance(10, 0.1)
binom_variance(100, 0.9)
binom_variance(10, 1)
binom_variance(100, 0)

f_variance <- function(f) {
  (1 - f) / f
}

grid <- expand.grid(f = seq(0, 1, 0.001))
grid$variance <- f_variance(grid$f)

ggplot(grid, aes(x = f, y = variance)) +
  geom_line() +
  labs(x = "f", y = "variance") +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  theme_publication() +
  labs(x = latex2exp::TeX("$f$", italic = TRUE),
       y = latex2exp::TeX("$\\sigma^2 = (\\frac{1-f}{f})$"))






# Specify x/n values
target_values <- c(0.25, 0.75) #c(0, 0.25, 0.75, 1)
# Initialize an empty data frame to store results
pi_df <- data.frame()

# Loop through possible combinations to find x and n values
for (x_over_n in target_values) {
  for (n in seq(10, 100, by = 10)) {
    x <- round(x_over_n * n)

    # Ensure that x is a valid value (between 0 and n)
    if (x >= 0 && x <= n) {
      result <- data.frame(x, n, b_est(x, n))
      pi_df <- rbind(pi_df, result)
    }
  }
}
pi_df <- pi_df %>% mutate(
  pi = case_when(
    est == 0.5 ~ "0.5",
    est > 0.5 ~ "0.75",
    est < 0.5 ~ "0.25"
  )
)

estimator <- function(beta, f) {
  gamma <- o2groups::get_gamma(beta = beta, f = f)
  delta <- o2groups::scale(gamma)
  return(delta)
}


ggplot(pi_df,
       aes(x = n,
           y = est,
           color = x)) +
  geom_hline(yintercept = target_values,
             linetype = "dashed",
             color = "grey") +
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  position =  position_dodge2(width = 5)) +
  scale_color_gradientn(
    colours = c("black", "#FF00FF"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_publication()+
  labs(
    y = latex2exp::TeX("$\\bar{\\pi}_{a\\leftarrow a}$"),
    x = latex2exp::TeX("$\\tau_{.\\leftarrow a}$"),
    color = latex2exp::TeX("$\\tau_{a\\leftarrow a}$"),
    title =  latex2exp::TeX("For a given value $\\bar{\\pi}_{a\\leftarrow a}$ what's the effect of $\\tau_{.\\leftarrow a}$ & $\\tau_{a\\leftarrow a}$?")
  )








f_values <- c(0.1, 0.5, 0.9)

delta_df <- map(.x = f_values, .f = function(f){
  mutate(.data = pi_df,
         across(
           .cols = c("est", "lower_ci", "upper_ci"),
           .fns = \(x) estimator(beta = x, f = f)
         ),
         f = f)
}) %>%
  bind_rows()




delta_df %>%
  mutate(f = as.factor(f)) %>%
  ggplot(
    aes(x = n,
        y = est,
        color = x)) +
  facet_wrap(~f,
             labeller = labeller(f = function(x) paste0("f = ", x))) +
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci,
                      shape = pi),
                  position =  position_dodge2(width = 5)) +
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "red") +
  scale_color_gradientn(
    colours = c("black", "#FF00FF"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_publication()+
  labs(
    y = latex2exp::TeX("$\\bar{\\delta}_a$"),
    x = latex2exp::TeX("$\\tau_{.\\leftarrow a}$"),
    color = latex2exp::TeX("$\\tau_{a\\leftarrow a}$"),
    title =  latex2exp::TeX("Estimates of $\\bar{\\delta}_a$ by $\\tau_{a\\leftarrow a}$, $\\tau_{.\\leftarrow a}$ & $f$"),
    shape = latex2exp::TeX("$\\bar{\\pi}_{a\\leftarrow a}$")
  )



# %>%
#   rename_with(
#     .fn = \(x) paste0("delta_", x),
#     .cols =  c("est", "lower_ci", "upper_ci")
#   )

b_est(0, 10) %>%
  o2groups::get_gamma(beta = ., f = 0.5) %>%
  o2groups::scale()

# Plot results
delta_df %>%
  ggplot(
    aes(x = n,
        y = est,
        color = x)) +
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  position =  position_dodge2(width = 5)) +
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "red") +
  scale_color_gradientn(
    colours = c("black", "#FF00FF"),
    values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
    na.value = "gray"
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_publication()+
  labs(
    y = latex2exp::TeX("$\\bar{\\delta}_a$"),
    x = latex2exp::TeX("$\\tau_{.\\leftarrow a}$"),
    color = latex2exp::TeX("$\\tau_{a\\leftarrow a}$"),
    title =  latex2exp::TeX("Estimates of $\\bar{\\delta}_a$ by $\\tau_{.\\leftarrow a}$ & $\\tau_{a\\leftarrow a}$")
  )
