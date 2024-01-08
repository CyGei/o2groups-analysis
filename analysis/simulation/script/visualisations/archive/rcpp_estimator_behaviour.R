library(parallel)
library(Rcpp)

# Define the estimator function in C++
cppFunction('
NumericVector estimatorCpp(NumericVector x,
NumericVector n,
NumericVector f,
double alpha = 0.05) {
  int size = x.size();
  NumericVector result(size);

  for (int i = 0; i < size; ++i) {
    double binom_est = R::pbinom(x[i], n[i], f[i], 0, 0);
    double numerator = binom_est * (1 - f[i]);
    double denominator = f[i] * (1 - binom_est);
    double gamma = numerator / denominator;
    double delta = std::isinf(gamma) ? 1 : (std::isnan(gamma) ? NA_REAL : (std::isfinite(gamma) ? (gamma - 1) / (gamma + 1) : 0));
    result[i] = delta;
  }

  return result;
}')

# Function to apply the estimator in parallel
estimator_parallel <- function(x, n, f, alpha = 0.05) {
  options(mc.cores = parallel::detectCores())
  mclapply(1:length(x), function(i) {
    estimatorCpp(x[i], n[i], f[i], alpha)
  })
}

# Create grid
grid <- expand.grid(
  x = seq(0, 100, by = 2),
  n = seq(0, 100, by = 2),
  f = seq(0.01, 1-0.05, by = 0.05),
  delta = runif(51, -1, 1)
)
grid <- grid[grid$n >= grid$x & grid$n > 0,]

# Apply estimator in parallel
grid$est <- unlist(estimator_parallel(grid$x, grid$n, grid$f))
grid$bias <- grid$delta - grid$est



ggplot(grid,
       aes(
         x = f,
         y = x / n,
         fill = bias
       )) +
  geom_tile(width = 0.05,
            height = 0.05,
            color = NA,
            lwd = 0) +
  scico::scale_fill_scico(palette = 'vik') +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.25),
    expand = c(0, 0.01)) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    expand = c(0, 0.01))
