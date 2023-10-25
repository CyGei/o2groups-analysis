library(noah)
# Description ---------------------------------------------------------------------------------
# This script generates scenarios random changes to the parameters (under various constraints)
# It saves the scenarios in 2 formats: list and dataframe.

# Function -------------------------------------------------------------------------------------
# Helper Function
generate_scenarios_helper <- function() {
  duration <- 100

  n_groups <- round(truncnorm::rtruncnorm(n = 1, a = 1.5, b = 8.1, mean = 2.5, sd = 3))

  size <- round(runif(n_groups, min = 20, max = 200))

  name <- LETTERS[1:n_groups]

  p_zero <- runif(n_groups, min = 0, max = 1)

  scaled_delta <-
    ifelse(p_zero > 0.5,
      0,
      truncnorm::rtruncnorm(
        n_groups,
        a = -1,
        b = 1,
        mean = 0,
        sd = 0.35
      )
    )

  delta <- o2groups::reverse_scale(scaled_delta)

  # 1 random introduction + sampling
  intro_n <- integer(n_groups)
  intro_n[sample(n_groups, 1)] <- 1
  for (i in seq_along(n_groups)) {
    intro_n[i] <- intro_n[i] + round(runif(1, min = 0, max = 0.1 * size[i]))
  }

  r0 <- truncnorm::rtruncnorm(n_groups, a = 1, mean = 2, sd = 2)

  GT_mean <- truncnorm::rtruncnorm(1, a = 1, mean = 4, sd = 3)
  GT_sd <- truncnorm::rtruncnorm(1, a = 1, mean = 1.5, sd = 2)
  generation_time <- simulacr::make_disc_gamma(mean = GT_mean, sd = GT_sd)

  INCUB_mean <- truncnorm::rtruncnorm(1, a = 1, mean = 4, sd = 3)
  INCUB_sd <- truncnorm::rtruncnorm(1, a = 1, mean = 1.5, sd = 2)
  incubation_period <- simulacr::make_disc_gamma(mean = INCUB_mean, sd = INCUB_sd)

  scenario <- list(
    scenario = NULL,
    duration = duration,
    n_groups = n_groups,
    size = size,
    name = name,
    delta = delta,
    scaled_delta = scaled_delta,
    intro_n = intro_n,
    r0 = r0,
    GT_mean = GT_mean,
    GT_sd = GT_sd,
    INCUB_mean = INCUB_mean,
    INCUB_sd = INCUB_sd,
    generation_time = generation_time,
    incubation_period = incubation_period
  )

  return(scenario)
}

# Main function
generate_scenarios <- function(n, seed = 123) {
  set.seed(seed)
  scenarios <- vector("list", n)
  ark <- Ark$new(seed = seed)
  pseudonyms <- paste0(noah::pseudonymize(as.integer(1:n), .ark = ark))


  for (i in 1:n) {
    scenario <- generate_scenarios_helper()
    scenario$scenario <-
      paste0(
        pseudonyms[i],
        "_",
        format(Sys.time(), "%Y%m%d%H%M%S")
      )
    scenarios[[i]] <- scenario
  }
  return(scenarios)
}

# Function Call -------------------------------------------------------------------------------
#
# scenarios <- generate_scenarios(10)


# Save Output ----------------------------------------------------------------------
# saveRDS(scenarios, "analysis/simulation/data/scenarios.rds")



# Draft to check for duplicates -------------------------------------------
# # Check for duplicate pseudonyms and add an additive integer
# pseudonym_counts <- table(pseudonyms)
# duplicates <- pseudonyms[duplicated(pseudonyms)]
# for (i in seq_along(pseudonyms)) {
#   if (pseudonyms[i] %in% duplicates) {
#     additive_integer <- pseudonym_counts[pseudonyms[i]]
#     pseudonyms[i] <- paste0(pseudonyms[i], "_", additive_integer)
#     pseudonym_counts[pseudonyms[i]] <- additive_integer + 1
#   }
# }

# pseudonym_counts <- table(pseudonyms)
# duplicates <- pseudonyms[duplicated(pseudonyms)]
# pseudonyms <- purrr::map2(pseudonyms, pseudonym_counts, ~ {
#   if (.x %in% duplicates) {
#     additive_integer <- .y
#     paste0(.x, "_", additive_integer)
#   } else {
#     .x
#   }
# })
