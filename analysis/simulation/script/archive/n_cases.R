library(here)
library(dplyr)
library(furrr)
plan(multisession, workers = 20)


# create a N-cases folder in data
dir.create(here("analysis/simulation/data", "n_cases"), recursive = FALSE)
simulations_path <- here("analysis/simulation/data", "simulations")
simulations_files <- list.files(path = simulations_path, pattern = "*.rds", full.names = TRUE)


count_cases <- function(df) {
  df %>%
    group_by(scenario, simulation, group) %>%
    summarize(n_cases = n(), .groups="keep") %>%
    ungroup()
}


furrr::future_map(simulations_files, function(i) {

  simulations_list <- readRDS(i)

  n_cases <-
    purrr::map(simulations_list, count_cases) %>%
    bind_rows()

  sim_name <- stringr::str_remove(i, ".*simulations/")

  saveRDS(n_cases,
          here(
            "analysis/simulation/data",
            "n_cases",
            sim_name
          ))

})

n_cases_files <- list.files(
  path =  here("analysis/simulation/data",
               "n_cases"),
  pattern = "*.rds",
  full.names = TRUE
)
cat("Case count completed")
length(n_cases_files)
