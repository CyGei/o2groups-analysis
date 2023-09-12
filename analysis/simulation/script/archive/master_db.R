time_start <- format(Sys.time(), "%Y-%m-%d-%H:%M")
library(DBI)
library(RSQLite)
library(randomNames)
library(here)


# 0. Parameters --------------------------------------------------------------------------------------------
n_scenarios <- 3000
peak_coeffs <- seq(0.7 , 1, 0.1)
rseed <- sample(100000:999999, 1)


# 1. Database Set-up --------------------------------------------------------------------------------------------
output_dir <- here("analysis/simulation/data")
db_conn <-
  dbConnect(RSQLite::SQLite(), dbname = paste0(output_dir, "/database.db"))

# 2. Generate scenarios --------------------------------------------------------------------------------------------
source("analysis/simulation/script/generate_scenarios.R")
scenarios <- generate_scenarios(n_scenarios, seed = rseed)
saveRDS(scenarios, paste0(output_dir, "/scenarios.rds"))

# 3. Processing -----------------------------------------------------------------------
source("analysis/simulation/script/run_simulations.R")

process_time <- system.time({
  for (i in seq_along(scenarios)) {
    scenario <- scenarios[[i]]
    simulation <- run_simulations_db(scenario)

    result <- purrr::map_dfr(peak_coeffs, ~ {
      peak_est <-
        o2groups::process_simulation(simulation, .x, scenario$size, scenario$name) %>%
        mutate(scenario = scenario$scenario, peak_coeff = .x)
      return(peak_est)
    })

    dbWriteTable(db_conn, "simulations", simulation, append = TRUE)
    dbWriteTable(db_conn, "results", result, append = TRUE)

  }

  rm(list = c("scenario", "simulation"))
})

dbDisconnect(db_conn)

# 4. Save the run details --------------------------------------------------------------------------------------------
time_end <- format(Sys.time(), "%Y-%m-%d-%H:%M")

# Convert to POSIXct datetime objects
time_start <- as.POSIXct(time_start, format = "%Y-%m-%d-%H:%M")
time_end <- as.POSIXct(time_end, format = "%Y-%m-%d-%H:%M")
totaltime <- time_end - time_start

run_details <- data.frame(
  rseed = rseed,
  time_start = as.character(time_start),
  time_end = as.character(time_end),
  totaltime = as.numeric(totaltime),
  n_scenarios = n_scenarios,
  peak_coeffs = paste(peak_coeffs, collapse = ", "),
  process_time = process_time[[3]]
  )
saveRDS(run_details,
        file = paste0(output_dir, "/run_details_", Sys.Date(), ".rds"))



# Query ---------------------------------------------------------------------------------------

# # Example SQL query
# query <- "SELECT * FROM results"
#
# # Execute the query and retrieve the results as a data frame
# results <- dbGetQuery(db_conn, query)


