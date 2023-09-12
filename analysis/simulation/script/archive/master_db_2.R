time_start <- format(Sys.time(), "%Y-%m-%d-%H:%M")
library(DBI)
library(RSQLite)
library(randomNames)
library(here)
library(foreach)
library(doFuture)

# 0. Set-up --------------------------------------------------------------------------------------------
# Output directory
output_dir <- here("analysis/simulation/data")
dir.create(output_dir)

# Database Connection
db_conn <- dbConnect(RSQLite::SQLite(), dbname = "analysis/simulation/data/database.db")

# Unique Random Seed for the run
if (dbExistsTable(db_conn, "run_details")) {
  run_details <- dbGetQuery(db_conn, "SELECT * FROM run_details")
  while (TRUE) {
    rseed <- sample(100000:999999, 1)
    if (!rseed %in% run_details$rseed) {
      break
    }
  }

} else {
  rseed <- sample(100000:999999, 1)
}

# Workers
n_workers <- future::availableCores() - 1
plan(multisession, workers = n_workers)



# 1. Parameters --------------------------------------------------------------------------------------------
n_scenarios <- 5000
n_simulations <- 100
peak_coeffs <- seq(0.7, 1, 0.1)


# 2. Generate scenarios --------------------------------------------------------------------------------------------
source("analysis/simulation/script/generate_scenarios.R")
scenarios <- generate_scenarios(n_scenarios, seed = rseed)
scenarios_db <- lapply(scenarios, function(x) {
  x$generation_time <- NULL
  x$incubation_period <-NULL
  return(x)
})

dbWriteTable(db_conn, "scenarios", dplyr::bind_rows(scenarios_db), append = TRUE)
saveRDS(scenarios, file = paste0(output_dir, "/scenarios.rds"))

# 3. Processing -----------------------------------------------------------------------
process_time <- system.time({

  for (i in seq_along(scenarios)) {
    scenario <- scenarios[[i]]

    # Simulations
    simulations <- foreach(
      j = seq_len(n_simulations),
      .options.future = list(seed = TRUE)
    ) %dofuture% {
      sim <- o2groups::simulate_groups(
        duration = 100,
        n_groups = scenario$n_groups,
        size = scenario$size,
        name = scenario$name,
        delta = scenario$delta,
        intro_n = scenario$intro_n,
        r0 = scenario$r0,
        generation_time = scenario$generation_time$d(0:50),
        incubation_period = scenario$incubation_period$r(500)
      )$data
      sim$scenario <- scenario$scenario
      sim$simulation <- j
      return(sim)
    }

    # Results
    results <-
      foreach(
        k = seq_along(simulations),
        .options.future = list(seed = TRUE)
      ) %dofuture% {
        sim <- simulations[[k]]
        peaks <- o2groups:::get_peak(sim)

        result <- data.frame(
          scenario = character(),
          simulation = integer(),
          peak_coeff = double(),
          group = character(),
          est = numeric(),
          lower_ci = numeric(),
          upper_ci = numeric(),
          successes = numeric(),
          trials = numeric(),
          stringsAsFactors = FALSE
        )

        for (peak_coeff in peak_coeffs) {
          for (group in sort(names(peaks))) {
            d_group <- o2groups::early_delta(
              data = sim,
              min_t = 0,
              max_t = peak_coeff * peaks[[group]],
              size = scenario$size,
              name = scenario$name
            )

            if (!group %in% rownames(d_group)) {
              next
            }

            d_group <- d_group[group, ]
            d_group <- as.data.frame(as.list(d_group))
            d_group$group <- group
            d_group$peak_coeff <- peak_coeff
            d_group$simulation <- k
            d_group$scenario <- scenario$scenario
            result <- rbind(result, d_group)
          }
        }
        return(result)
      }

    # Save to database
    dbWriteTable(db_conn, "simulations", dplyr::bind_rows(simulations), append = TRUE)
    dbWriteTable(db_conn, "results", dplyr::bind_rows(results), append = TRUE)
  }
})

# 4. Save the run details --------------------------------------------------------------------------------------------
time_end <- format(Sys.time(), "%Y-%m-%d-%H:%M")

# Convert to POSIXct datetime objects
time_start <- as.POSIXct(time_start, format = "%Y-%m-%d-%H:%M")
time_end <- as.POSIXct(time_end, format = "%Y-%m-%d-%H:%M")
totaltime <- time_end - time_start

run_details <- data.frame(
  rseed = rseed,
  workers = n_workers,
  time_start = as.character(time_start),
  time_end = as.character(time_end),
  totaltime = as.numeric(totaltime),
  n_scenarios = n_scenarios,
  n_simulations = n_simulations,
  peak_coeffs = paste(peak_coeffs, collapse = ", "),
  process_time = process_time[[3]]
)
#save to database
dbWriteTable(db_conn, "run_details", run_details, append = TRUE)

# 5. Close the database --------------------------------------------------------------------------------------------
dbDisconnect(db_conn)


# Query ---------------------------------------------------------------------------------------

# #connect to database
# db_conn <- dbConnect(RSQLite::SQLite(), dbname = "analysis/simulation/data/database.db")
#
# # Query the results
# query <- "SELECT * FROM results"
# results <- dbGetQuery(db_conn, query)
#
# # check that there are 100 simulations ids for each scenario
# results %>%
#   group_by(scenario) %>%
#   summarise(n = n_distinct(simulation))
