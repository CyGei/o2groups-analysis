time_start <- format(Sys.time(), "%Y-%m-%d-%H:%M")
library(DBI)
library(RSQLite)
library(randomNames)
library(here)
library(foreach)
library(doFuture)


# 0. Parameters --------------------------------------------------------------------------------------------
n_scenarios <- 10
n_simulations <- 100
peak_coeffs <- seq(0.7, 1, 0.1)
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
plan(multisession, workers = 4)

process_time <- system.time({
  for (i in seq_along(scenarios)) {
    scenario <- scenarios[[i]]

    # Simulations
    system.time({
      results <- foreach(
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

        peaks <- o2groups:::get_peak(sim)

        result <- data.frame(
          scenario = character(),
          simulation = integer(),
          peak_coeff = double(),
          group = character(),
          est = numeric(), lower_ci = numeric(), upper_ci = numeric(),
          successes = numeric(), trials = numeric(), stringsAsFactors = FALSE
        )
        # loop through each peak coefficient
        for (peak_coeff in peak_coeffs) {
          for (group in sort(names(peaks))) {
            d_group <- o2groups::early_delta(
              data = sim, min_t = 0,
              max_t = peak_coeff * peaks[[group]], size = scenario$size,
              name = scenario$name
            )

            if (!group %in% rownames(d_group)) {
              next
            }

            d_group <- d_group[group, ]
            d_group <- as.data.frame(as.list(d_group))
            d_group$group <- group
            d_group$peak_coeff <- peak_coeff
            d_group$simulation <- j
            d_group$scenario <- scenario$scenario
            result <- rbind(result, d_group)
          }
        }
        return(result)
      }
    })
    # Save to database
    dbWriteTable(db_conn, "results", bind_rows(results), append = TRUE)
  }

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
        file = paste0(output_dir, "/run_details_", Sys.Date(), ".rds")
)



# Query ---------------------------------------------------------------------------------------

# # Example SQL query
# query <- "SELECT * FROM results"
#
# # Execute the query and retrieve the results as a data frame
# results <- dbGetQuery(db_conn, query)
