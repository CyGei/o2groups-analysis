# List of scenarios considered in the analysis.
# Note: dissorative coeff = 1/assortative coeff


REF_n_groups <- 2
REF_size <- rep(50, 2)
REF_name <- LETTERS[1:2]
REF_delta <- c(10, 1 / 10)
REF_intro_group <- LETTERS[1:2]
REF_intro_n <- rep(1, 2)
REF_r0 <- rep(2, 2)
REF_generation_time <- simulacr::make_disc_gamma(mean = 5, sd = 2)
REF_incubation_period <- simulacr::make_disc_gamma(mean = 3, sd = 1)

scenarios <- list(
  # Null Scenario : Neutral Mixing
  list(
    scenario = "NULL - Neutral Mixing",
    param = "Delta",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = c(1, 1),
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "Delta: 10, 1.5",
    param = "Delta",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = c(10, 1.5),
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "Delta: 2, 1/2",
    param = "Delta",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = c(2, 1 / 2),
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "BASE - Delta: 10, 1/10 ",
    param = "Delta",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),

  # Size: Large Groups, Small Groups, Different Sizes
  list(
    scenario = "Size: 100",
    param = "Size",
    n_groups = REF_n_groups,
    size = rep(100, 2),
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "Size: 25",
    param = "Size",
    n_groups = REF_n_groups,
    size = rep(25, 2),
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "Size: 25, 250",
    param = "Size",
    n_groups = REF_n_groups,
    size = c(25, 250),
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),

  # R0: Low R0, High R0, Different R0
  list(
    scenario = "R0: 1.5",
    param = "r0",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = rep(1.5, 2),
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "R0: 2.5",
    param = "r0",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = rep(2.5, 2),
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "R0: 1.5, 2.5",
    param = "r0",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = c(1.5, 2.5),
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),

  # Generation Time:.
  list(
    scenario = "GT: mean 10, sd 1",
    param = "generation_time",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = simulacr::make_disc_gamma(mean = 10, sd = 1),
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "GT: mean 2, sd 1",
    param = "generation_time",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = simulacr::make_disc_gamma(mean = 2, sd = 1),
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "GT: mean 5, sd 10",
    param = "generation_time",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = REF_intro_n,
    r0 = REF_r0,
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 10),
    incubation_period = REF_incubation_period
  ),

  # Introduction: Single Introduction, Different Introduction Sizes
  list(
    scenario = "Single Introduction Group A",
    param = "intro_group",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = LETTERS[1],
    intro_n = 1,
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "Single Introduction Group B",
    param = "intro_group",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = LETTERS[2],
    intro_n = 1,
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "20% introductions",
    param = "intro_group",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = rep(20, 2),
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "10% introductions",
    param = "intro_group",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = rep(10, 2),
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "5% introductions",
    param = "intro_group",
    n_groups = REF_n_groups,
    size = REF_size,
    name = REF_name,
    delta = REF_delta,
    intro_group = REF_intro_group,
    intro_n = rep(5, 2),
    r0 = REF_r0,
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),

  # Group number:
  list(
    scenario = "3 groups: 3rd neutral",
    param = "n_groups",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1 / 10, 1),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  ),
  list(
    scenario = "6 groups: Group 3-6 assortative/dissorative",
    param = "n_groups",
    n_groups = 6,
    size = rep(100, 6),
    name = LETTERS[1:6],
    delta = rep(c(10, 1 / 10), 3),
    intro_group = LETTERS[1:6],
    intro_n = rep(1, 6),
    r0 = rep(2, 6),
    generation_time = REF_generation_time,
    incubation_period = REF_incubation_period
  )
)

# save files individually in the scenarios folder
purrr::walk(scenarios, function(scenario) {
  scenario_file <- paste0(
    here("analysis/manual/data/scenarios"),
    "/",
    janitor::make_clean_names(scenario$scenario),
    ".rds"
  )

  if (file.exists(scenario_file)) {
    stop(paste0("File already exists: ", scenario_file))
  } else {
    saveRDS(scenario, scenario_file)
  }
})
rm(list =ls())
