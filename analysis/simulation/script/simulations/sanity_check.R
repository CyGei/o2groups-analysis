# Model Sanity Check
# The purpose of this script is to check wether the proportions of within and between group transmission
# are simulated as expected.

pacman::p_load_gh("CyGei/o2groups") # for simulation
pacman::p_load_gh("CyGei/linktree@realtime") # for analysis
pacman::p_load(dplyr)


# Test1 -------------------------------------------------------------------
# 2 group simulation
set.seed(123)
duration = 100
group_n = 2
size = c(300, 300)
name = c("HCW", "patient")
gamma = c(1, 1)
intro_n = c(1, 1)
r0 = c(2, 2)
generation_time = c(0, 0.1, 0.2, 0.4, 0.2, 0.1, 0)
incubation_period = sample(1:14, sum(size), replace = TRUE)

set.seed(123)
sim <- simulate_groups(
  duration = duration,
  group_n = group_n,
  size = size,
  name = name,
  gamma = gamma,
  intro_n = intro_n,
  r0 = r0,
  generation_time = generation_time,
  incubation_period = incubation_period
)
head(sim)
#% of within group transmission
linktree::get_pi(from = sim$source_group, to = sim$group) %>%
  mutate(across(where(is.numeric),\(x) round(x, 2)))
size / sum(size) %>% round(2)



# Test 2 ------------------------------------------------------------------
# 3 group simulation
set.seed(123)
group_n = 3
size = c(300, 300, 300)
name = c("HCW", "patient", "other")
gamma = c(1, 1, 1)
intro_n = c(1, 1, 1)
r0 = c(2, 2, 2)

sim <- simulate_groups(
  duration = duration,
  group_n = group_n,
  size = size,
  name = name,
  gamma = gamma,
  intro_n = intro_n,
  r0 = r0,
  generation_time = generation_time,
  incubation_period = incubation_period
)
head(sim)

# % of within group transmission
linktree::get_pi(from = sim$source_group, to = sim$group) %>%
  mutate(across(where(is.numeric),\(x) round(x, 2)))
size / sum(size) %>% round(2)

# % of between group transmission
ttab <- linktree:::ttable(from = sim$source_group, to = sim$group, level = name)
prop.table(ttab, 1)
prop.table(ttab, 1)[!diag(1, nrow(ttab))] %>% round(2) %>% mean()
