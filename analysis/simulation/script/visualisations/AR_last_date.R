library(tidyverse)
library(here)
library(furrr)
library(furrr)

n_workers <- future::availableCores() - 2
plan(multisession, workers = n_workers)

remove_object <- function(list, object_name) {
  list <- list[!names(list) %in% object_name]
  return(list)
}

data_path <- "analysis/simulation/data"

scenario_ids <- list.files(
  path = here(data_path, "scenarios"),
  pattern = "*.rds",
  full.names = FALSE
)

AR_date <- function(scenario_id){
  scenario <-
    readRDS(here(data_path, "scenarios", scenario_id)) %>%
    remove_object(object_name = names(.)[!names(.) %in% c("scenario", "size")])


  population_size <- sum(scenario$size)

  simulations <-
    readRDS(here(data_path, "simulations", scenario_id)) %>%
    bind_rows() %>%
    group_by(simulation) %>%
    summarise(
      scenario_id = scenario_id,
      attack_rate = n() / population_size ,
      last_date = max(date_infection)
    )

  return(simulations)
}


tictoc::tic()
AR_df <- furrr::future_map(scenario_ids, .f = AR_date) %>%
  bind_rows()
tictoc::toc()




AR_df %>%
  ggplot(aes(x = last_date, y = attack_rate)) +
  geom_point()+
  labs(x = "Last date of infection", y = "Attack rate (total infections / population size)")

AR_df %>%
  ggplot(aes(x = last_date, y = attack_rate)) +
  geom_hex(bins = 100) +
  scico::scale_fill_scico(palette = "lajolla")+
  labs(x = "Last date of infection", y = "Attack rate (total infections / population size)")


# display distribution of data using geom_bar
AR_df %>%
  ggplot(aes(x = last_date, y = attack_rate)) +
  geom_bar(stat = "identity") +
  labs(x = "Last date of infection", y = "Attack rate (total infections / population size)")

# display as a distribution instead using violin plots
AR_df %>%
  mutate(last_date_chop = santoku::chop(last_date, breaks = seq(0,100, by = 10))) %>%
  ggplot(aes(x = last_date_chop, y = attack_rate)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(x = "Last date of infection", y = "Attack rate (total infections / population size)")

AR_df %>%
  ggplot(aes(x = last_date, y = attack_rate)) +
  stat_density_2d_filled(aes(fill = after_stat(level) ),contour_var = "ndensity")+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Last date of infection", y = "Attack rate (total infections / population size)")


# count the number of observations per last_date
AR_df %>%
  count(last_date) %>%
  ggplot(aes(x = last_date, y = n)) +
  geom_col() +
  labs(x = "Last date of infection", y = "Number of simulations")


