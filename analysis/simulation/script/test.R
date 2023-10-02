library(testthat)
# n_groups ----------------------------------------------------------------

test_n_groups <- function(scenarios, results, n_sample = NULL) {
  test_that("n_groups in scenarios corresponds to n_groups in results", {
    if (!is.null(n_sample)) {
      sampled_scenarios <- sample(length(scenarios), n_sample)
      scenarios <- scenarios[sampled_scenarios]
      results <- results[sampled_scenarios]
    }

    expect_equal(
      map_dbl(scenarios, ~ .x$n_groups),
      map_dbl(results, ~ skim_without_charts(.x) %>%
                filter(skim_variable == "name") %>%
                pull(character.n_unique)),
      info = "n_groups in scenario does not equal n_groups in results"
    )
  })
}


# skim output -------------------------------------------------------------

test_skim_output <- function(scenarios, results, n_sample = NULL) {
  # Define the test logic within a test_that block
  test_that("No whitespace, empties, and complete cases for the `name` (group) column", {
    n_scenarios <- length(scenarios)

    if (!is.null(n_sample)) {
      sampled_indices <- sample(1:n_scenarios, n_sample)
      scenarios <- scenarios[sampled_indices]
      results <- results[sampled_indices]
    }

    for (i in 1:length(scenarios)) {
      skim <- skim_without_charts(results[[i]]) %>%
        as.data.frame() %>%
        filter(skim_variable == "name")

      expect_equal(
        skim$character.whitespace,
        0,
        info = "Whitespace count for 'name' is not equal to 0"
      )

      expect_equal(
        skim$character.empty,
        0,
        info = "Empty count for 'name' is not equal to 0"
      )

      expect_equal(
        skim$complete_rate,
        1,
        info = "Complete rate for 'name' is not equal to 1"
      )
    }
  })
}





# outcomes_df -------------------------------------------------------------
#outcomes_df is aggregated over the simulations

test_outcomes_df_conditions <- function(outcomes_df) {
  test_that("Testing outcomes_df conditions", {

    # Check condition 1: Sensitivity and Specificity
    expect_true(all(
      xor(!is.na(outcomes_df$sensitivity), !is.na(outcomes_df$specificity))
    ),
    info = "Condition 1 failed: Sensitivity and Specificity are not exclusive")

    # Check condition 2: Significance and Sensitivity/Specificity
    expect_true(all(
      outcomes_df$significance == outcomes_df$sensitivity |
        outcomes_df$significance == outcomes_df$specificity
    ),
    info = "Condition 2 failed: Significance does not match Sensitivity or Specificity")
  })
}

