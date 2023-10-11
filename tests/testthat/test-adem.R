test_that("Throws error for unsupported inputs", {
  expect_error(adem(threshold_method = "test"),
               "`threshold_method` must be one of")
  expect_error(adem(delay_distribution = "test"),
               "`delay_distribution` must be one of")
  expect_error(adem(fit_distribution = "test"),
               "`fit_distribution` must be one of")
  expect_error(adem(units = "test"),
               "`units` must be one of")

})
test_that("Miscellaneous tests with adem_results", {

  deaths <- MASS::deaths

  fit_formula <- deaths ~ 1 + t

  adem_results <- adem(
    data = c(deaths),
    data_start = c(1974, 1),
    data_end = c(1979, 12),
    data_frequency = 12,
    k = 24,
    fit_formula = fit_formula,
    theta_start = c(7, 0),
    threshold_method = "quantile")

  # Test that the output is a tibble
  expect_true(tibble::is_tibble(adem_results) == TRUE)

})

