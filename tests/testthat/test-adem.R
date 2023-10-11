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
