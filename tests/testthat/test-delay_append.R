test_that("Can take arbitrary numeric inputs", {
  expect_equal(delay_append(t = -Inf, opt_par = 0.5, delay_distribution = "geometric"), 0)
  expect_equal(delay_append(t = -1, opt_par = 0.5, delay_distribution = "geometric"), 0)
  expect_equal(delay_append(t = Inf, opt_par = 0.5, delay_distribution = "geometric"), 1)
  expect_equal(delay_append(t = -Inf, opt_par = c(5,0.5), delay_distribution = "negative.binomial"), 0)
  expect_equal(delay_append(t = -1, opt_par = c(5,0.5), delay_distribution = "negative.binomial"), 0)
  expect_equal(delay_append(t = Inf, opt_par = c(5,0.5), delay_distribution = "negative.binomial"), 1)
})
test_that("Gives error for non-numeric inputs", {
  expect_error(delay_append(t = "test", opt_par = 0.5, delay_distribution = "geometric"),
               "Non-numeric argument to mathematical function")
  expect_error(delay_append(t = c(1,1), opt_par = "test", delay_distribution = "geometric"),
               "Non-numeric argument to mathematical function")
  expect_error(delay_append(t = "test", opt_par = c(5,0.5), delay_distribution = "negative.binomial"),
               "Non-numeric argument to mathematical function")
})
test_that("Gives error for unsupported 'delay_distributions'", {
  expect_error(delay_append(t = "test", opt_par = c(5,0.5), delay_distribution = "test"),
               "'arg' should be one of")
})
test_that("Gives error for unsupported number of 'opt_par'", {
  expect_error(delay_append(t = c(1), opt_par = c(0.1,0.5), delay_distribution = "geometric"),
               "The 'geometric' distribution only takes scalar 'prob' parameters not")
  expect_error(delay_append(t = c(1), opt_par = c(0.1), delay_distribution = "negative.binomial"),
               "The 'negative.binomial' distribution expects a vector of size two giving the 'size' and 'prob', not")
})
test_that("Gives warning for non-finite values for 'opt_par'", {
  expect_warning(delay_append(t = c(1), opt_par = Inf, delay_distribution = "geometric"),
                 "NaNs produced")
  expect_warning(delay_append(t = c(1), opt_par = c(Inf,0.5), delay_distribution = "negative.binomial"),
                 "NaNs produced")
  expect_warning(delay_append(t = c(1), opt_par = c(Inf,-Inf), delay_distribution = "negative.binomial"),
                 "NaNs produced")
})

