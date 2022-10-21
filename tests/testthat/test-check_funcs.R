#test-check_funcs.r
# library(testthat)

test_that("Inputs to is_int", {
  expect_error(is.int(c(1, 2, 3, NA, NULL, NaN, Inf)), NA)
  expect_error(is.int())
  expect_error(is.int("a"))
  expect_equal(is.int(c(1, 1L, 1.5)), c(TRUE, TRUE, FALSE))
})
