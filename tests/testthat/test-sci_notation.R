#test-sci_notation.r
# library(testthat)

test_that("Check inputs to exponent()", {
  expect_error(exponent())
  expect_error(exponent("a"))
  expect_error(exponent(c(1, 10, 100, 1000, NA, NULL, NaN, Inf)), NA)
})

test_that("Check inputs to mantissa()", {
  expect_error(mantissa())
  expect_error(mantissa("a"))
  expect_error(mantissa(c(1, 10, 100, 1000, NA, NULL, NaN, Inf)), NA)
})
