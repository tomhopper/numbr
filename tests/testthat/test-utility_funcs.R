# test-utility_funcs.R
# library(testthat)

test_that("nin inputs and ouputs", {

})

# Unit tests for %==%

test_that("%==% valid output", {
  expect_identical(c(2,3,4) %==% c(1, 3, 5), expected = c(FALSE, TRUE, FALSE))
})

test_that("%==% catch input errors", {
  #expect_error(object = `%==%`)
  expect_error(object = `3 %==% "e"`)
  expect_error(object = `"e" %==% 3`)
})

