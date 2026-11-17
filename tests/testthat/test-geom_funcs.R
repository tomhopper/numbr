# Unit tests for area_hex ####

testthat::test_that("Missing parameters", {
  expect_error(area_hex())
})
testthat::test_that("Suspicious parameters", {
  testthat::expect_warning(area_hex(3, 6))
})

testthat::test_that("Invalid parameters", {
  expect_error(area_hex(3, "a"))
  expect_error(area_hex("a", 6))
  expect_error(area_hex(3, TRUE))
  expect_error(area_hex(TRUE, 6))
})
