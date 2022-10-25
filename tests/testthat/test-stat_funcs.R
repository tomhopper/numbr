# test-stat_funcs.R
# library(testthat)

# Unit tests for mean_geom()

test_that("Check invalid inputs to mean_geom are captured", {
  expect_error(mean_geom(c(10L, 1L, 5L, "a")))
  expect_error(mean_geom())
})

test_that("Check valid output from mean_geom with both integer and double input", {
  expect_type(object = mean_geom(c(10L, 1L, 5L)), type = "double")
  expect_type(object = mean_geom(c(10.0, 1.0, 5.0)), type = "double")
  expect_type(object = mean_geom(c(10.5, 1.1, 5.6)), type = "double")
  expect_type(object = mean_geom(c(10L, 1L, 5L, NA)), type = "double")
  expect_type(object = mean_geom(c(10.0, 1.0, 5.0, NA)), type = "double")
  expect_equal(object = mean_geom(c(10L, 1L, 5L, NA), na.rm = TRUE), expected = mean_geom(c(10L, 1L, 5L)))
  expect_equal(object = mean_geom(c(10.0, 1.0, 5.0, NA), na.rm = TRUE), expected = mean_geom(c(10L, 1L, 5L)))
})


# Unit tests for mode_stat()

test_that("stat_mode input handling", {
  expect_error(mode_stat(c(complex(1, 1), complex(1, 1), complex(1, 2)), method = "density"))
})


## PRESS ####



## pred_r_squared ####



## model_fit_stats ####



## nCr ####



## nPr ####
