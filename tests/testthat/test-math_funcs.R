#test_math_funcs.r
# library(testthat)
library(tibble)

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

# Unit tests for %==%

test_that("%==% valid output", {
  expect_identical(c(2,3,4) %==% c(1, 3, 5), expected = c(FALSE, TRUE, FALSE))
})

test_that("%==% catch input errors", {
  #expect_error(object = `%==%`)
  expect_error(object = `3 %==% "e"`)
  expect_error(object = `"e" %==% 3`)
})

# Unit tests for num_order_to_word()

test_that("Missing and incorrect input to x", {
  expect_error(num_order_to_word())
  expect_error(num_order_to_word(x = "a"))
  expect_error(num_order_to_word(x = list(5435435, 55435435)))
  expect_error(num_order_to_word(x = data.frame(a = c(5435435, 55435435))))
})

test_lookup <- tibble(expon = c(33, 30, 27, 24, 21, 18, 15, 12, 9, 6, 3,
                 0, -3, -6, -9, -12),
       word = c("decillion", "nonillian", "octillian",
                "septillion", "sextillion", "quintillion", "quadrillion",
                "trillion", "billion", "million", "thousand",
                "", "thousandth", "millionth", "billionth",
                "trillionth"))
test_lookup2 <- test_lookup
test_lookup2[["expon"]] <- as.integer(test_lookup2[["expon"]])
test_lookup3 <- test_lookup
test_lookup3[["word"]] <- as.factor(test_lookup3[["word"]])

test_that("Check input of lookup table.", {
  expect_error(num_order_to_word(5435435, lookup = test_lookup), NA)
  expect_error(num_order_to_word(5435435, lookup = test_lookup2), NA)
  expect_error(num_order_to_word(5435435, lookup = test_lookup3), NA)
})

rm(test_lookup, test_lookup2, test_lookup3)

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
