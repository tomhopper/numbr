#test_math_funcs.r
# library(testthat)
library(tibble)

# Unit tests for num_order_to_word() ####

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


# Unit tests for decimal_places ####
