#' @name nin
#' @title Not in operator
#' @description Checks if x is in y
#' @param x a value to check
#' @param y a value or vector of values to check against x
#' @return Boolean value, true if x is not in y; false otherwise
"%nin%" <- function(x, y) {!(x %in% y)}
