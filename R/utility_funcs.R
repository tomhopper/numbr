# @name nin
# @title Not in operator
# @description Checks if x is in y
# @param x a value to check
# @param y a value or vector of values to check against x
# @return Boolean value, true if x is not in y; false otherwise
# Removed rdoxygen2 comments in 0.8.0 to prevent documentation of
# this *internal* function.
"%nin%" <- function(x, y) {!(x %in% y)}

# @name waiver
# @title Used in place of null or empty values to function parameters
# @return An empty data structure of class waiver
waiver <- function() structure(list(), class = "waiver")

# @name is.waive
# @title Checks if a parameter contains the default value, waiver()
# @param x A parameter to the function
# @return True if
is.waive <- function(x) inherits(x, "waiver")

#' @title Test if Two Numeric Vectors are (Nearly) Equal Row-by-Row
#' @export
#' @param x A numeric or integer vector. May be of length 1.
#' @param y A numeric or integer vector to compare to x. May be of length 1.
#' @return A logical vector of TRUE and FALSE values indicating which rows
#'   are (nearly) equal.
#' @description Implements \code{isTRUE(all.equal())} on a row-by-row basis.
#'   x and y can be of different lengths, including \code{length() == 1}.
#'   Handy for making comparisons between data frame columns
#'   within \code{\link[dplyr]{filter}} and similar functions.
#' @seealso \link{all.equal}
#' @examples
#' \dontrun{
#'  x <- 1L
#'  y <- 1.001
#'  x == y
#'  # FALSE
#'  x %==% y
#'  # FALSE
#'  y <- 1.00000001
#'  x == y
#'  # FALSE
#'  x %==% y
#'  # TRUE
#'  x <- c(1, 2)
#'  x == y
#'  # FALSE FALSE
#'  x %==% y
#'  # TRUE FALSE
#' }
"%==%" <- function(x, y) {
  if (!missing(x) && !missing(y)){
    if (is.null(dim(x)) && is.null(dim(y)) &&
        ("numeric" %in% class(x) || "integer" %in% class(x)) &&
        ("numeric" %in% class(y) || "integer" %in% class(y))) {
      if (length(x) > length(y)) {
        y = rep(y, length.out = length(x))
        warning("lhs is longer than rhs; rhs will be recycled to length(lhs).")
      }
      else {
        if(length(y) > length(x)) {
          x = rep(x, length.out = length(y))
          warning("rhs is longer than lhs; lhs will be recycled to length(rhs).")
        }
      }
      z = rep(NA, times = length(x))
      for (i in 1:max(length(x), length(y))) {
        z[i] <- isTRUE(all.equal(x[i], y[i]))
      }
    } else {
      invisible(NULL)
      stop("lhs and rhs must be numeric or integer vectors of the same length")
    }
  } else {
    invisible(NULL)
    stop("Values must be supplied on both the left-hand side and right-hand side. e.g. \'lhs %==% rhs\'.")
  }
  return(z)
}

