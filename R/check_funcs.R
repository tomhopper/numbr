#' @name is.int
#' @export
#' @title Determins if a number is an IEEE-754 integer
#' @description Determines if a vector fits the IEEE-754 definition of an integer that can be exactly represented with a double-precision number.
#' @param x the vector to check
#' @param ... provides backward compatibility with previous versions.
#' @return Returns TRUE for each element of \code{x} that is an IEEE-754-compliant integer, and FALSE for all other elements.
#' @source \url{http://www.win-vector.com/blog/2015/06/r-in-a-64-bit-world/}
#' @examples
#'  x <- c(1, 412, 4.4, 0.003)
#'  is.int(x)
# "is.int" chosen so that it shows up in autocomplete with is.integer and other is.* functions
is.int <- function(x, ...) {
  if(missing(x)) {
    args <- list(...)
    if("v" %in% names(args)) {
      x <- args[["v"]]
    } else {
      stop("Argument 'x' is missing.")
    }
  }

  is_int <- is.numeric(x) &
    x > -2^53 & x < 2^53 &
    (floor(x)==ceiling(x))

  return(is_int)
}

