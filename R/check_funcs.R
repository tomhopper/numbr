#' is.int
#' @description Determines if a vector fits the IEEE-754 definition of an integer that can be exactly represented with a double-precision number.
#' @param v the vector to check
#' @return Returns TRUE if vector v is an IEEE-754-compliant integer, otherwise FALSE.
#' @source \url{http://www.win-vector.com/blog/2015/06/r-in-a-64-bit-world/}
is.int <- function(v) {
  is.numeric(v) &
    v > -2^53 & v < 2^53 &
    (floor(v)==ceiling(v))
}
