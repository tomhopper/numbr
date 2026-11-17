# trig_funcs

#' @name cot
#' @title Returns the cotangent, the complementary tangent, for angle \eqn{\theta\} on the unit circle
#' @param theta numeric or complex vector of angles.
#' @return a vector of length \code{length(m)} and class \code{class(m)}
cot <- function(theta = waiver()) {
  if(is.waive(theta)) stop("Argument theta is required.")
  if(is.numeric(theta) | is.complex(theta)) {
    return(1/tan(theta))
  } else {
    stop("Argument theta must be numeric or complex.")
  }
}

#' @name sec
#' @title Returns the secant corresponding to angle \eqn{\theta\} on the unit circle
#' @param theta numeric or complex vector of angles.
#' @return a vector of length \code{length(m)} and class \code{class(m)}
sec <- function(theta = waiver()) {
  if(is.waive(theta)) stop("Argument theta is required.")
  if(is.numeric(theta) | is.complex(theta)) {
    return(1/cos(theta))
  } else {
    stop("Argument theta must be numeric or complex.")
  }
}

#' @name csc
#' @title Returns the cosecant corresponding to angle \eqn{\theta\} on the unit circle
#' @param theta numeric or complex vector of angles.
#' @return a vector of length \code{length(m)} and class \code{class(m)}
csc <- function(theta = waiver()) {
  if(is.waive(theta)) stop("Argument theta is required.")
  if(is.numeric(theta) | is.complex(theta)) {
    return(1/sin(theta))
  } else {
    stop("Argument theta must be numeric or complex.")
  }
}

#' @name perp_slope
#' @title Returns the slope of lines perpendicular to the lines with given slope \code{m}.
#' @param m a numeric or complex vector of line slopes
#' @return a vector of length \code{length(m)} and class \code{class(m)}
#' @description
#' Returns the negative reciprocal of the given number.
#'
perp_slope <- function(m = waiver()) {
  if(is.waive(m)) stop("Argument m is required.")
  if(is.numeric(m) | is.complex(m)) {
    return(-1/m)
  } else {
    stop("Argument m must be numeric or complex.")
  }
}

#' @name dependent_y
#' @title Given a point on a line and the line's slope, returns a function that calculates the y value of another point on the line given x.
#' @param x numeric or complex vector. X values for points at which y values will be calculated
#' @param x1 description
#' @param y1 description
#' @param m description
#' @return A function that accepts a value x and returns the corresponding y to the point on the line defined by x1, y1, and m.
dependent_y <- function(x1 = waiver(), y1 = waiver(), m = waiver()) {
  return(function(x) {m*(x) - m*x1 + y1})
}

#' @name dependent_x
#' @title Given a point on a line and the line's slope, returns a function that calculates the x value of another point given y.
#' @param y description
#' @param x1 description
#' @param y1 description
#' @param m description
#' @return description
#' @examples
#' # example code
#'
dependent_x <- function(x1 = waiver(), y1 = waiver(), m = waiver()) {
  # Check that
  if(is.waive(x1) | is.waive(y1) | is.waive(m)) stop("Arguments x1, y1, and m are required.")
  # Check that arguments are of the right type
  #if(
  return(function(y = waiver()) {
    if(is.waive(y)) stop("Argument y is required.")
    return((y - y1)/m + x1)
  })
}
