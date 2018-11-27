## Extract mantissa and exponent from vector ####
#' @name exponent
#' @title Exponent of a number in scientific notation
#' @export
#' @description Returns the exponent of a number as it is written in scientific
#'   notation (powers of 10).
#' @references Thanks to Stackoverflow answer by Paul McMurdie \url{https://stackoverflow.com/a/25555105}
#' @param x (required) numeric. A number.
#' @return the exponent of the scientific notation representation of the number \code{x}
#' @examples
#'  x <- c(1,400,500000,.0003)
#'  exponent(x)
exponent <- function(x) {
  # check missing
  if(!missing(x)) {
    if(is.numeric(x)) {
      expon <- floor(log10(abs(x)))
      return(expon)
    } else {
      if(is.complex(x)) {
        invisible(NaN)
        #warning("x may not be a complex number.")
        stop("x may not be a complex number.")
      } else {
        invisible(NaN)
        stop("x must be numeric.")
      }
    }
  } else {
    invisible(NaN)
    stop("The numeric vector x must be supplied. e.g. exponent(x = 5753).")
  }
}

#' @name mantissa
#' @title Mantissa of a number in scientific notation
#' @export
#' @description Returns the mantissa of a number as it is written in scientific
#'   notation (powers of 10).
#' @param x (required) numeric. A number.
#' @return the mantissa of the scientific notation representation of the number \code{x}
#' @examples
#'  x <- c(1,400,500000,.0003)
#'  mantissa(x)
mantissa <- function(x) {
  if(!missing(x)) {
    if(is.numeric(x)) {
      mant <- log10(abs(x))
      mant <- 10^(mant - floor(mant))
      return (mant)
    } else {
      if(is.complex(x)) {
        invisible(NaN)
        stop("x may not be a complex number.")
      } else {
        invisible(NaN)
        stop("x must be numeric.")
      }
    }
  } else {
    invisible(NaN)
    stop("The numeric vector x must be supplied. e.g. mantissa(x = 5753).")
  }
}

