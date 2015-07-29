## Extract mantissa and exponent from vector
#' Exponent of a number in scientific notation
#' @param x (required) numeric. A number.
#' @return the exponent of the scientific notation representation of the number \code{x}
exponent <- function(x) {
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

#' Mantissa of a number in scientific notation
#' @param x (required) numeric. A number.
#' @return the mantissa of the scientific notation representation of the number \code{x}
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
