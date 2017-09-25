## Extract mantissa and exponent from vector ####
#' @title Exponent of a number in scientific notation
#' @description Returns the exponent of a number as it is written in scientific
#'   notation (powers of 10).
#' @param x (required) numeric. A number.
#' @return the exponent of the scientific notation representation of the number \code{x}
#' @examples
#'  x <- c(1,400,500000,.0003)
#'  exponent(x)
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

#' @title Mantissa of a number in scientific notation
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

## Convert numbers to words ####
#' @title Convert a number to words
#' @description Converts a number to a character string approximation. e.g. 312e9
#'   returns as '300 billion.' Also returns a numeric representation of the approximation.
#' @param x A number to convert. length(x) must equal 1.
#' @param lookup A data frame specifying numeric exponents as `expon` and corresponding names as `word`. e.g. lookup = data.frame(expon = c(3, 0, -3), word = c("thousands", "", "thousandths"))
#' @return A list containing the rounded number and its string representation
#' @examples
#' # Simplest example
#' num_order_to_word(1340)
#' # Using a variable
#' x <- exp(1)^20
#' num_order_to_word(x)
#' x <- exp(1)^-10
#' num_order_to_word(x)
#' # Fails with an error message
#' \dontrun{
#' x <- 1:4
#' num_order_to_word(x)
#' }
num_order_to_word <- function(x, lookup = NULL) {
  # x must be a numeric or integer vector
  if(is.numeric(x) | is.integer(x)) {
    # this won't work with more than one element in x
    if(length(x) == 1L) {
      # set up our lookup table
      if(is.null(lookup)) {
        # User has not supplied the lookup table
        lookup <- data.frame(expon = c(21, 18, 15, 12, 9, 6, 3,
                                       0, -3, -6, -9, -12),
                             word = c("septillion", "sextillion", "quintillion",
                                      "trillion", "billion", "million", "thousand",
                                      "", "thousandth", "millionth", "billionth",
                                      "trillionth"),
                             stringsAsFactors = FALSE)
      } else {
        # User has supplied the lookup table
        # Check that lookup has the right format:
        # data frame
        # columns expon and word, and nothing else
        # column expon is numeric and word is character (if factor, then convert)
        if("data.frame" %nin% class(lookup) |
           any(c("expon", "word") %nin% colnames(lookup)) |
           ncol(lookup) != 2) {
          stop("Lookup must be a data frame with columns 'expon' and 'word'.")
        }
        if("numeric" %nin% class(lookup[,"expon"])) stop("Lookup$expon must be numeric.")
        if("factor" %in% class(lookup[,"word"])) lookup[,"word"] <- as.character(lookup[, "word"])
        if("character" %nin% class(lookup[,"word"])) stop("Lookup$word must be character.")
      }
      # Get the exponent and round to the nearest multiple of 3 so
      # we can look up values in our lookup table and return the number
      # with the correct digits.
      x_exp <- exponent(x)
      if(x_exp != 0) {
        x_exp <- floor(x_exp / 3.0) * 3
      } else {
        x_exp <- 0
        # if(x_exp <= -3) {
        #   x_exp <- ceiling(x_exp / 3.0) * 3
        # } else {
        #   x_exp <- 0
        # }
      }
      # Look up the word for the number
      if(x_exp != 0) {
        x_name <- lookup$word[lookup$expon == x_exp]
      } else {
        x_name <- ""
      }
      # Convert x to a number with -3 < exponent() < 3
      # then combine the number and x_name.
      # e.g. 200,000,000,000 should return
      # "200 billion"
      if(abs(x_exp) >= 3) {
        x_n <- x / 10^x_exp
        x_n <- round(x_n / 10^exponent(x_n), 0) * 10^exponent(x_n)
      } else {
        x_n <- round(x / 10^(exponent(x)-1), 0) * 10^(exponent(x)-1)
      }
      # Create word equivalent of approximate number
      x_name <- paste0(as.character(x_n), ifelse(nchar(x_name > 0), " ", ""), x_name)
      x_n <- x_n * 10 ^ x_exp
    } else {
      stop(paste(deparse(substitute(x)), "has", ifelse(length(x) > 1L, "too many", "no"), "elements. Please pass a numeric variable with length(x) == 1 to 'x'."))
    }
  } else {
    stop(paste(deparse(substitute(x)), "is neither numeric nor integer. Please pass either a numeric or an integer variable to 'x'."))
  }
  # Return as a list object with both the number and its string representation
  return(list(number = x_n, name = x_name))
}
