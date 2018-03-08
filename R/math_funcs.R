#' @title Returns the geometric mean of a vector
#' @export
#' @param x A numeric or integer vector
#' @param na.rm a boolean indicating, if \code{TRUE}, removes \code{NA} values before calculating the mean
#' @description Calculates the geometric mean of a vector, returning a single value. The geometric mean,
#'   like the mean and the median, is an indication of the central tendancy of a set of numbers.
#'   The geometric mean is  the \emph{n}th root of the product of \emph{n} numbers. For instance,
#'   the geometric mean of 2 and 8 is \code{sqrt(2 * 8) = 4}.
#'   The geometric mean is useful when computing the central tendency of measures that have different
#'   ranges. For instance, when computing a single "figure of merit" from differentrating scales that
#'   have ranges 0 to 5 and 0 to 100.
#' @details The geometric mean is only defined for positive numbers, and \code{mean_geom} first removes
#'   any negative numbers. The definition of the mean is
#'   \deqn{\left(\prod_{i=1}^n x_i\right)^\frac{1}{n} = \sqrt[n]{x_1 x_2 \cdots x_n}}
#'   For speed, \code{mean_geom} computes the log of each term, sums the logs, then computes the
#'   exponent of the sum.
#' @return The geometric mean of the vector \code{x}, or \code{NULL} with a warning
#'   if the geometric mean cannot be calculated
# "mean_geom" chosen so that it shows up with "mean" in autocomplete.
mean_geom = function(x, na.rm=TRUE){
  if(class(x) %in% c("numeric", "integer")) {
    return(exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x)))
  } else {
    warning("'x' must be a numeric vector of class integer or numeric")
    return(NULL)
  }
}

#' @title Returns the mode of a vector.
#' @export
#' @param x a vector to determine the mode of
#' @param na.rm a boolean indicating, if \code{TRUE}, removes \code{NA} values before calculating the mode
#' @return a vector containing the modes of \code{x}
#' @description Determines the most common value in a vector. Handles multiple modes by returning a vector
#'  containing all modes. Works with any data type.
mode_stat <- function(x, na.rm = FALSE) {
  if(na.rm){
    x <- x[!is.na(x)]
  }

  ux <- unique(x)
  tab <- tabulate(match(x, ux));
  return(ux[tab == max(tab)])
}

# for uni-modal, this is about 2.5% faster
# mode_uni <- function(x, na.rm = FALSE) {
#   if(na.rm){
#     x = x[!is.na(x)]
#   }
#
#   ux <- unique(x)
#   return(ux[which.max(tabulate(match(x, ux)))])
# }


## Convert numbers to words ####
#' @title Convert a number to large-number word representation
#' @export
#' @description Converts a number to a character string approximation using the "short scale"
#'   version of large number names. e.g. 312e6 returns as '300 million.'
#'   Simultaneously returns a numeric representation of the approximation.
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
        # User has not supplied the lookup table;
        # provide the short scale version (used in American English)
        lookup <- data.frame(expon = c(33, 30, 27, 24, 21, 18, 15, 12, 9, 6, 3,
                                       0, -3, -6, -9, -12),
                             word = c("decillion", "nonillian", "octillian",
                                      "septillion", "sextillion", "quintillion", "quadrillion",
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