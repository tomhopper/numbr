#' @name decimal_places
#' @title Counts the number of digits to the right of the decimal
#' @export
#' @description Counts the number of digits to the right of the decimal place, ignoring any trailing zeroes.
#' @param x A numeric vector
#' @return An integer vector containing the number of digits to the right of the decimal for each
#'   element of \code{x}
#' @examples
#' \dontrun{
#'  decimal_places(c(0.11, 23.4, 185.8987, 0.1100))
#' }
decimal_places <- function(x) {
  ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
         nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
         0)
}


## Convert numbers to words ####
#' @name num_order_to_word
#' @title Convert a vector of numbers to large-number word representation
#' @export
#' @description Converts a vector of numbers to a character string approximation
#'   using the "short scale" version of large number names. e.g. 312e6 returns
#'   as '300 million.' Simultaneously returns a numeric representation of the
#'   approximation.
#' @param x A vector of numbers to convert.
#' @param lookup Optional. A data frame specifying numeric exponents as `expon` and corresponding names as `word`. e.g. lookup = data.frame(expon = c(3, 0, -3), word = c("thousands", "", "thousandths"))
#' @param nsmall Optional. An integer number of digits to include to the right of the the leading digit
#' @return A data frame containing the originally-supplied vector, the short scale version, and its string representation
#' @importFrom tibble tibble
#' @examples
#' # Simplest example
#' num_order_to_word(1340)
#' # Using a variable
#' x <- exp(1)^20
#' num_order_to_word(x)
#' num_order_to_word(x, nsmall = 1)
#' x <- exp(1)^-10
#' num_order_to_word(x)
#' \dontrun{
#' x <- 1:4
#' num_order_to_word(x)
#' }
num_order_to_word <- function(x, lookup = NULL, nsmall = 0) {
  # x must be a numeric or integer vector
  if(!missing(x)) {
    if(is.numeric(x) || is.integer(x)) {
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
                                      "trillionth"))
      } else {
        # User has supplied the lookup table
        # Check that lookup has the right format:
        # data frame
        # columns expon and word, and nothing else
        # column expon is numeric and word is character (if factor, then convert)
        if(!inherits(lookup, "data.frame") ||
           any(c("expon", "word") %nin% colnames(lookup)) ||
           ncol(lookup) != 2) {
          invisible(NULL)
          stop("Lookup must be a data frame with columns 'expon' and 'word'.")
        }
        if (!inherits(lookup[["expon"]], "integer") && !inherits(lookup[["expon"]], "numeric")) {
          invisible(NULL)
          stop("Lookup$expon must be either class numeric or integer.")
        }
        if (inherits(lookup[["word"]], "factor"))
          lookup[["word"]] <- as.character(lookup[["word"]])
        if (!inherits(lookup[["word"]], "character")) {
          invisible(NULL)
          stop("Lookup$word must be class character.")
        }
      }
      # Get the exponent and round to the nearest multiple of 3 so
      # we can look up values in our lookup table and return the number
      # with the correct digits.
      x_exp <- exponent(x)
      x_exp <- ifelse(x_exp != 0, floor(x_exp / 3.0) * 3, 0)
      # Look up the word for the number
      x_name <- rep(NA, times = length(x_exp))
      for(i in 1:length(x_exp)) {
        if(is.na(x_exp[i]) | is.infinite(abs(x_exp[i]))) {
          x_name[i] <- ""
        } else {
          x_name[i] <- ifelse(x_exp[i] != 0, lookup$word[lookup$expon == x_exp[i]], "")
        }
      }
      # Convert x to a number with -3 < exponent() < 3
      # then combine the number and x_name.
      # e.g. 200,000,000,000 should return
      # "200 billion"
      x_n <- rep(NA, times = length(x_exp))
      for(i in 1:length(x_exp)){
        if(is.na(x_exp[i])) {
          x_n[i] <- NA
        } else {
          if (is.infinite(x_exp[i])) {
            x_n[i] <- ifelse(x_exp[i] > 0, x[i], 0)
          } else {
            if(abs(x_exp[i]) >= 3) {
              x_n[i] <- x[i] / 10^x_exp[i]
              x_n[i] <- round(x_n[i] / 10^exponent(x_n[i]), nsmall) * 10^exponent(x_n[i])
            } else {
              if(is.na(x_exp[i])) {
                x_n[i] <- NA
              } else {
                x_n[i] <- round(x[i] / 10^(exponent(x[i])-1), nsmall) * 10^(exponent(x[i])-1)
              }
            }
          }
        }
      }
      # Create word equivalent of approximate number
      x_name <- paste0(as.character(x_n), ifelse(nchar(x_name > 0), " ", ""), x_name)
      x_name <- trimws(x_name)
      x_name <- ifelse(x_name == "NA", NA, x_name)
      x_n <- x_n * 10 ^ x_exp
    } else {
      stop(paste(deparse(substitute(x)), "is neither numeric nor integer. Please pass either a numeric or an integer variable to 'x'."))
    }
  } else {
    invisible(NULL)
    stop("x is missing; please supply a number to x.")
  }
  # Return as a list object with both the number and its string representation
  return(tibble(number = x_n, name = x_name))
}

#' @title Converts a decimal fraction to a natural frequency that is
#'   approximately the same.
#' @description
#'  Intended to create
#'  Calculates a natural frequency from a given decimal fraction and a list of
#'  denominators. e.g. if given \code{x = 0.71} and \code{denom = 10}, would return
#'  a numerator of 7 and the denominator 10 in a vector \code{c(7, 10)}.
#' @details
#'  For every given denominator in \code{denom}, frac_to_nfreq calculates every
#'  fraction \eqn{x * denom} and rounds to a whole number \code{num}.
#'
#'  It then calculates the true ratio \code{ratio = num / denom} and the absolute error,
#'  \code{abs_error}, between  \code{x} and each value of \code{ratio}.
#'
#'  If \code{return = 'best'}, the function returns a named vector containing only the
#'  numerator and denominator with the lowest absolute error. If there are ties in
#'  \code{abs_error}, frac_to_nfreq will return the natural ratio with the smallest
#'  denominator.
#'
#'  For example, if \code{x = 0.2} and \code{denom = 2:10}, then frac_to_nfreq will
#'  return 1 and 5 but not 2 and 10.
#'
#'  When \code{return = 'table'}, the function returns a data frame with one row for
#'  every given denominator, and columns for the denominators, the corresponding
#'  numerator, the actual ratio, and the absolute error.
#'
#'  Having an unstable return type can clearly lead to errors, but is done here
#'  because an earlier version of the function did not have the \code{return}
#'  parameter, and functioned only as \code{return = 'best'}. The function was
#'  rewritten to provide the full table of results in a way that does not break
#'  legacy code.
#' @param x numeric. The decimal fraction to convert. If \code{length(x)} > 1, the
#'   function will return an error.
#' @param denom numeric. A vector of integers to check for the denominator
#'   of the natural ratio. Defaults to \code{2:10}.
#' @param return character. If "best", \code{frac_to_nfreq} returns a vector with
#'   only the best match of numerator and denominator If "table,"
#'   returns a data frame with results for every given denominator. Defaults to "best."
#' @param crit_error numeric. Defines the maximum absolute error for which to print a
#'   warning when \code{return = 'best'}.
#' @return if \code{return = 'best'}, returns a named numeric vector containing
#'   the numerator and denominator of the closest natural fraction. If
#'   \code{return = "table"}, returns a data frame containing a row for
#'   each denominator given in \code{denom}, and the corresponding numerator,
#'   the calculated ratio of \eqn{numerator / denominator}, and the absolute
#'   error difference of the ratio from \code{x}.
#' @TODO Make this works when x is a vector
#' @TODO Make sure "natural ratio" is the right term of art
#' @TODO Make it work with x >= 1
#' @DONE Add a way to warn the user when abs_error > crit_error
#' @DONE Add a hidden argument to let the user adjust crit_error
frac_to_nfreq <- function(x, denom = 2:10, return = "best", crit_error = 0.1) {
  if(missing(x)) stop("Argument x is missing. Please provide a fraction.")
  if(!is.numeric(x) | !is.numeric(denom) | !is.numeric(crit_error))
    stop("Arguments x, denom, and crit_error must be numeric.")
  if(length(x) > 1) stop("Argument x must have a singular value (length(x) == 1).")
  if(crit_error <=0 | crit_error >= 1) stop("crit_error must be between 0 and 1.")
  if(x <= 0 | x >= 1) stop("Argument x must be between 0 and 1.")
  # Create table of denominators and numerators
  table_df <- data.frame(denom = as.integer(denom),
                         num = as.integer(round(x*denom, digits = 0)))
  # Calculate the actual ratio of numerator / denominator
  table_df['ratio'] <- table_df['num'] / table_df['denom']
  # Determine the absolute error between x and each value of ratio
  table_df['abs_error'] <- abs(x - table_df['ratio'])

  if(return == "table") {
    return(table_df)
  } else {
    if(return == "best"){
      # find the row with the smallest absolute error
      result_df <- table_df[which(table_df['abs_error'] == min(table_df['abs_error'])),]
      if(nrow(result_df) > 1) {
        # Select the row with the smallest denom
        result_df <- result_df[which(result_df['denom'] == min(result_df['denom'])),]
      }
      if(result_df['abs_error'] >= crit_error) warning(paste("Absolute error is greater than ", crit_error * 100, "%. Result may not be representative of x.", sep = ""))
      # Return the nearest numerator and denominator as a named vector
      return(c(numer = result_df[['num']], denom = result_df[['denom']]))
    } else {
      stop("Argument return must be either 'best' or 'table'.")
      return(0)
    }
  }
}
