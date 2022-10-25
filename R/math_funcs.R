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

## Returns the area of a regular hexagon
#' @name area_hex
#' @title Calculates the area of a hexagon given either the longer radius \eqn{R} or the apothem \eqn{r}
#' @export
#' @description from \url{https://stackoverflow.com/a/59022366}
#' @param r The apothem, or shorter radius of a hexagon, from center to side
#' @param R The longer, or maximal, radius of a hexagon, from center to vertex Also the length of one of the sides.
#' @return A numeric vector of areas, in the same units as \code{r} and/or \code{R}
#' @examples
#' \dontrun{
#'  r <- 1.74 / 2
#'  area_hex(r = r)
#'  R <- 2 / 2
#'  area_hex(R = R)
#'  area_hex(R = R, r = r)
#' }
area_hex <- function(R = waiver(), r = waiver()) {
  if(is.waive(R) & is.waive(r))
    # no parameters supplied
    stop("At least one of the parameters R or r must be supplied.")
  if(!is.waive(R))
    if(!is.numeric(R))
      # R supplied but not numeric
      stop("R must be numeric.")
  if(!is.waive(r))
    if(!is.numeric(r))
      # r supplied but not numeric
      stop("r must be numeric.")
  # At least R or r exists and is numeric
  if(is.waive(R)) {
    # if R is missing, calculate it from r
    R <- 2 * r / sqrt(3)
  } else {
    if(is.waive(r)) {
      # r must be missing, so calculate it from R
      r <- sqrt(3) * R / 2
    }
  }
  # r and R both exist and are numeric; check r and R describe a hexagon.
  # Warn if not and calculate area anyway
  r_deci <- decimal_places(r)
  if(!all(round(r, r_deci-1) %==% round(sqrt(3) * R / 2, r_deci-1)))
    warning("Some (r, R) pairs don't look like they describe a hexagon.")

  # Calculate the area
  area <- 3 * R * r
  # Other variants of area calculation
  # R equation
  # area <- 3 * sqrt(3) * R^2 / 2
  # r equation
  # area <- 2 * sqrt(3) * r^2

  return(area)
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
        if("data.frame" %nin% class(lookup) ||
           any(c("expon", "word") %nin% colnames(lookup)) ||
           ncol(lookup) != 2) {
          invisible(NULL)
          stop("Lookup must be a data frame with columns 'expon' and 'word'.")
        }
        if (!any(c("integer", "numeric") %in% class(lookup[["expon"]]))) {
          invisible(NULL)
          stop("Lookup$expon must be either class numeric or integer.")
        }
        if ("factor" %in% class(lookup[["word"]]))
          lookup[["word"]] <- as.character(lookup[["word"]])
        if ("character" %nin% class(lookup[["word"]])) {
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
