#' @name mean_geom
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
#'   any negative numbers.
#'   For speed, \code{mean_geom} computes the log of each term, sums the logs, then computes the
#'   exponent of the sum.
#' @return The geometric mean of the vector \code{x}, or \code{NULL} with a warning
#'   if the geometric mean cannot be calculated
#' @importFrom stats na.omit
# "mean_geom" chosen so that it shows up with "mean" in autocomplete.
mean_geom = function(x, na.rm=TRUE){
  if (!missing(x)) {
    if (inherits(x, "numeric") || inherits(x, "integer")) {
      if (na.rm) x <- na.omit(x)
      return(exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x)))
    } else {
      invisible(NULL)
      stop("'x' must be a numeric vector of class integer or numeric")
    }
  } else {
    invisible(NULL)
    stop("'x' is missing. Please supply a numeric vector for calculation of the geometric mean.")
  }
}

#' @name mode_stat
#' @title Returns the mode of a vector.
#' @export
#' @param x a vector
#' @param na.rm a boolean indicating, if \code{TRUE}, removes \code{NA} values before calculating the mode
#' @param method specifies the method used for determining the mode
#' @return a vector containing the modes of \code{x}
#' @description Determines the most common value in a vector. Handles multiple modes by returning a vector
#'  containing all modes. Works with any data type.
#' @details \code{method} must be one of either "tabulate" or "density." "Tabulate"
#' @importFrom stats density
mode_stat <- function(x, na.rm = FALSE, method = "tabulate") {
  if(!missing(x)) {
    if(!(method == "tabulate" || method == "density")) stop("method must be either \'tabulate\' or \'numeric.\'"); invisible(NaN)
    if(inherits(x, "numeric") && method == "density") {
      temp <- density(x = x)
      return_val <- temp$x[which.max(temp$y)]
    } else {
      if(method == "tabulate"){
        if(inherits(x, "numeric")) warning("Looks like you want to find the mode for a vector of real numbers.\nYou might want to set method = 'density'.")
        if(na.rm){
          x <- x[!is.na(x)]
        }
        ux <- unique(x)
        tab <- tabulate(match(x, ux));
        return_val <- ux[which.max(tab)]
        #return_val <- ux[tab == max(tab)]
      } else {
        invisible(NaN)
        stop("Either the method supplied is not recognized, or method = density was attempted with a non-numeric vector.")
      }
    }
  } else {
    invisible(NaN)
    stop("x must be supplied.")
  }
  return(return_val)
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


#' @title PRESS statistic
#' @description Calculates the predicted residual sum of squares, or PRESS, statistic.
#' 		Used primarily to compare alternative models.
#' @param linear.model An \code{lm} or \code{glm} object
#' @return The PRESS statistic
#' @importFrom stats residuals lm.influence
PRESS <- function(linear.model) {
  if (!missing(linear.model)) {
    if (!inherits(linear.model, "lm")) {
      stop("Supply a linear model to parameter 'linear.model'")
      invisible(NaN)
    }
    # calculate the predictive residuals
    pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
    # calculate the PRESS
    PRESS <- sum(pr^2)
  } else {
    invisible(NULL)

  }

  return(PRESS)
}

#' @name pred_r_squared
#' @title Predicted R squared
#' @description Calculates the predicted r-squared statistic.
#' 		Used primarily to compare alternative models.
#' @param linear.model An \code{lm} or \code{glm} object
#' @return The predicted r squared value
#' @importFrom stats anova
pred_r_squared <- function(linear.model) {
  if(!inherits(linear.model, "lm")) {
    stop("Supply a linear model to parameter 'linear.model'")
    invisible(NaN)
  }
  # Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  # Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)

  return(pred.r.squared)
}

#' @name model_fit_stats
#' @title Model Fit Statistics
#' @description Returns lm and glm model fit statistics R-squared, adjusted R-squared,
#'      predicted R-squared, PRESS, AIC, and BIC.
#'      Thanks to John Mount for his 6-June-2014 blog post, R style tip: prefer functions that return data frames" for
#'      the idea \url{http://www.win-vector.com/blog/2014/06/r-style-tip-prefer-functions-that-return-data-frames}
#' @export
#' @param ... One or more \code{lm} or \code{glm} models, passed as bare (unquoted) names.
#' @return A data frame containing containing the R-squared, adjusted R-squared, Predictive R-squared, PRESS, AIC, and BIC statistics for each model, with a row for each supplied model.
#' @importFrom stats AIC BIC coef
#' @examples
#' \dontrun{
#' m1_lm <- lm(mpg ~ disp + hp + drat + wt + qsec, data = mtcars)
#' m2_lm <- lm(mpg ~ disp + wt, data = mtcars)
#' m3_lm <- lm(mpg ~ hp + drat + qsec, data = mtcars)
#' m1_glm <- glm(mpg ~ disp + wt + qsec, data = mtcars)
#'
#' model_fit_stats(m1_lm, m2_lm, m3_lm, m1_glm)
#' }
model_fit_stats <- function(...) {
  var_names <- as.character(match.call())[-1]
  dots <- list(...)
  ndots <- length(dots)

  # Check each item in list individually for class() == lm,
  # drop items that are not lm, and throw a warning. Finally,
  # drop the non-lm models from the input.
  nclass <- NULL
  for(i in 1:ndots) {
    if(!(inherits(dots[[i]], "lm"))) {
      nclass <- c(i, nclass)
    }
  }
  if(length(nclass) > 0) {
    warning(paste("Models", paste(var_names[nclass], collapse = ", "), "are not of class \'lm\' and will be excluded."))
    var_names <- var_names[-c(nclass)]
    dots <- dots[-c(nclass)]
    ndots <- ndots - length(nclass)
  }

  # If we have any lm, loop through the inputs and calculate fit statistics,
  # storing them in a data frame
  if(ndots > 0) {
    # create return.df
    return.df <- data.frame(model = var_names,
                            terms = NA,
                            r.sqr = NA,
                            adj.r.sqr = NA,
                            pre.r.sqr = NA,
                            PRESS = NA,
                            AIC = NA,
                            BIC = NA)
    # Populate return.df with model fit statistics
    for(i in 1:ndots) {
      return.df[[i, "terms"]] <- length(coef(dots[[i]])) - 1
      summary_dot <- summary(dots[[i]])
      return.df[[i, "r.sqr"]] <- ifelse(inherits(dots[[i]], "glm"), NA, summary_dot[["r.squared"]])
      return.df[[i, "adj.r.sqr"]] <- ifelse(inherits(dots[[i]], "glm"), NA, summary(dots[[i]])["adj.r.squared"])
      return.df[[i, "pre.r.sqr"]] <- ifelse(inherits(dots[[i]], "glm"), NA, pred_r_squared(dots[[i]]))
      return.df[[i, "PRESS"]] <- PRESS(dots[[i]])
      return.df[[i, "AIC"]] <- AIC(dots[[i]])
      return.df[[i, "BIC"]] <- BIC(dots[[i]])
    }
    #	Return the fit statistics data frame
    return(return.df)
  } else {
    # if ndots == 0, there were no models for which statistics could be calculated. Throw an error.
    stop("model_fit_stats needs objects of class 'lm.'")
  }
}

#' @name nCr
#' @title Calculate the number of possible combinations of \eqn{n} objects, taken \eqn{r} at a time without repitition
#' @description Calculates the number of arrangements in which no element occurs more than once and order does
#'   not matter, without the requirement of using all the elements from a given set. For example, if we were
#'   arranging an apple, a pear, and an orange (\eqn{n = 3}) into sets of two (\eqn{k = 2}), we would find
#'   that there are three possible combinations: an apple and a pear, an apple and an orange, and a pear
#'   and an orange. We would not count combinations where the order was reversed (e.g. a pear and an apple)
#'   as different combinations.
#' @details Implements the equation \deqn{ \frac{n!}{r!(n - r)!} }{n! / (r! * (n - r)!)}
#' @param n An integer number of elements to choose from for the combination. May be a vector if \code{r} has a single element (i.e. \code{length(r) == 1}).
#' @param r An integer subgroup size to combine. May be a vector if \code{n} has a single element (i.e. \code{length(n) == 1}).
#' @return An integer number of possible permutations
#' @export
#' @examples
#'   \dontrun{
#'     nCr(3, 2)
#'     nCr(3:5, 2)
#'     nCr(5, 2:4)
#'     nCr(1:5, 3)
#'   }
nCr <- function(n, r) {
  if(!(all(is.int(n)) & all(is.int(r)))) {stop("'n' and 'r' must both be integer numbers.")}
  len_n <- length(n)
  len_r <- length(r)
  if(len_n != 1 & len_r != 1) {stop("Either n or r must contain only one element.")}
  result <- factorial(n) / (factorial(r) * factorial(n - r))
  names(result) <- paste0("n", n, "r", r)
  return(result)
}

#' @name nPr
#' @title Calculate the number of possible partial permutations of \eqn{n} objects, \eqn{r} at a time
#' @description Calculates the number of ordered arrangements in which no element occurs more than once,
#'   without the requirement of using all the elements from a given set. Also known as \emph{partial permutations}
#'    or as \emph{sequences without repetition}.
#' @details Implements the equation \deqn{\frac{n!}{(n - r)!}}{n! / (n - r)!}
#' @param n An integer number of elements to permute. May be a vector if \code{r} has a single element (i.e. \code{length(r) == 1}).
#' @param r An integer subgroup size. May be a vector if \code{n} has a single element (i.e. \code{length(n) == 1}).
#' @return A named vector containing the integer number of possible permutations for each value of \code{n} supplied.
#' @export
#' @examples
#'   \dontrun{
#'     nPr(3, 2)
#'     nPr(3:5, 2)
#'     nPr(5, 2:4)
#'     nPr(1:5, 3)
#'   }
nPr <- function(n, r) {
  if(!(all(is.int(n)) & all(is.int(r)))) {stop("'n' and 'r' must both be integer numbers.")}
  len_n <- length(n)
  len_r <- length(r)
  if(len_n != 1 & len_r != 1) {stop("Either n or r must contain only one element.")}
  result <- factorial(n) / (factorial(n - r))
  names(result) <- paste0("n", n, "r", r)
  return(result)
}

