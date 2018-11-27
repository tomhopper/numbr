#' @title PRESS statistic
#' @description Calculates the predicted residual sum of squares, or PRESS, statistic.
#' 		Used primarily to compare alternative models.
#' @param linear.model An \code{lm} or \code{glm} object
#' @return The PRESS statistic
#' @importFrom stats residuals lm.influence

PRESS <- function(linear.model) {
  # calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  # calculate the PRESS
  PRESS <- sum(pr^2)

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
    if(!("lm" %in% class(dots[[i]]))) {
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
      return.df[[i, "r.sqr"]] <- ifelse("glm" %in% class(dots[[i]]), NA, summary_dot[["r.squared"]])
      return.df[[i, "adj.r.sqr"]] <- ifelse("glm" %in% class(dots[[i]]), NA, summary(dots[[i]])["adj.r.squared"])
      return.df[[i, "pre.r.sqr"]] <- ifelse("glm" %in% class(dots[[i]]), NA, pred_r_squared(dots[[i]]))
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