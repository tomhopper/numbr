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
#'  # calculate the area of a standard hex sticker
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

