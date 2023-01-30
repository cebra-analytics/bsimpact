#' Incursion class builder
#'
#' Builds a class to represent a spatially-explicit incursion (layer) of actual
#' invasive species presence or density, or incursion likelihoods across a
#' defined region for impact analysis, and transforms the incursion values for
#' impact calculations.
#'
#' @param x A \code{terra::SpatRaster}, \code{raster::RasterLayer}, or numeric
#'   vector defining the incursion presence, density, or probabilities at each
#'   spatial location.
#' @param region A \code{Region} or inherited class object representing the
#'   spatial region (template) for impact analysis.
#' @param type One of \code{"presence"}, \code{"density"}, or \code{"prob"} to
#'   indicate if the values in \code{x} represent incursion presence, density,
#'   or probabilities respectively.
#' @param multiplier Numeric multiplier to transform/scale incursion values for
#'   calculating impacts (especially when type is \code{"density"}). Default is
#'   \code{1}.
#' @param threshold Numeric threshold (>= value) for signifying sufficient
#'   incursion density or probability to contribute to impact calculations
#'   (after the application of the \code{multiplier}). Default is \code{0}.
#' @param ... Additional parameters.
#' @return An \code{Incursions} class object (list) containing functions for
#'   accessing incursion type and transformed incursion values:
#'   \describe{
#'     \item{\code{get_type()}}{Get the incursion type.}
#'     \item{\code{get_impact_incursion()}}{Get the transformed incursion (0-1)
#'       values for calculating impacts.}
#'   }
#' @export
Incursion <- function(x,
                      region = NULL,
                      type = c("presence", "density", "prob"),
                      multiplier = 1,
                      threshold = 0, ...) {
  UseMethod("Incursion")
}

#' @name Incursion
#' @export
Incursion.Raster <- function(x, ...) {
  # Call the terra version of the function
  Incursion(terra::rast(x), ...)
}

#' @name Incursion
#' @export
Incursion.SpatRaster <- function(x,
                                 region = NULL, ...) {

  # Resolve values and call default (vector) version
  if (!is.null(region)) {

    # Check region
    if (!inherits(region, "Region")) {
      stop("Region model must be a 'Region' or inherited class object.",
           call. = FALSE)
    }
    if (!region$is_compatible(x)) {
      stop("The spatial object x should be compatible with that defining the ",
           "region.", call. = FALSE)
    }

    # Extract values from locations defined by region
    Incursion(as.matrix(x[region$get_indices()]), region = region, ...)

  } else { # Use all values
    Incursion(as.matrix(x[]), region = region, ...)
  }
}

#' @name Incursion
#' @export
Incursion.default <- function(x,
                              region = NULL,
                              type = c("presence", "density", "prob"),
                              multiplier = 1,
                              threshold = 0, ...) {

  # Check region and x
  if (!is.null(region)) {
    if (!inherits(region, "Region")) {
      stop("Region model must be a 'Region' or inherited class object.",
           call. = FALSE)
    }
    if (length(x) != region$get_locations()) {
      stop("The length of x must be equal to the number of region locations.",
           call. = FALSE)
    }
  }
  if (min(x) < 0) {
    stop("The values of x must be >= 0.", call. = FALSE)
  }

  # Check multiplier and threshold
  if (!is.numeric(multiplier) || multiplier <= 0 ) {
    stop("The multiplier must be numeric and > 0.", call. = FALSE)
  }
  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    stop("The threshold must be numeric, >= 0, and <= 1.", call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = "Incursion")

  # Get type
  type <- match.arg(type)
  self$get_type <- function() {
    return(type)
  }

  # Ensure x values are consistent with type
  if (type == "prob" && max(x) > 1) {
    stop("The probability values of x must be <= 1.", call. = FALSE)
  }

  # Get transformed incursion values for calculating impacts
  self$get_impact_incursion <- function() {

    # Apply multiplier then threshold
    x <- as.numeric(x)*multiplier
    x <- x*(x > threshold)

    # Truncate to 1
    x[which(x > 1)] <- 1

    return(x)
  }

  return(self)
}
