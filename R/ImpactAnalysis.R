#' Impact analysis base class builder
#'
#' Builds a base class to represent impact analysis functionality for
#' calculating or classifying, and combining, spatially-explicit impacts of
#' invasive species incursions across various aspects of the environment,
#' society, and/or economy.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of the impact analysis.
#' @param region A \code{Region} or inherited class object representing the
#'   spatial region (template) for the impact analysis.
#' @param incursion An \code{Incursion} or inherited class object representing
#'   the spatial distribution of an actual invasive species presence or
#'   density, or incursion likelihoods across the \code{region}.
#' @param impact_layers A named list of spatial layers
#'   (\code{terra::SpatRaster} or \code{raster::RasterLayer}) or vectors of
#'   location values (consistent with the spatial \code{region}), for each
#'   named aspect (mechanism, service, sector, asset type, etc.) specified via
#'   the impact scope in the \code{context}. Unknown layers/values for aspects
#'   may be omitted.
#' @param combine_function The function used to combine impact layers across
#'   aspects of the environment, society, and/or economy. Either \code{"sum"},
#'   \code{"max"} (maximum), or a user defined function having the form
#'   \code{"function(aspect_locations)"}, where \code{aspect_locations} is
#'   a list of vectors of values at each \code{region} location for each
#'   \code{context} impact scope aspect, which is passed to the function.
#'   The function should return a single vector of values for each location.
#' @param ... Additional parameters.
#' @return An \code{ImpactAnalysis} class object (list) containing functions
#'   for calculating invasive species (likely) incursion impacts:
#'   \describe{
#'     \item{\code{incursion_impacts()}}{Calculate (likely) incursion impacts
#'       for each aspect of the environment, society, and/or economy.}
#'     \item{\code{combined_impacts()}}{Combine (likely) incursion impacts
#'       across aspects of the environment, society, and/or economy, to produce
#'       an overall impact.}
#'   }
#' @include Context.R
#' @include Region.R
#' @include Incursion.R
#' @export
ImpactAnalysis <- function(context,
                           region,
                           incursion,
                           impact_layers,
                           combine_function = c("sum", "max"),
                           class = character(), ...) {
  UseMethod("ImpactAnalysis")
}

#' @name ImpactAnalysis
#' @export
ImpactAnalysis.Context <- function(context,
                                   region,
                                   incursion,
                                   impact_layers,
                                   combine_function = c("sum", "max"),
                                   class = character(), ...) {

  # Check region and incursion model objects
  if (!inherits(region, "Region")) {
    stop("Region model must be a 'Region' or inherited class object.",
         call. = FALSE)
  }
  if (!inherits(incursion, "Incursion")) {
    stop("Incursion model must be a 'Incursion' or inherited class object.",
         call. = FALSE)
  }

  # Check impact layers
  if (!is.list(impact_layers) ||
      !all(names(impact_layers) %in% context$get_impact_scope()) ||
      any(!sapply(impact_layers,
                  function(x) (class(x) %in% c("SpatRaster", "RasterLayer") ||
                               is.numeric(x)))) ||
      (all(sapply(impact_layers, class) %in% c("SpatRaster", "RasterLayer"))
       && (region$get_type() != "grid" ||
           !all(sapply(impact_layers,
                       function(x) region$is_compatible(x))))) ||
      (any(sapply(impact_layers, is.numeric)) &&
       !all(sapply(impact_layers, length) == region$get_locations()))) {
    stop(paste("Impact layers must be a named list of spatial layers or",
               "location vectors, named consistently with the context impact",
               "scope, and which are compatible with the defined region."),
         call. = FALSE)
  }

  # Check combine function
  if (is.character(combine_function)) {
    combine_function <- match.arg(combine_function)
  } else if (!is.function(combine_function)) {
    stop(paste("Combine function must be 'sum', 'max', or a user-defined",
               "function."), call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = c(class, "ImpactAnalysis"))

  # Calculate (likely) incursion impacts for each aspect
  self$incursion_impacts <- function() {
    # overridden in inherited classes
  }

  # Combine (likely) impacts across aspects to produce an overall impact
  self$combined_impacts <- function() {
    # overridden in inherited classes
  }

  return(self)
}
