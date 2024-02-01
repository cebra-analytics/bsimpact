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
#'   Set to \code{"none"} when combining impacts is not applicable.
#' @param mgmt_costs Optional spatial layer (\code{terra::SpatRaster} or
#'   \code{raster::RasterLayer}) or vector of management costs at each
#'   location specified by the \code{region}, measured in the unit specified
#'   in the \code{context}. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return An \code{ImpactAnalysis} class object (list) containing functions
#'   for calculating invasive species (likely) incursion impacts and management
#'   costs (optional):
#'   \describe{
#'     \item{\code{get_incursion()}}{Get incursion object.}
#'     \item{\code{incursion_impacts()}}{Calculate (likely) incursion impacts
#'       for each aspect of the environment, society, and/or economy.}
#'     \item{\code{combined_impacts()}}{Combine (likely) incursion impacts
#'       across aspects of the environment, society, and/or economy, to produce
#'       an overall impact at each location.}
#'     \item{\code{incursion_mgmt_costs()}}{Calculate (likely) incursion
#'       management costs at each location (when specified).}
#'     \item{\code{save_analysis(...)}}{Save the impact analysis as a
#'       collection of raster TIF and/or comma-separated value (CSV) files,
#'       appropriate for the \code{region} type, including the individual and
#'       combined incursion impacts, and incursion management costs (when
#'       specified).\code{Terra} raster write options may be passed to the
#'       function for saving grid-based analysis outputs.}
#'   }
#' @include Context.R
#' @include Region.R
#' @include Incursion.R
#' @export
ImpactAnalysis <- function(context,
                           region,
                           incursion,
                           impact_layers,
                           combine_function = c("sum", "max", "none"),
                           mgmt_costs = NULL,
                           subclass = character(), ...) {
  UseMethod("ImpactAnalysis")
}

#' @name ImpactAnalysis
#' @export
ImpactAnalysis.Context <- function(context,
                                   region,
                                   incursion,
                                   impact_layers,
                                   combine_function = c("sum", "max", "none"),
                                   mgmt_costs = NULL,
                                   subclass = character(), ...) {

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
  } else if (!is.function(combine_function) ||
             length(formalArgs(combine_function)) != 1) {
    stop(paste("Combine function must be 'sum', 'max', 'none', or a",
               "user-defined with form function(aspect_locations)."),
         call. = FALSE)
  }

  # Check mgmt_costs
  if (!is.null(mgmt_costs) &&
      (!(class(mgmt_costs) %in% c("SpatRaster", "RasterLayer") ||
         is.numeric(mgmt_costs)) ||
       (class(mgmt_costs) %in% c("SpatRaster", "RasterLayer") &&
         !region$is_compatible(mgmt_costs)) ||
       (is.numeric(mgmt_costs) &&
        length(mgmt_costs) != region$get_locations()))) {
    stop(paste("Management costs must be a spatial layer or location vector",
               "compatible with the defined region."),
         call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = c(subclass, "ImpactAnalysis"))

  # Get incursion object
  self$get_incursion <- function() {
    return(incursion)
  }

  # Calculate (likely) incursion impacts for each aspect
  self$incursion_impacts <- function() {
    # overridden in inherited classes
  }

  # Combine (likely) impacts across aspects to produce an overall impact
  if (!is.character(combine_function) || combine_function != "none") {
    self$combined_impacts <- function() {
      # overridden in inherited classes
    }
  }

  # Calculate (likely) incursion management costs (when specified)
  if (!is.null(mgmt_costs)) {
    self$incursion_mgmt_costs <- function() {

      # Extract spatial raster management cost values
      if (class(mgmt_costs) %in% c("SpatRaster", "RasterLayer")) {
        mgmt_costs <- mgmt_costs[region$get_indices()][,1]
      }

      # Multiply by impact incursion values at impact locations
      if (region$get_type() == "grid") {
        incursion_mgmt_costs <-
          region$get_rast(mgmt_costs*incursion$get_impact_incursion())
        impact_locations <- region$get_template()
      } else {
        incursion_mgmt_costs <-  mgmt_costs*incursion$get_impact_incursion()
        impact_locations <- rep(FALSE, region$get_locations())
      }
      for (impact_layer in impact_layers) {
        impact_locations <- impact_locations | impact_layer > 0
      }
      incursion_mgmt_costs <- incursion_mgmt_costs*impact_locations

      return(incursion_mgmt_costs)
    }
  }

  # Save the impact analysis as a collection of appropriate files
  self$save_analysis <- function(...) {
    # overridden in inherited classes
  }

  return(self)
}
