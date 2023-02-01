#' Value impacts class builder
#'
#' Builds a class to represent quantitative impact analysis functionality for
#' calculating and combining spatially-explicit value-based impacts of invasive
#' species incursions across various aspects of the environment, society,
#' and/or economy.
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
#'   the impact scope in the \code{context}.
#' @param loss_rates A vector of value loss rates for each named aspect
#'   (mechanism, service, sector, asset type, etc.) specified via the impact
#'   scope in the \code{context}.
#' @param mgmt_costs A spatial layer (\code{terra::SpatRaster} or
#'   \code{raster::RasterLayer}) or vector of management costs at each
#'   location (specified by the \code{region}). Only used when the
#'   \code{context} valuation type is \code{"monetary"}. Default is
#'   \code{NULL}.
#' @param combine_function The function used to combine impact layers across
#'   aspects of the environment, society, and/or economy. Either \code{"sum"}
#'   or a user defined function having the form
#'   \code{"function(aspect_locations)"}, where \code{aspect_locations} is
#'   a list of vectors of values at each \code{region} location for each
#'   \code{context} impact scope aspect, which is passed to the function.
#'   The function should return a single vector of values for each location.
#' @param ... Additional parameters.
#' @return An \code{ValueImpacts} class object (list) containing functions
#'   for calculating invasive species (likely) incursion impacts and management
#'   costs (\code{context} valuation \code{"monetary"} type only):
#'   \describe{
#'     \item{\code{incursion_impacts()}}{Calculate (likely) incursion impacts
#'       (damages or losses) for each aspect of the environment, society,
#'       and/or economy.}
#'     \item{\code{combined_impacts()}}{Combine (likely) incursion impacts
#'       across aspects of the environment, society, and/or economy, to produce
#'       an overall impact (damage or loss).}
#'     \item{\code{incursion_mgmt_costs()}}{Calculate (likely) incursion
#'       management costs (valuation \code{"monetary"} type only).}
#'     \item{\code{total_costs()}}{Calculate (likely) total incursion
#'       (damages/losses plus management) costs (valuation \code{"monetary"}
#'       type only).}
#'   }
#' @include ImpactAnalysis.R
#' @export
ValueImpacts <- function(context,
                         region,
                         incursion,
                         impact_layers,
                         loss_rates,
                         mgmt_costs = NULL,
                         combine_function = "sum", ...) {
  UseMethod("ValueImpacts")
}

#' @name ValueImpacts
#' @export
ValueImpacts.Context <- function(context,
                                 region,
                                 incursion,
                                 impact_layers,
                                 loss_rates,
                                 mgmt_costs = NULL,
                                 combine_function = "sum", ...) {

  # Build via base class (for checks)
  self <- ImpactAnalysis(context = context,
                         region = region,
                         incursion = incursion,
                         impact_layers = impact_layers,
                         combine_function = combine_function,
                         class = "ValueImpacts", ...)

  # Check context is consistent with quantitative impact analysis
  if (!context$get_valuation_type() %in% c("monetary", "non-monetary") ||
      length(context$get_impact_measures()) > 1 ||
      !is.character(context$get_impact_measures())) {
    stop(sprintf("Context is inappropriately configured for value-based",
                 "impact analysis with '%s' valuation or invalid measure(s).",
                 context$get_valuation_type()), call. = FALSE)
  }

  # Check loss rates
  if (length(loss_rates) == length(context$get_impact_scope()) &&
      is.null(names(loss_rates))) {
    names(loss_rates) <- context$get_impact_scope()
    warning(paste("Unnamed loss rates assumed to be in order consistent with",
                  "the context impact scope."), call. = FALSE)
  }
  if (!is.numeric(loss_rates) || any(loss_rates < 0) || any(loss_rates > 1) ||
      (!is.null(names(loss_rates)) &&
       !all(names(loss_rates) %in% context$get_impact_scope())) ||
      (is.null(names(loss_rates)) &&
       length(loss_rates) != length(context$get_impact_scope()))) {
    stop(paste("Loss rates must be numeric, >= 0, <= 1, named consistently",
               "with the context impact scope."), call. = FALSE)
  }

  # Check mgmt_costs
  if (!is.null(mgmt_costs) &&
      (!(class(mgmt_costs) %in% c("SpatRaster", "RasterLayer") ||
         is.numeric(mgmt_costs)) ||
       (class(mgmt_costs) %in% c("SpatRaster", "RasterLayer") &&
        region$is_compatible(mgmt_costs)) ||
       (is.numeric(mgmt_costs) &&
        length(mgmt_costs) != region$get_locations()))) {
    stop(paste("Management costs must be a spatial layer or location vector",
               "compatible with the defined region."),
         call. = FALSE)
  }

  # Calculate (likely) incursion impacts for each aspect
  self$incursion_impacts <- function() { # overridden

    # TODO ####
    # Get impact incursion values (quantitative) or mask (class)
    if (context$get_valuation_type() %in% c("monetary", "non-monetary")) {
      impact_incursion <- incursion$get_impact_incursion()
    } else if (context$get_valuation_type() %in% c("ranking", "categorical")) {
      impact_incursion <- 1*(incursion$get_impact_incursion() > 0)
    }

    # Extract spatial raster values
    for (i in 1:length(impact_layers)) {
      if (class(impact_layers[[i]]) %in% c("SpatRaster", "RasterLayer")) {
        impact_layers[[i]] <- impact_layers[[i]][region$get_indices()][,1]
      }
    }

    #


    # Place in spatial raster when grid region
    if (region$get_type() == "grid") {
      impact_incursion_rast <- region$get_template()
      impact_incursion_rast[region$get_indices()] <- impact_incursion
      impact_incursion <- impact_incursion_rast
    }


    # overridden in inherited classes
    return(NULL)
  }

  # Combine (likely) impacts across aspects to produce an overall impact
  self$combined_impacts <- function() { # overridden
    # TODO ####
    # overridden in inherited classes
  }

  return(self)
}
