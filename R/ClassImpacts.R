#' Classification impacts class builder
#'
#' Builds a class to represent classification impact analysis functionality for
#' ranking, categorising, and combining spatially-explicit class-based impacts
#' of invasive species incursions across various aspects of the environment,
#' society, and/or economy.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of the impact analysis.
#' @param region A \code{Region} or inherited class object representing the
#'   spatial region (template) for the impact analysis.
#' @param incursion An \code{Incursion} or inherited class object representing
#'   the spatial distribution of an actual invasive species presence or
#'   density, or incursion likelihoods across the \code{region}.
#' @param impact_layers A named list of binary (0,1) spatial layers
#'   (\code{terra::SpatRaster} or \code{raster::RasterLayer}) or binary vectors
#'   of locations (consistent with the spatial \code{region}), for indicating
#'   the spatial distribution of each named aspect (mechanism, service, sector,
#'   asset type, etc.) specified via the impact scope in the \code{context}.
#'   Unknown layers/values for aspects may be omitted.
#' @param combine_function The function used to combine impact layers across
#'   aspects of the environment, society, and/or economy. Either \code{"max"},
#'   (for maximum ordinal class), or a user defined function having the form
#'   \code{"function(aspect_locations)"}, where \code{aspect_locations} is
#'   a list of vectors of the class at each \code{region} location for each
#'   \code{context} impact scope aspect, which is passed to the function.
#'   The function should return a single vector of classes for each location.
#' @param mgmt_costs Optional spatial layer (\code{terra::SpatRaster} or
#'   \code{raster::RasterLayer}) or vector of management costs at each
#'   location specified by the \code{region}, measured in the unit specified
#'   in the \code{context}. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{ClassImpacts} class object (list) containing functions
#'   for classifying and combining invasive species (likely) incursion impacts,
#'   and management costs (optional):
#'   \describe{
#'     \item{\code{incursion_impacts()}}{Classify (likely) incursion impacts
#'       for each aspect of the environment, society, and/or economy.}
#'     \item{\code{combined_impacts()}}{Combine (likely) incursion impacts
#'       across aspects of the environment, society, and/or economy, to produce
#'       an overall impact class at each location.}
#'     \item{\code{incursion_mgmt_costs()}}{Calculate (likely) incursion
#'       management costs at each location (when specified).}
#'   }
#' @include ImpactAnalysis.R
#' @export
ClassImpacts <- function(context,
                         region,
                         incursion,
                         impact_layers,
                         combine_function = "max",
                         mgmt_costs = NULL, ...) {
  UseMethod("ClassImpacts")
}

#' @name ClassImpacts
#' @export
ClassImpacts.Context <- function(context,
                                 region,
                                 incursion,
                                 impact_layers,
                                 combine_function = "max",
                                 mgmt_costs = NULL, ...) {

  # Build via base class (for checks)
  self <- ImpactAnalysis(context = context,
                         region = region,
                         incursion = incursion,
                         impact_layers = impact_layers,
                         combine_function = combine_function,
                         mgmt_costs = mgmt_costs,
                         subclass = "ClassImpacts", ...)

  # Check context is consistent with classification impact analysis
  if (!context$get_valuation_type() %in% c("ranking", "categorical")) {
    stop(sprintf(paste("Context is inappropriately configured for class-based",
                       "impact analysis with '%s' valuation."),
                 context$get_valuation_type()),
         call. = FALSE)
  }

  #### TO HERE

  # Calculate (likely) incursion impacts for each aspect
  incursion_impacts <- NULL
  self$incursion_impacts <- function() { # overridden
    if (is.null(incursion_impacts)) {

      # Get impact incursion values
      impact_incursion <- incursion$get_impact_incursion()

      # Extract spatial raster impact layer values
      for (i in 1:length(impact_layers)) {
        if (class(impact_layers[[i]]) %in% c("SpatRaster", "RasterLayer")) {
          impact_layers[[i]] <- impact_layers[[i]][region$get_indices()][,1]
        }
      }

      # Calculate incursion impacts
      incursion_impacts <<- list()
      for (aspect in names(impact_layers)) {
        incursion_impacts[[aspect]] <<-
          impact_layers[[aspect]]*loss_rates[aspect]*impact_incursion
      }

      # Place in spatial rasters when grid region
      if (region$get_type() == "grid") {
        for (aspect in names(impact_layers)) {
          incursion_impact_rast <- region$get_template()
          incursion_impact_rast[region$get_indices()] <-
            incursion_impacts[[aspect]]
          incursion_impacts[[aspect]] <<- incursion_impact_rast
        }
      }
    }
    return(incursion_impacts)
  }

  # Combine (likely) impacts across aspects to produce an overall impact
  combined_impacts <- NULL
  self$combined_impacts <- function() { # overridden
    if (is.null(combined_impacts)) {

      # Get incursion impacts
      incursion_impacts <- self$incursion_impacts()

      # Extract spatial raster incursion impact layer values
      for (i in 1:length(incursion_impacts)) {
        if (class(incursion_impacts[[i]]) %in%
            c("SpatRaster", "RasterLayer")) {
          incursion_impacts[[i]] <-
            incursion_impacts[[i]][region$get_indices()][,1]
        }
      }

      # Combine incursion impacts
      if (is.character(combine_function)) {
        if (combine_function == "sum") {
          combined_impacts <<- rowSums(as.data.frame(incursion_impacts))
        } else if (combine_function == "max") {
          combined_impacts <<- do.call(pmax, incursion_impacts)
        }
      } else if (is.function(combine_function)) {
        combined_impacts <<- combine_function(incursion_impacts)
      }

      # Place in spatial raster when grid region
      if (region$get_type() == "grid") {
        combined_impacts_rast <- region$get_template()
        combined_impacts_rast[region$get_indices()] <- combined_impacts
        combined_impacts <<- combined_impacts_rast
      }
    }
    return(combined_impacts)
  }

  # Calculate (likely) incursion management costs via super class
  incursion_mgmt_costs <- NULL
  super <- list(incursion_mgmt_costs = self$incursion_mgmt_costs)
  if (!is.null(super$incursion_mgmt_costs)) { # when specified
    self$incursion_mgmt_costs <- function() {
      if (is.null(incursion_mgmt_costs)) {
        incursion_mgmt_costs <<- super$incursion_mgmt_costs()
      }
      return(incursion_mgmt_costs)
    }
  }

  return(self)
}

# # Get impact incursion values (quantitative) or mask (class)
# if (context$get_valuation_type() %in% c("monetary", "non-monetary")) {
#   impact_incursion <- incursion$get_impact_incursion()
# } else if (context$get_valuation_type() %in% c("ranking", "categorical")) {
#   impact_incursion <- 1*(incursion$get_impact_incursion() > 0)
# }