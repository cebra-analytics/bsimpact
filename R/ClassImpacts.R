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
#' @param impact_classes A vector of class (ranking or category) values
#'   (consistent with the impact measures in the \code{context}) for each named
#'   aspect (mechanism, service, sector, asset type, etc.) specified via the
#'   impact scope in the \code{context}.
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
                         impact_classes,
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
                                 impact_classes,
                                 combine_function = "max",
                                 mgmt_costs = NULL, ...) {

  # Match combine function
  if (is.character(combine_function)) {
    combine_function <- match.arg(combine_function)
  }

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

  # Check impact classes match context impact measures for each impact scope
  if (length(impact_classes) == length(context$get_impact_scope()) &&
      is.null(names(impact_classes))) {
    names(impact_classes) <- context$get_impact_scope()
    message(paste("Unnamed impact classes assumed to be in order consistent",
                  "with the context impact scope."))
  }
  if (!all(impact_classes %in% context$get_impact_measures()) ||
      (!is.null(names(impact_classes)) &&
       !all(names(impact_classes) %in% context$get_impact_scope())) ||
      (is.null(names(impact_classes)) &&
       length(impact_classes) != length(context$get_impact_scope()))) {
    stop(paste("Impact classes must match context impact measures and be",
               "named consistently with the context impact scopes."),
         call. = FALSE)
  }

  # Calculate (likely) incursion impacts for each aspect
  incursion_impacts <- NULL
  self$incursion_impacts <- function() { # overridden
    if (is.null(incursion_impacts)) {

      # Get binary impact incursion values
      impact_incursion <- 1*(incursion$get_impact_incursion() > 0)

      # Extract/create spatial raster impact layers for each classified aspect
      for (a in names(impact_classes)) {
        if (class(impact_layers[[a]]) %in% c("SpatRaster", "RasterLayer")) {
          impact_layers[[a]] <-
            1*(impact_layers[[a]][region$get_indices()][,1] > 0)
        } else if (is.null(impact_layers[[a]])) {
          impact_layers[[a]] <- rep(1, region$get_locations())
        }
      }

      # Calculate incursion impacts
      incursion_impacts <<- list()
      for (a in names(impact_layers)) {

        # Ranking or class index
        if (context$get_valuation_type() == "ranking" &&
            is.numeric(context$get_impact_measures())) { # numeric ranking
          impact_class_i <- impact_classes[a]
        } else { # character ranking/categorical
          impact_class_i <- which(context$get_impact_measures() ==
                                    impact_classes[a]) - 1
        }

        # Incursion impact ranking or class index
        incursion_impacts[[a]] <<-
          impact_layers[[a]]*impact_class_i*impact_incursion

        # Substitute categories
        if (context$get_valuation_type() == "categorical" ||
            !is.numeric(context$get_impact_measures())) {
          incursion_impacts[[a]] <<-
            factor(context$get_impact_measures()[incursion_impacts[[a]] + 1],
                   context$get_impact_measures())
        }
      }

      # Place in spatial rasters when grid region
      if (region$get_type() == "grid") {
        for (a in names(incursion_impacts)) {
          incursion_impacts[[a]] <<- region$get_rast(incursion_impacts[[a]])
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
      if (is.character(combine_function) && combine_function == "max") {
        if (context$get_valuation_type() == "ranking" &&
            is.numeric(context$get_impact_measures())) { # numeric ranking
            combined_impacts <<- do.call(pmax, incursion_impacts)
        } else { # character ranking/categorical
          combined_impacts <<- do.call(pmax,
                                       lapply(incursion_impacts, as.numeric))
          combined_impacts <<-
            factor(context$get_impact_measures()[combined_impacts],
                   context$get_impact_measures())
        }
      } else if (is.function(combine_function)) {
        combined_impacts <<- combine_function(incursion_impacts)
      }

      # Place in spatial raster when grid region
      if (region$get_type() == "grid") {
        combined_impacts <<- region$get_rast(combined_impacts)
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
