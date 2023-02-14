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
#' @param combine_function The function used to combine impact layers across
#'   aspects of the environment, society, and/or economy. Either \code{"sum"}
#'   or a user defined function having the form
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
#' @return A \code{ValueImpacts} class object (list) containing functions
#'   for calculating and combining invasive species (likely) incursion impacts,
#'   management costs (optional), and total costs (when applicable):
#'   \describe{
#'     \item{\code{incursion_impacts()}}{Calculate (likely) incursion impacts
#'       (damages or losses) for each aspect of the environment, society,
#'       and/or economy.}
#'     \item{\code{combined_impacts()}}{Combine (likely) incursion impacts
#'       across aspects of the environment, society, and/or economy, to produce
#'       an overall impact (damage or loss) at each location.}
#'     \item{\code{incursion_mgmt_costs()}}{Calculate (likely) incursion
#'       management costs (when specified) at each location.}
#'     \item{\code{total_costs()}}{Calculate (likely) total incursion
#'       (damages/losses plus management) costs at each location (when
#'       \code{context} \code{mgmt_cost_unit} matches \code{impact_measures}).}
#'   }
#' @include ImpactAnalysis.R
#' @export
ValueImpacts <- function(context,
                         region,
                         incursion,
                         impact_layers,
                         loss_rates,
                         combine_function = c("sum", "none"),
                         mgmt_costs = NULL, ...) {
  UseMethod("ValueImpacts")
}

#' @name ValueImpacts
#' @export
ValueImpacts.Context <- function(context,
                                 region,
                                 incursion,
                                 impact_layers,
                                 loss_rates,
                                 combine_function = c("sum", "none"),
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
                         subclass = "ValueImpacts", ...)

  # Check context is consistent with quantitative impact analysis
  if (!context$get_valuation_type() %in% c("monetary", "non-monetary") ||
      length(context$get_impact_measures()) > 1 ||
      !is.character(context$get_impact_measures())) {
    stop(sprintf(paste("Context is inappropriately configured for value-based",
                       "impact analysis with '%s' valuation or invalid",
                       "measure(s)."), context$get_valuation_type()),
         call. = FALSE)
  }

  # Check loss rates
  if (length(loss_rates) == length(context$get_impact_scope()) &&
      is.null(names(loss_rates))) {
    names(loss_rates) <- context$get_impact_scope()
    message(paste("Unnamed loss rates assumed to be in order consistent with",
                  "the context impact scope."))
  }
  if (!is.numeric(loss_rates) || any(loss_rates < 0) || any(loss_rates > 1) ||
      (!is.null(names(loss_rates)) &&
       !all(names(loss_rates) %in% context$get_impact_scope())) ||
      (is.null(names(loss_rates)) &&
       length(loss_rates) != length(context$get_impact_scope()))) {
    stop(paste("Loss rates must be numeric, >= 0, <= 1, and named",
               "consistently with the context impact scope."), call. = FALSE)
  }

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
          incursion_impacts[[aspect]] <<-
            region$get_rast(incursion_impacts[[aspect]])
        }
      }
    }

    return(incursion_impacts)
  }

  # Combine (likely) impacts across aspects to produce an overall impact
  combined_impacts <- NULL
  if (!is.character(combine_function) || combine_function != "none") {
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
          combined_impacts <<- region$get_rast(combined_impacts)
        }
      }

      return(combined_impacts)
    }
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

  # Calculate (likely) total incursion (damages/losses plus management) costs
  if (!is.null(mgmt_costs) &&
      (is.function(self$combined_impacts) || length(impact_layers) == 1) &&
      all(context$get_mgmt_cost_unit() == context$get_impact_measures())) {
    self$total_costs <- function() {
      if (length(impact_layers) == 1) {
        return(self$incursion_impacts()[[1]] + self$incursion_mgmt_costs())
      } else {
        return(self$combined_impacts() + self$incursion_mgmt_costs())
      }
    }
  }

  return(self)
}
