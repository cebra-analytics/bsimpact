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
#'     \item{\code{get_context()}}{Get context object.}
#'     \item{\code{get_incursion()}}{Get incursion object.}
#'     \item{\code{incursion_impacts(raw = FALSE)}}{Calculate (likely)
#'       incursion impacts (damages or losses) for each aspect of the
#'       environment, society, and/or economy. Returns results consistent with
#'       region, or vectors when \code{raw = TRUE}.}
#'     \item{\code{combined_impacts(raw = FALSE)}}{Combine (likely) incursion
#'       impacts across aspects of the environment, society, and/or economy, to
#'       produce an overall impact (damage or loss) at each location. Returns
#'       result consistent with region, or a vector when \code{raw = TRUE}.}
#'     \item{\code{incursion_mgmt_costs()}}{Calculate (likely) incursion
#'       management costs (when specified) at each location.}
#'     \item{\code{total_costs()}}{Calculate (likely) total incursion
#'       (damages/losses plus management) costs at each location (when
#'       \code{context} \code{mgmt_cost_unit} matches \code{impact_measures}).}
#'     \item{\code{save_analysis(...)}}{Save the impact analysis as a
#'       collection of raster TIF and/or comma-separated value (CSV) files,
#'       appropriate for the \code{region} type, including the individual and
#'       combined incursion impacts, and incursion management and total costs
#'       (when specified).\code{Terra} raster write options may be passed to
#'       the function for saving grid-based analysis outputs.}
#'   }
#' @references
#'   Arponen, A., Heikkinen, R. K., Thomas, C. D., & Moilanen, A. (2005). The
#'   Value of Biodiversity in Reserve Selection: Representation, Species
#'   Weighting, and Benefit Functions. \emph{Conservation Biology}, 19(6),
#'   2009–2014. \url{https://www.jstor.org/stable/3591223}
#'
#'   Costanza, R., D'Arge, R., De Groot, R., Farber, S., Grasso, M.,
#'   Hannon, B., Limburg, K., Naeem, S., O'Neill, R. V., & Paruelo, J. (1998).
#'   The value of ecosystem services: putting the issues in perspective.
#'   \emph{Ecological Economics}, 25(E 1), 67–72.
#'   \doi{10.1016/S0921-8009(98)00019-6}
#'
#'   Department of Agriculture, Fisheries and Forestry (DAFF) (2021). National
#'   Environmental Biosecurity Response Agreement 2.0.
#'   \url{https://www.agriculture.gov.au/biosecuritytrade/policy/emergency/nebra}
#'
#'   Dodd, A. J., Stoeckl, N., Baumgartner, J. B. & Kompas, T. F. (2020). Key
#'   Result Summary: Valuing Australia's Biosecurity System. Tech. Rep. 170713,
#'   \emph{Centre of Excellence for Biosecurity Risk Analysis (CEBRA),}
#'   \emph{The University of Melbourne, Melbourne}.
#'   \url{https://cebra.unimelb.edu.au/__data/assets/pdf_file/0020/3535013/CEBRA_Value_Docs_KeyResultSummary_v0.6_Endorsed.pdf}
#'
#'   MacLeod, A., Head, J., & Gaunt, A. (2004). An assessment of the potential
#'   economic impact of Thrips palmi on horticulture in England and the
#'   significance of a successful eradication campaign. \emph{Crop Protection},
#'   23(7), 610–601. \doi{10.1016/j.cropro.2003.11.010}
#'
#'   O'Loughlin, L. S., Gooden, B., Barney, J. N., & Lindenmayer, D. B. (2019).
#'   Surrogacy in invasion research and management : inferring "impact" from
#'   "invasiveness". \emph{Frontiers in Ecology and the Environment}, 17(8),
#'   464–473. \doi{10.1002/fee.2097}
#'
#'   Soliman, T., Mourits, M. C. M., Oude Lansink, A. G. J. M., &
#'   van der Werf, W. (2010). Economic impact assessment in pest risk analysis.
#'   \emph{Crop Protection}, 29(6), 517–524. \doi{10.1016/j.cropro.2009.12.014}
#'
#'   Soliman, T., Mourits, M. C. M., Oude Lansink, A. G. J. M., &
#'   van der Werf, W. (2015). Quantitative economic impact assessment of
#'   invasive plant pests: What does it require and when is it worth the
#'   effort? \emph{Crop Protection}, 69, 9–17.
#'   \doi{10.1016/j.cropro.2014.11.011}
#'
#'   Stoeckl, N., Dodd, A., & Kompas, T. (2020). Values and vulnerabilities:
#'   what assets are protected by Australia's national biosecurity system and
#'   thus at risk of incursion? \emph{Centre of Excellence for Biosecurity}
#'   \emph{Risk Analysis (CEBRA), The University of Melbourne, Melbourne}.
#'   \url{https://cebra.unimelb.edu.au/__data/assets/pdf_file/0011/3898946/CEBRA_Value_Docs_ValuesAndVulnerabilities_v0.4-PostSAC.pdf}
#'
#'   Weitzman, M. L. (1998). The Noah's Ark Problem. \emph{Econometrica},
#'   66(6), 1279–1298. \doi{10.2307/2999617}
#'
#'   Welsh, M. J., Brockerhoff, E. G., Kean, J. M., Phillips, C.,
#'   Stringer, L. D., Vereijssen, J., Turner, J. A., Epanchin-Niell, R. S.,
#'   Monge, J. J., Soliman, T., Robinson, A. P., Kompas, T., Liebhold, A. M.,
#'   & Ormsby, M. (2021). Approaches for estimating benefits and costs of
#'   interventions in plant biosecurity across invasion phases.
#'   \emph{Ecological Applications}, 31(5). \doi{10.1002/eap.2319}
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
  self$incursion_impacts <- function(raw = FALSE) { # overridden
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
      if (region$get_type() == "grid" && !raw) {
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
    self$combined_impacts <- function(raw = FALSE) { # overridden
      if (is.null(combined_impacts)) {

        # Get incursion impacts
        incursion_impacts <- self$incursion_impacts(raw = raw)

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
        if (region$get_type() == "grid" && !raw) {
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

  # Save the impact analysis as a collection of appropriate files
  self$save_analysis <- function(...) {

    # Save individual incursion impacts
    incursion_impacts <- self$incursion_impacts()
    if (region$get_type() == "grid") {
      for (aspect in names(incursion_impacts)) {
        terra::writeRaster(incursion_impacts[[aspect]],
                           filename = sprintf("incursion_impacts_%s.tif",
                                              aspect), ...)
      }
    } else if (region$get_type() == "patch") {
      names(incursion_impacts) <- paste0(names(incursion_impacts), "_impact")
      analysis_data <- cbind(region$get_coords(extra_cols = TRUE),
                             incursion_impacts)
    } else if (region$get_type() == "single") {
      names(incursion_impacts) <- paste0(names(incursion_impacts), "_impact")
      analysis_data <- as.data.frame(incursion_impacts)
    }

    # Save combined incursion impacts
    if (!is.null(self$combined_impacts)) {
      combined_impacts <- self$combined_impacts()
      if (!is.null(combined_impacts)) {
        if (region$get_type() == "grid") {
          terra::writeRaster(combined_impacts,
                             filename = "combined_impacts.tif", ...)
        } else if (region$get_type() %in% c("patch", "single")) {
          analysis_data$combined_impact <- combined_impacts
        }
      }
    }

    # Save incursion management costs
    if (!is.null(self$incursion_mgmt_costs)) {
      incursion_mgmt_costs <- self$incursion_mgmt_costs()
      if (!is.null(incursion_mgmt_costs)) {
        if (region$get_type() == "grid") {
          terra::writeRaster(incursion_mgmt_costs,
                             filename = "incursion_mgmt_costs.tif", ...)
        } else if (region$get_type() %in% c("patch", "single")) {
          analysis_data$incursion_mgmt_cost <- incursion_mgmt_costs
        }
      }
    }

    # Save total incursion costs
    if (!is.null(self$total_costs)) {
      total_costs <- self$total_costs()
      if (!is.null(total_costs)) {
        if (region$get_type() == "grid") {
          terra::writeRaster(total_costs, filename = "total_costs.tif", ...)
        }
      } else if (region$get_type() %in% c("patch", "single")) {
        analysis_data$total_cost <- total_costs
      }
    }

    # Save analysis data when patch/single to CSV file
    if (region$get_type() %in% c("patch", "single")) {
      write.csv(analysis_data, "impact_analysis.csv", row.names = FALSE)
    }
  }

  return(self)
}
