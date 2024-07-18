#' Context class builder
#'
#' Builds a class to represent the context of a bio-security impact analysis,
#' including: information about the invasive pest, weed or disease species,
#' genus or functional group involved; the types of impacts involved
#' (ecological/environmental, social/human-welfare, and/or economic/market);
#' the scope of impacts, such as aspects of environment (e.g. ecological
#' mechanisms), social aspects (e.g. culture, health), and/or economic sectors;
#' the valuation or measures used, such as monetary values, other quantified
#' values (e.g. national significance, distinctiveness), ranking (e.g. GISS) or
#' classification (e.g. EICAT, SEICAT).
#'
#' @param species_name Invasive species, genus, or functional group name.
#' @param species_type Type of invasive species. One of \code{"pest"},
#'   \code{"weed"}, or \code{"disease"}.
#' @param impact_types The types of impacts involved. One or more of
#'   \code{"ecological"}, \code{"social"}, and/or \code{"economic"}.
#' @param impact_scope A vector of aspects of the impact types analysed, such
#'   as ecological mechanisms, ecosystem services, social aspects, economic
#'   sectors and/or asset types.
#' @param valuation_type The type of valuation used to measure impacts. One of
#'   \code{"monetary"}, \code{"non-monetary"} (quantities), \code{"ranking"},
#'   or \code{"categorical"}.
#' @param impact_measures A vector of one or more measures used to quantify or
#'   classify each impact aspect, consistent with the valuation type. Monetary
#'   or non-monetary (quantitative) measures should specify the unit used (e.g.
#'   "$"). Ranking or categorical measures should specify a list of
#'   values/categories in ascending order (when applicable), such as GISS
#'   (0, 1, 2, 3, 4, 5) ranking, or EICAT/SEICAT ("-", "MC", "MN", "MO", "MR",
#'   "MV") categories, where the first ranking/category represents the value or
#'   symbol for no impact or invasive species presence. Default is "$".
#' @param mgmt_cost_unit The unit of measure for management costs. This will
#'   typically be the same unit as \code{"impact_measures"} when the
#'   \code{"valuation_type"} is \code{"monetary"}. One of \code{"$"} (default),
#'   \code{"hours"}, \code{"none"}, or user specified.
#' @param ... Additional parameters.
#' @return A \code{Context} class object (list) containing functions for
#'   accessing attributes:
#'   \describe{
#'     \item{\code{get_species_name()}}{Get the invasive species, genus, or
#'       functional group name.}
#'     \item{\code{get_species_type()}}{Get the type of invasive species:
#'       "pest", "weed", or "disease".}
#'     \item{\code{get_impact_types()}}{Get the types of impacts analysed:
#'       "ecological", "social", and/or "economic".}
#'     \item{\code{get_impact_scope()}}{Get the aspects of impacts analysed.}
#'     \item{\code{get_valuation_type()}}{Get the type of valuation used to
#'       measure impacts: "monetary", "non-monetary", "ranking",
#'       "categorical".}
#'     \item{\code{get_impact_measures()}}{Get the measures used to quantify or
#'       classify each impact aspect.}
#'     \item{\code{get_mgmt_cost_unit()}}{Get the unit of measure for
#'       management costs.}
#'   }
#' @references
#'   Bacher, S., Blackburn, T. M., Essl, F., Genovesi, P., Heikkilä, J.,
#'   Jeschke, J. M., Jones, G., Keller, R., Kenis, M., Kueffer, C.,
#'   Martinou, A. F., Nentwig, W., Pergl, J., Pyšek, P., Rabitsch, W.,
#'   Richardson, D. M., Roy, H. E., Saul, W-C., Scalera, R., Vilà, M.,
#'   Wilson, J. R. U., Kumschick, S. (2018). Socio-economic impact
#'   classification of alien taxa (SEICAT).
#'   \emph{Methods in Ecology and Evolution}, 9(1), 159–168.
#'   \doi{10.1111/2041-210X.12844}
#'
#'   Blackburn, T. M., Essl, F., Evans, T., Hulme, P. E., Jeschke, J. M.,
#'   Kühn, I., Kumschick, S., Marková, Z., Mrugała, A., Nentwig, W., Pergl, J.,
#'   Pyšek, P., Rabitsch, W., Ricciardi, A., Richardson, D. M., Sendek, A.,
#'   Vilà, M., Wilson, J. R. U., Winter, M., Genovesi, P., & Bacher, S. (2014).
#'   A Unified Classification of Alien Species Based on the Magnitude of their
#'   Environmental Impacts. \emph{PLoS Biology}, 12(5).
#'   \doi{10.1371/journal.pbio.1001850}
#'
#'   IUCN (2020). IUCN EICAT Categories and Criteria. The Environmental Impact
#'   Classification for Alien Taxa (EICAT) First edition.
#'   \emph{IUCN, Gland, Switzerland and Cambridge, UK}. IUCN.
#'   \doi{10.2305/IUCN.CH.2020.05.en}
#'
#'   Nentwig, W., Kühnel, E., & Bacher, S. (2010). A Generic Impact-Scoring
#'   System Applied to Alien Mammals in Europe. \emph{Conservation Biology},
#'   24(1), 302–311. \doi{10.1111/j.1523-1739.2009.01289.x}
#' @export
Context <- function(species_name,
                    species_type = c("pest",
                                     "weed",
                                     "disease"),
                    impact_types = c("ecological",
                                     "social",
                                     "economic"),
                    impact_scope,
                    valuation_type = c("monetary",
                                       "non-monetary",
                                       "ranking",
                                       "categorical"),
                    impact_measures = "$",
                    mgmt_cost_unit = c("$",
                                       "hours",
                                       "none",
                                       "user"), ...) {
  UseMethod("Context")
}

#' @name Context
#' @export
Context.default <- function(species_name,
                            species_type = c("pest",
                                             "weed",
                                             "disease"),
                            impact_types = c("ecological",
                                             "social",
                                             "economic"),
                            impact_scope,
                            valuation_type = c("monetary",
                                               "non-monetary",
                                               "ranking",
                                               "categorical"),
                            impact_measures = "$",
                            mgmt_cost_unit = c("$",
                                               "hours",
                                               "none",
                                               "user"), ...) {

  # Match arguments to selections
  species_type <- match.arg(species_type)
  impact_types <- match.arg(impact_types, several.ok = TRUE)
  valuation_type <- match.arg(valuation_type)
  if (!is.character(mgmt_cost_unit) || length(mgmt_cost_unit) > 1) {
    mgmt_cost_unit <- match.arg(mgmt_cost_unit)
  }

  # Ensure impact scope/aspects are strings
  impact_scope <- as.character(impact_scope)

  # Ensure impact measures are appropriate for valuation type
  if (valuation_type == "monetary" && length(unique(impact_measures)) > 1) {
    stop("Monetary impacts must use the same impact measure.", call. = FALSE)
  } else if (valuation_type == "non-monetary" &&
             !length(impact_measures) %in% c(1, length(impact_scope))) {
    stop(paste("The number of non-monetary impact measures must be consistent",
               "with the number of impact scope aspects."), call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = "Context")

  # Get the invasive species, genus, or functional group name
  self$get_species_name <- function() {
    return(species_name)
  }

  # Get the type of invasive species
  self$get_species_type <- function() {
    return(species_type)
  }

  # Get the types of impacts analysed
  self$get_impact_types <- function() {
    return(impact_types)
  }

  # Get the aspects of impacts analysed
  self$get_impact_scope <- function() {
    return(impact_scope)
  }

  # Get the type of valuation used to measure impacts
  self$get_valuation_type <- function() {
    return(valuation_type)
  }

  # Get the measures used to quantify or classify each impact aspect
  self$get_impact_measures <- function() {
    return(impact_measures)
  }

  # Get the unit of measure for management costs
  self$get_mgmt_cost_unit <- function() {
    return(mgmt_cost_unit)
  }

  return(self)
}
