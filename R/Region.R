#' Region class builder
#'
#' Builds a class to represent a spatial region for an impact analysis,
#' defined via a raster layer with active (non-NA) cells, or via a data
#' frame specifying locations, patches, or sub-regions.
#'
#' @param x A \code{terra::SpatRaster} or \code{raster::RasterLayer} object
#'   representing a grid-based spatial region (template). The spatial division
#'   of locations, patches, or sub-regions may also be defined via a data frame
#'   (or matrix) of location coordinates in longitude and latitude (WGS84) with
#'   explicitly named columns "lon" and "lat". If missing, the region is
#'   assumed to be a single aspatial patch.
#' @param ... Additional parameters.
#' @return A \code{Divisions} class object (list) containing functions for
#'   accessing attributes, checking compatibility of objects with the
#'   region, and to maintain and calculate spatial data:
#'   \describe{
#'     \item{\code{get_type()}}{Get the type of representation: spatial "grid"
#'       (raster cells), "patch" (coordinates), or "single" (aspatial).}
#'     \item{\code{get_locations()}}{Get the number of locations (cells or
#'       patches) that are included in the simulation.}
#'     \item{\code{is_compatible(y)}}{Check the compatibility of object
#'       \code{y} with the region defined by \code{x}.}
#'     \item{\code{get_template(empty = FALSE)}}{Get the spatial template when
#'      the \code{type} is "grid", with either zeros in non-NA locations
#'      (default), or with no values when \code{empty = TRUE}.}
#'     \item{\code{get_rast(values)}}{Get spatial \code{terra::SpatRaster} set
#'       with a single value or a vector of values matching the number of
#'       locations when \code{type} is "grid".}
#'     \item{\code{get_indices()}}{Get cell indices of grid or patch locations
#'       that are included in the simulation when the \code{type} is "grid".}
#'     \item{\code{get_res()}}{Get the spatial cell resolution (in m) of the
#'       region when the \code{type} is "grid".}
#'     \item{\code{is_included(indices)}}{Check if cell \code{indices} of grid
#'       locations are included (non-NA cells) in the simulation when the
#'       \code{type} is "grid", and return a logical vector indicating the
#'       inclusion of each index.}
#'     \item{\code{get_data()}}{Get the data frame specifying the division when
#'       \code{type} is "patch" or "other".}
#'     \item{\code{get_coords(extra_cols = FALSE)}}{Get a data frame of patch
#'       location coordinates when \code{type} is "patch", as well as optional
#'       extra named columns from the original location data.}
#'     \item{\code{get_feat()}}{Get spatial \code{terra::SpatVector} features
#'       or points when \code{type} is "grid" or "patch".}
#'   }
#' @export
Region <- function(x, ...) {
  UseMethod("Region")
}

#' @name Region
#' @export
Region.Raster <- function(x, ...) {

  # Call the terra version of the function
  Region(terra::rast(x), ...)
}

#' @name Region
#' @export
Region.SpatRaster <- function(x, ...) {

  # Non-NA cell indices and points (terra::SpatVector)
  indices <- which(!is.na(x[]))
  grid_pts <- terra::as.points(x, values = FALSE)

  # Create a class structure
  self <- structure(list(), class = "Region")

  # Get the divisions type
  self$get_type <- function() {
    return("grid")
  }

  # Get the number of active cell locations
  self$get_locations <- function() {
    return(length(indices))
  }

  # Check compatibility of a spatial raster y with the divisions specified by x
  self$is_compatible <- function(y) {
    y <- terra::rast(y)
    return(terra::crs(y) == terra::crs(x) &&
             terra::ext(y) == terra::ext(x) &&
             all(terra::res(y) == terra::res(x)))
  }

  # Get spatial template with zero/NA or empty values
  self$get_template <- function(empty = FALSE) {
    if (empty) {
      template <- terra::rast(x)
    } else {
      template <- x*0
      names(template) <- "value"
    }
    return(template)
  }

  # Get spatial raster set with values
  self$get_rast <- function(values) {

    # Check values is vector of correct length
    if (!length(values) %in% c(1, self$get_locations())) {
      stop(paste("Values should be a single value or vector of length",
                 "matching the number of region locations."), call. = FALSE)
    }

    # Get a template raster to populate
    value_rast <- self$get_template()

    # Set categories when present
    if (is.factor(values)) {
      levels(value_rast) <- data.frame(ID = 1:length(levels(values)),
                                       category = levels(values))
    }

    # Set values
    value_rast[indices] <- values

    return(value_rast)
  }

  # Get cell indices
  self$get_indices <- function() {
    return(indices)
  }

  # Get the spatial cell resolution
  self$get_res <- function() {
    if (terra::is.lonlat(x)) { # EPSG:4326
      corners <- array(terra::ext(x), c(2, 2))
      diagonal <- terra::distance(corners[1,,drop = FALSE],
                                  corners[2,,drop = FALSE], lonlat = TRUE)
      return(diagonal/sqrt(terra::nrow(x)^2 + terra::ncol(x)^2))
    } else {
      return(mean(terra::res(x)[1]))
    }
  }

  # Check if cell indices are included (non-NA cells) in the design
  self$is_included <- function(indices) {
    return(!is.na(x[indices][,1]))
  }

  # Get spatial terra::SpatVector features or points
  self$get_feat <- function() {
    return(grid_pts)
  }

  return(self)
}

#' @name Region
#' @export
Region.matrix <- function(x, ...) {

  # Call the data frame version of the function
  Region(as.data.frame(x), ...)
}

#' @name Region
#' @export
Region.data.frame <- function(x, ...) {

  # Check data frame
  if (!all(c("lon", "lat") %in% names(x))) {
    stop("Coordinate data frame must contain columns named 'lon' and 'lat'.",
         call. = FALSE)
  }

  # Patch points (terra::SpatVector)
  patch_pts <- terra::vect(x[, c("lon", "lat")], crs = "EPSG:4326")

  # Create a class structure
  self <- structure(list(), class = "Region")

  # Get the spatial region type
  self$get_type <- function() {
    return("patch")
  }

  # Get the number of patch locations
  self$get_locations <- function() {
    return(nrow(x))
  }

  # Check compatibility of vector, matrix, or adjacency data frame y
  # with the divisions defined by x
  self$is_compatible <- function(y) {
    if (is.data.frame(y)) {
      return(ncol(y) == 3 && all(unique(unlist(y[,1:2])) %in% 1:nrow(x)))
    } else {
      y <- as.matrix(y)
      return(nrow(y) == nrow(x) && ncol(y) %in% c(1, nrow(x)))
    }
  }

  # Get the data frame specifying the division
  self$get_data <- function() {
    return(x)
  }

  # Get location coordinates plus optional extra named columns
  self$get_coords <- function(extra_cols = FALSE) {
    if (extra_cols) {
      extra_cols <- names(x)[which(!names(x) %in% c("lon", "lat"))]
      return(x[, c("lon", "lat", extra_cols)])
    } else {
      return(x[, c("lon", "lat")])
    }
  }

  # Get spatial terra::SpatVector features or points
  self$get_feat <- function() {
    return(patch_pts)
  }

  return(self)
}

#' @name Region
#' @export
Region.NULL <- function(...) {

  # Create a class structure
  self <- structure(list(), class = "Region")

  # Get the spatial region type
  self$get_type <- function() {
    return("single")
  }

  # Get the number of patch locations
  self$get_locations <- function() {
    return(1)
  }

  return(self)
}
