context("Region")

test_that("initializes with planar CRS raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  expect_silent(region <- Region(template))
  expect_is(region, "Region")
  expect_equal(region$get_type(), "grid")
  expect_equal(region$get_locations(), length(which(is.finite(template[]))))
  expect_true(region$is_compatible(region$get_template()))
  expect_equal(region$get_indices(), which(is.finite(template[])))
  expect_equal(region$get_res(), 1000)
  expect_equal(region$is_included(5922:5925), c(TRUE, FALSE, FALSE, TRUE))
  expect_silent(features <- region$get_feat())
  expect_is(features, "SpatVector")
  expect_length(features, region$get_locations())
})

test_that("initializes with lonlat raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb_wgs84.tif"))
  expect_silent(region <- Region(template))
  coord <- terra::xyFromCell(template, 1113)
  mid_res <- terra::distance(coord, coord + 0.025, lonlat = TRUE)/sqrt(2)
  expect_true(abs(region$get_res() - mid_res)/mid_res < 0.05)
  expect_equal(region$is_included(1113:1116), c(TRUE, FALSE, FALSE, TRUE))
})

test_that("initializes with CSV data", {
  TEST_DIRECTORY <- test_path("test_inputs")
  locations <- utils::read.csv(file.path(TEST_DIRECTORY, "vic_cities.csv"))
  expect_silent(region <- Region(locations))
  expect_equal(region$get_type(), "patch")
  expect_equal(region$get_locations(), nrow(locations))
  expect_true(region$is_compatible(1:nrow(locations)))
  expect_equal(region$get_data(), locations)
  expect_equal(region$get_coords(), locations[, c("lon", "lat")])
  expect_equal(region$get_coords(extra_cols = TRUE),
               locations[, c("lon", "lat", "name")])
  expect_silent(features <- region$get_feat())
  expect_is(features, "SpatVector")
  expect_length(features, region$get_locations())
})

test_that("empty initialize for single aspatial patch", {
  expect_silent(region <- Region())
  expect_equal(region$get_type(), "single")
  expect_equal(region$get_locations(), 1)
})
