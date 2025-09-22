context("Incursion")

test_that("initializes with raster layer", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion_rast <- template
  expect_error(Incursion(incursion_rast, 1:10),
               "Region model must be a 'Region' or inherited class object.")
  expect_error(Incursion(1:10, region),
               paste("The length of x must be equal to the number of region",
                     "locations."))
  expect_error(Incursion(incursion_rast - 0.1, region),
               "The values of x must be >= 0.")
  expect_error(Incursion(incursion_rast, region, multiplier = 0),
               "The multiplier must be numeric and > 0.")
  expect_error(Incursion(incursion_rast, region, threshold = 10),
               "The threshold must be numeric, >= 0, and <= 1.")
  expect_error(Incursion(incursion_rast*10, region, type = "prob"),
               "The probability values of x must be <= 1.")
  expect_silent(incursion <- Incursion(incursion_rast, region))
  expect_is(incursion, "Incursion")
  expect_equal(incursion$get_type(), "presence")
})

test_that("transforms incursion values via multiplier and threshold", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  expect_silent(incursion <- Incursion(template, region, multiplier = 2,
                                       threshold = 0.3))
  expected_x <- template[][,1][region$get_indices()]
  expected_x <- expected_x*2*(expected_x >= 0.15)
  expected_x[which(expected_x > 1)] <- 1
  expect_equal(incursion$get_impact_incursion(), expected_x)
})

test_that("sets incursion values", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  expect_silent(incursion <- Incursion(template, region, type = "prob"))
  original_x <- template[][,1][region$get_indices()]
  expect_error(incursion$set_values(1:10),
               paste("The length of the incursion values must be equal to the",
                     "number of region locations."))
  expect_error(incursion$set_values(-1*original_x),
               "The incursion values must be >= 0.")
  expect_error(incursion$set_values(2*original_x),
               "The incursion probability values must be <= 1.")
  expect_silent(incursion$set_values(0.8*original_x))
  expect_equal(incursion$get_impact_incursion(), 0.8*original_x)
  attr(original_x, "recovery_delay") <- rep(3, region$get_locations())
  expect_silent(incursion$set_values(original_x))
  expect_equal(incursion$get_impact_incursion(), original_x)
  expect_equal(attr(incursion$get_impact_incursion(), "recovery_delay"),
               attr(original_x, "recovery_delay"))
})

test_that("handles area incursions", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  expect_silent(incursion <- Incursion(template*100, region, type = "area"))
  expect_equal(incursion$get_type(), "area")
  expected_x <- template[][,1][region$get_indices()]*100
  expect_equal(incursion$get_impact_incursion(), expected_x)
  expect_silent(incursion <- Incursion(100, Region(), type = "area"))
  expect_equal(incursion$get_impact_incursion(), 100)
})
