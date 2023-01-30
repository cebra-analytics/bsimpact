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
