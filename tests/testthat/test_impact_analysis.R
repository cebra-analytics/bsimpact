context("ImpactAnalysis")

test_that("initializes with parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"))
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  template_alt <- terra::rast(file.path(TEST_DIRECTORY,
                                        "greater_melb_wgs84.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 100*template, aspect2 = 200*template)
  expect_error(ImpactAnalysis(context, region = 0, 0, 0),
               "Region model must be a 'Region' or inherited class object.")
  expect_error(ImpactAnalysis(context, region, incursion = 0, 0),
               paste("Incursion model must be a 'Incursion' or inherited",
                     "class object."))
  expect_error(ImpactAnalysis(context, region, incursion, impact_layers = 0),
               paste("Impact layers must be a named list of spatial layers or",
                     "location vectors, named consistently with the context",
                     "impact scope, and which are compatible with the defined",
                     "region."))
  expect_error(ImpactAnalysis(context, region, incursion,
                              impact_layers = list(bad = 0)),
               paste("Impact layers must be a named list of spatial layers or",
                     "location vectors, named consistently with the context",
                     "impact scope, and which are compatible with the defined",
                     "region."))
  expect_error(ImpactAnalysis(context, region, incursion,
                 impact_layers = list(aspect1 = 100*template,
                                      aspect2 = template_alt)),
               paste("Impact layers must be a named list of spatial layers or",
                     "location vectors, named consistently with the context",
                     "impact scope, and which are compatible with the defined",
                     "region."))
  expect_error(ImpactAnalysis(context, region, incursion,
                              impact_layers = list(aspect1 = 100*template,
                                                   aspect2 = 1:10)),
               paste("Impact layers must be a named list of spatial layers or",
                     "location vectors, named consistently with the context",
                     "impact scope, and which are compatible with the defined",
                     "region."))
  expect_error(ImpactAnalysis(context, region, incursion, impact_layers,
                              combine_function = 0),
               paste("Combine function must be 'sum', 'max', or user-defined",
                     "with form function(aspect_locations)."), fixed = TRUE)
  expect_error(ImpactAnalysis(context, region, incursion, impact_layers,
                              combine_function = function(a, b) 0),
               paste("Combine function must be 'sum', 'max', or user-defined",
                     "with form function(aspect_locations)."), fixed = TRUE)
  expect_silent(impact <- ImpactAnalysis(context, region, incursion,
                                         impact_layers))
  expect_is(impact, "ImpactAnalysis")
  expect_named(impact, c("incursion_impacts", "combined_impacts"))
  expect_null(impact$incursion_impacts())
  expect_null(impact$combined_impacts())
})

test_that("calculates incursion management costs", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"))
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  template_alt <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb_wgs84.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 100*template, aspect2 = 200*template)
  mgmt_costs = template*0 + 300
  expect_error(ImpactAnalysis(context, region, incursion, impact_layers,
                              mgmt_costs = list()),
               paste("Management costs must be a spatial layer or location",
                     "vector compatible with the defined region."))
  expect_error(ImpactAnalysis(context, region, incursion, impact_layers,
                              mgmt_costs = 1:10),
               paste("Management costs must be a spatial layer or location",
                     "vector compatible with the defined region."))
  expect_error(ImpactAnalysis(context, region, incursion, impact_layers,
                              mgmt_costs = template_alt),
               paste("Management costs must be a spatial layer or location",
                     "vector compatible with the defined region."))
  expect_silent(impact <- ImpactAnalysis(
    context, region, incursion, impact_layers, mgmt_costs = mgmt_costs))
  expect_named(impact, c("incursion_impacts", "combined_impacts",
                         "incursion_mgmt_costs"))
  expected_values <- (mgmt_costs[region$get_indices()][,1]*
                        incursion$get_impact_incursion())
  expect_silent(incursion_mgmt_costs <- impact$incursion_mgmt_costs())
  expect_is(incursion_mgmt_costs, "SpatRaster")
  expect_equal(incursion_mgmt_costs[region$get_indices()][,1], expected_values)
})
