context("ImpactAnalysis")

test_that("initializes with parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species",
                     impact_scope = c("aspect1", "aspect2"),
                     impact_measures = "$") # silent
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 2*template, aspect2 = 3*template)
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
  template_alt <- terra::rast(file.path(TEST_DIRECTORY,
                                        "greater_melb_wgs84.tif"))
  expect_error(ImpactAnalysis(context, region, incursion,
                 impact_layers = list(aspect1 = 2*template,
                                      aspect2 = template_alt)),
               paste("Impact layers must be a named list of spatial layers or",
                     "location vectors, named consistently with the context",
                     "impact scope, and which are compatible with the defined",
                     "region."))
  expect_error(ImpactAnalysis(context, region, incursion,
                              impact_layers = list(aspect1 = 2*template,
                                                   aspect2 = 1:10)),
               paste("Impact layers must be a named list of spatial layers or",
                     "location vectors, named consistently with the context",
                     "impact scope, and which are compatible with the defined",
                     "region."))
  expect_error(ImpactAnalysis(context, region, incursion, impact_layers, combine_function = 0),
               paste("Combine function must be 'sum', 'max', or a",
                     "user-defined function."))
  expect_silent(impact <- ImpactAnalysis(context, region, incursion,
                                         impact_layers))
  expect_is(impact, "ImpactAnalysis")
  expect_null(impact$incursion_impacts())
  expect_null(impact$combined_impacts())
})
