context("ClassImpacts")

test_that("initializes with parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")

  context <- Context("My species", impact_scope = c("aspect1", "aspect2"),
                     valuation_type = "ranking", impact_measures = 0:5)
  context_alt <- Context("My species", impact_scope = c("aspect1", "aspect2"),
                         valuation_type = "monetary")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 1*(template > 0.1 & template < 0.3),
                        aspect2 = 1*(template > 0.2 & template < 0.4))
  impact_classes <- c(aspect1 = 3, aspect2 = 2)
  expect_error(ClassImpacts(context_alt, region, incursion, impact_layers,
                            impact_classes),
               paste("Context is inappropriately configured for class-based",
                     "impact analysis with 'monetary' valuation."))
  expect_error(ClassImpacts(context, region, incursion, impact_layers,
                            impact_classes = 3),
               paste("Impact classes must match context impact measures and",
                     "be named consistently with the context impact scopes."))
  expect_error(ClassImpacts(context, region, incursion, impact_layers,
                            impact_classes = c(a = 3, b = 2)),
               paste("Impact classes must match context impact measures and",
                     "be named consistently with the context impact scopes."))
  expect_error(ClassImpacts(context, region, incursion, impact_layers,
                            impact_classes = c(aspect1 = 6, aspect2 = 2)),
               paste("Impact classes must match context impact measures and",
                     "be named consistently with the context impact scopes."))
  expect_message(
    impacts <- ClassImpacts(context, region, incursion, impact_layers,
                            impact_classes = c(3, 2)),
    paste("Unnamed impact classes assumed to be in order consistent with the",
          "context impact scope."))
  expect_silent(impacts <- ClassImpacts(context, region, incursion,
                                        impact_layers, impact_classes))
  expect_is(impacts, "ClassImpacts")
  expect_s3_class(impacts, "ImpactAnalysis")
  expect_named(impacts, c("incursion_impacts", "combined_impacts",
                          "save_analysis"))

  expect_silent(impacts <- ClassImpacts(context, region, incursion,
                                        impact_layers, impact_classes,
                                        combine_function = "none"))
  expect_named(impacts, c("incursion_impacts", "save_analysis"))
  expect_silent(impacts <- ClassImpacts(context, region, incursion,
                                        impact_layers, impact_classes,
                                        combine_function = function(x) x))
  expect_named(impacts, c("incursion_impacts", "combined_impacts",
                          "save_analysis"))
})

test_that("classifies individual and combines ranking incursion impacts", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"),
                     valuation_type = "ranking", impact_measures = 0:5)
  impact_classes <- c(aspect1 = 3, aspect2 = 2)
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 1*(template > 0.1 & template < 0.3),
                        aspect2 = 1*(template > 0.2 & template < 0.4))
  expect_silent(impacts <- ClassImpacts(context, region, incursion,
                                        impact_layers, impact_classes))
  expected_impacts <-
    lapply(list(aspect1 = "aspect1", aspect2 = "aspect2"), function(a) {
      (impact_layers[[a]][region$get_indices()][,1]*impact_classes[a]*
         (incursion$get_impact_incursion() > 0))})
  expect_silent(incursion_impacts <- impacts$incursion_impacts())
  expect_equal(sapply(incursion_impacts, class),
               c(aspect1 = "SpatRaster", aspect2 = "SpatRaster"))
  expect_equal(lapply(incursion_impacts,
                      function(l) l[region$get_indices()][,1]),
               expected_impacts)
  expected_combined_impacts <- pmax(expected_impacts[[1]],
                                    expected_impacts[[2]])
  expect_silent(combined_impacts <- impacts$combined_impacts())
  expect_is(combined_impacts, "SpatRaster")
  expect_equal(combined_impacts[region$get_indices()][,1],
               expected_combined_impacts)
})

test_that("classifies individual and combines categorical incursion impacts", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species",
                     impact_scope = c("aspect1", "aspect2", "aspect3"),
                     valuation_type = "categorical",
                     impact_measures = c("-", "MC", "MN", "MO", "MR", "MV"))
  impact_classes <- c(aspect1 = "MO", aspect2 = "MV", aspect3 = "MC")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 1*(template > 0.1 & template < 0.3),
                        aspect2 = 1*(template > 0.2 & template < 0.4))
  expect_silent(impacts <- ClassImpacts(context, region, incursion,
                                        impact_layers, impact_classes))
  expected_impacts <-
    lapply(list(aspect1 = "aspect1", aspect2 = "aspect2"), function(a) {
      unname(
        c("-", impact_classes[a])[impact_layers[[a]][region$get_indices()][,1]*
                                    (incursion$get_impact_incursion() > 0) + 1]
      )})
  expected_impacts$aspect3 <- unname(
    c("-", impact_classes[3])[(incursion$get_impact_incursion() > 0) + 1])
  for (a in context$get_impact_scope()) {
    expected_impacts[[a]] <- factor(expected_impacts[[a]],
                                    context$get_impact_measures())
  }
  expect_silent(incursion_impacts <- impacts$incursion_impacts())
  expect_equal(sapply(incursion_impacts, class),
               c(aspect1 = "SpatRaster", aspect2 = "SpatRaster",
                 aspect3 = "SpatRaster"))
  cat_df <- data.frame(ID = 1:6, category = context$get_impact_measures())
  expect_equal(sapply(incursion_impacts, terra::cats),
               list(aspect1 = cat_df, aspect2 = cat_df, aspect3 = cat_df))
  expect_equal(lapply(incursion_impacts,
                      function(l) l[region$get_indices()][,1]),
               expected_impacts)
  expected_impacts_i <- lapply(expected_impacts, as.numeric)
  expected_combined_impacts <- factor(
    cat_df$category[pmax(expected_impacts_i[[1]], expected_impacts_i[[2]],
                         expected_impacts_i[[3]])], cat_df$category)
  expect_silent(combined_impacts <- impacts$combined_impacts())
  expect_is(combined_impacts, "SpatRaster")
  expect_equal(terra::cats(combined_impacts)[[1]], cat_df)
  expect_equal(combined_impacts[region$get_indices()][,1],
               expected_combined_impacts)
})

test_that("calculates incursion management costs", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"),
                     valuation_type = "ranking", impact_measures = 0:5)
  impact_classes <- c(aspect1 = 3, aspect2 = 2)
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 1*(template > 0.1 & template < 0.3),
                        aspect2 = 1*(template > 0.2 & template < 0.4))
  impact_locations <- (impact_layers$aspect1 > 0 | impact_layers$aspect2 > 0)
  mgmt_costs = template*0 + 300
  expect_silent(
    impacts <- ClassImpacts(context, region, incursion, impact_layers,
                            impact_classes, mgmt_costs = mgmt_costs))
  expect_named(impacts, c("incursion_impacts", "combined_impacts",
                          "incursion_mgmt_costs", "save_analysis"))
  expected_incursion_mgmt_costs <-
    ((mgmt_costs*impact_locations)[region$get_indices()][,1]*
        incursion$get_impact_incursion())
  expect_silent(incursion_mgmt_costs <- impacts$incursion_mgmt_costs())
  expect_is(incursion_mgmt_costs, "SpatRaster")
  expect_equal(incursion_mgmt_costs[region$get_indices()][,1],
               expected_incursion_mgmt_costs)
})
