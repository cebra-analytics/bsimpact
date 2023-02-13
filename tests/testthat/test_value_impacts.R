context("ValueImpacts")

test_that("initializes with parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"))
  context_alt <- Context("My species", impact_scope = c("aspect1", "aspect2"),
                         valuation_type = "ranking")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 1*(template > 0.1 & template < 0.3),
                        aspect2 = 1*(template > 0.2 & template < 0.4))
  loss_rates = c(aspect1 = 0.3, aspect2 = 0.4)
  expect_error(ValueImpacts(context_alt, region, incursion, impact_layers,
                            loss_rates),
               paste("Context is inappropriately configured for value-based",
                     "impact analysis with 'ranking' valuation or invalid",
                     "measure(s)."), fixed = TRUE)
  expect_error(ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = loss_rates + 1),
               paste("Loss rates must be numeric, >= 0, <= 1, and named",
                     "consistently with the context impact scope."))
  expect_error(ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = 0.3),
               paste("Loss rates must be numeric, >= 0, <= 1, and named",
                     "consistently with the context impact scope."))
  expect_error(ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = c(a = 0.3, b = 0.4)),
               paste("Loss rates must be numeric, >= 0, <= 1, and named",
                     "consistently with the context impact scope."))
  expect_message(
    impacts <- ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = c(0.3, 0.4)),
    paste("Unnamed loss rates assumed to be in order consistent with the",
          "context impact scope."))
  expect_silent(
    impacts <- ValueImpacts(context, region, incursion,
                            impact_layers, loss_rates = loss_rates))
  expect_is(impacts, "ValueImpacts")
  expect_s3_class(impacts, "ImpactAnalysis")
  expect_named(impacts, c("incursion_impacts", "combined_impacts"))
})

test_that("calculates individual and combined incursion impacts", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"))
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 1*(template > 0.1 & template < 0.3),
                        aspect2 = 1*(template > 0.2 & template < 0.4))
  loss_rates = c(aspect1 = 0.3, aspect2 = 0.4)
  expect_silent(
    impacts <- ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = loss_rates))
  expected_impacts <-
    lapply(list(aspect1 = "aspect1", aspect2 = "aspect2"), function(a) {
      (impact_layers[[a]][region$get_indices()][,1]*loss_rates[a]*
         incursion$get_impact_incursion())})
  expect_silent(incursion_impacts <- impacts$incursion_impacts())
  expect_equal(sapply(incursion_impacts, class),
               c(aspect1 = "SpatRaster", aspect2 = "SpatRaster"))
  expect_equal(lapply(incursion_impacts,
                      function(l) l[region$get_indices()][,1]),
               expected_impacts)
  expected_combined_impacts <- expected_impacts[[1]] + expected_impacts[[2]]
  expect_silent(combined_impacts <- impacts$combined_impacts())
  expect_is(combined_impacts, "SpatRaster")
  expect_equal(combined_impacts[region$get_indices()][,1],
               expected_combined_impacts)
  expect_silent(impacts <- ValueImpacts(
    context, region, incursion, impact_layers, loss_rates = loss_rates,
    combine_function = function(l) rowMeans(as.data.frame(l))))
  expected_combined_impacts <- rowMeans(as.data.frame(expected_impacts))
  expect_silent(combined_impacts <- impacts$combined_impacts())
  expect_is(combined_impacts, "SpatRaster")
  expect_equal(combined_impacts[region$get_indices()][,1],
               expected_combined_impacts)
})

test_that("calculates incursion management and total costs", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"))
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 1*(template > 0.1 & template < 0.3),
                        aspect2 = 1*(template > 0.2 & template < 0.4))
  impact_locations <- (impact_layers$aspect1 > 0 | impact_layers$aspect2 > 0)
  loss_rates = c(aspect1 = 0.3, aspect2 = 0.4)
  mgmt_costs = template*0 + 300
  expect_silent(
    impacts <- ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = loss_rates, mgmt_costs = mgmt_costs))
  expect_named(impacts, c("incursion_impacts", "combined_impacts",
                          "incursion_mgmt_costs", "total_costs" ))
  expected_incursion_mgmt_costs <-
    ((mgmt_costs*impact_locations)[region$get_indices()][,1]*
       incursion$get_impact_incursion())
  expect_silent(incursion_mgmt_costs <- impacts$incursion_mgmt_costs())
  expect_is(incursion_mgmt_costs, "SpatRaster")
  expect_equal(incursion_mgmt_costs[region$get_indices()][,1],
               expected_incursion_mgmt_costs)
  expected_total_costs <-
    (impacts$combined_impacts()[region$get_indices()][,1] +
       expected_incursion_mgmt_costs)
  expect_silent(total_costs <- impacts$total_costs())
  expect_is(total_costs, "SpatRaster")
  expect_equal(total_costs[region$get_indices()][,1], expected_total_costs)
})
