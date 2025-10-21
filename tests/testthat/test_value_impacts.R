context("ValueImpacts")

test_that("initializes with parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"))
  context_alt <- Context("My species", impact_scope = c("aspect1", "aspect2"),
                         valuation_type = "ranking")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4))
  loss_rates = c(aspect1 = 0.3, aspect2 = 0.4)
  discount_rates = c(aspect1 = 0.04, aspect2 = 0.05)
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
  expect_error(ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = loss_rates, is_dynamic = 3),
               "The dynamic indicator must be logical TRUE or FALSE.")
  expect_error(ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = loss_rates,
                            discount_rates = c(a = 0.05)),
               paste("Discount rates must be numeric, >= 0, <= 1, and named",
                     "consistently with the context impact scope."))
  expect_error(ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = loss_rates,
                            discount_rates = c(a = 0.04, b = 0.05)),
               paste("Discount rates must be numeric, >= 0, <= 1, and named",
                     "consistently with the context impact scope."))
  expect_message(
    impacts <- ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = loss_rates,
                            discount_rates = c(0.04, 0.05)),
    paste("Unnamed discount rates assumed to be in order consistent with the",
          "context impact scope."))
  expect_silent(
    impacts <- ValueImpacts(context, region, incursion,
                            impact_layers, loss_rates = loss_rates,
                            discount_rates = discount_rates))
  expect_is(impacts, "ValueImpacts")
  expect_s3_class(impacts, "ImpactAnalysis")
  expect_named(impacts, c("get_context", "get_incursion", "get_id", "set_id",
                          "incursion_impacts", "combined_impacts",
                          "save_analysis"))
  expect_is(impacts$get_context(), "Context")
  expect_is(impacts$get_incursion(), "Incursion")
  expect_silent(impacts <- ValueImpacts(context, region, incursion,
                                        impact_layers, loss_rates = loss_rates,
                                        combine_function = "none"))
  expect_named(impacts, c("get_context", "get_incursion", "get_id", "set_id",
                          "incursion_impacts", "save_analysis"))
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"),
                     valuation_type = "non-monetary",
                     impact_measures = c("$", "HCAS"))
  expect_error(impacts <- ValueImpacts(context, region, incursion,
                                       impact_layers, loss_rates = loss_rates,
                                       combine_function = "sum"),
               paste("Cannot combine impact aspects that have different",
                     "impact measures."))
  expect_silent(impacts <- ValueImpacts(context, region, incursion,
                                        impact_layers, loss_rates = loss_rates,
                                        combine_function = "none"))
})

test_that("calculates individual and combined incursion impacts", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"))
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4))
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
  expect_silent(
    impacts <- ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = loss_rates))
  expect_silent(incursion_impacts <- impacts$incursion_impacts(raw = TRUE))
  expect_equal(sapply(incursion_impacts, class),
               c(aspect1 = "numeric", aspect2 = "numeric"))
  expect_equal(incursion_impacts, expected_impacts)
  expect_silent(combined_impacts <- impacts$combined_impacts(raw = TRUE))
  expect_is(combined_impacts, "numeric")
  expect_equal(combined_impacts, expected_combined_impacts)
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
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4))
  impact_locations <- (impact_layers$aspect1 > 0 | impact_layers$aspect2 > 0)
  loss_rates = c(aspect1 = 0.3, aspect2 = 0.4)
  mgmt_costs = template*0 + 300
  expect_silent(
    impacts <- ValueImpacts(context, region, incursion, impact_layers,
                            loss_rates = loss_rates, mgmt_costs = mgmt_costs))
  expect_named(impacts, c("get_context", "get_incursion", "get_id", "set_id",
                          "incursion_impacts", "combined_impacts",
                          "incursion_mgmt_costs", "save_analysis",
                          "total_costs"))
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
  expect_silent(impacts <- ValueImpacts(context, region, incursion,
                                        impact_layers, loss_rates = loss_rates,
                                        mgmt_costs = mgmt_costs,
                                        combine_function = "none"))
  expect_named(impacts, c("get_context", "get_incursion", "get_id", "set_id",
                          "incursion_impacts", "incursion_mgmt_costs",
                          "save_analysis"))
  expect_silent(impacts <- ValueImpacts(context, region, incursion,
                                        impact_layers[1],
                                        loss_rates = loss_rates[1],
                                        mgmt_costs = mgmt_costs,
                                        combine_function = "none"))
  expect_named(impacts, c("get_context", "get_incursion", "get_id", "set_id",
                          "incursion_impacts", "incursion_mgmt_costs",
                          "save_analysis", "total_costs"))
  expected_incursion_mgmt_costs <-
    ((mgmt_costs*(impact_layers$aspect1 > 0))[region$get_indices()][,1]*
       incursion$get_impact_incursion())
  expected_total_costs <-
    (impacts$incursion_impacts()[[1]][region$get_indices()][,1] +
       expected_incursion_mgmt_costs)
  expect_silent(total_costs <- impacts$total_costs())
  expect_is(total_costs, "SpatRaster")
  expect_equal(total_costs[region$get_indices()][,1], expected_total_costs)
})

test_that("applies discounts to incursion impacts", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"))
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4))
  loss_rates = c(aspect1 = 0.3, aspect2 = 0.4)
  discount_rates = c(aspect1 = 0.04, aspect2 = 0.05)
  expect_silent(impacts <- ValueImpacts(context, region, incursion,
                                        impact_layers,
                                        loss_rates = loss_rates,
                                        discount_rates = discount_rates))
  expected_impacts <-
    lapply(list(aspect1 = "aspect1", aspect2 = "aspect2"), function(a) {
      (impact_layers[[a]][region$get_indices()][,1]*loss_rates[a]/
         ((1 + discount_rates[a])^3)*incursion$get_impact_incursion())})
  expect_silent(incursion_impacts <- impacts$incursion_impacts(raw = TRUE,
                                                               time_int = 3))
  expect_equal(incursion_impacts, expected_impacts)
  expected_combined_impacts <- expected_impacts[[1]] + expected_impacts[[2]]
  expect_silent(combined_impacts <- impacts$combined_impacts(raw = TRUE))
  expect_equal(combined_impacts, expected_combined_impacts)
  expect_silent(impacts <- ValueImpacts(context, region, incursion,
                                        impact_layers,
                                        loss_rates = loss_rates,
                                        discount_rates = 0.05))
  expected_impacts <-
    lapply(list(aspect1 = "aspect1", aspect2 = "aspect2"), function(a) {
      (impact_layers[[a]][region$get_indices()][,1]*loss_rates[a]/
         ((1 + 0.05)^3)*incursion$get_impact_incursion())})
  expect_silent(incursion_impacts <- impacts$incursion_impacts(raw = TRUE,
                                                               time_int = 3))
  expect_equal(incursion_impacts, expected_impacts)
})

test_that("applies recovery delay to prolong impacts", {
  TEST_DIRECTORY <- test_path("test_inputs")
  context <- Context("My species", impact_scope = c("aspect1", "aspect2"))
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- Region(template*0)
  incursion <- Incursion(template, region)
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.5),
                        aspect2 = 200*(template > 0.4 & template < 0.8))
  loss_rates = c(aspect1 = 0.3, aspect2 = 0.4)
  expect_silent(impacts <- ValueImpacts(context, region, incursion,
                                        impact_layers,
                                        loss_rates = loss_rates))
  x <- incursion$get_impact_incursion()
  impacts$set_id(3)
  attr(x, "recovery_delay") <- list(NULL, NULL, rep(0, region$get_locations()))
  attr(x, "recovery_delay")[[3]][which(x == 0)[1:100]] <- 1
  incursion$set_values(x)
  x_with_delay <- +(x > 0)
  x_with_delay[which(x == 0)[1:100]] <- 1
  expected_impacts <-
    lapply(list(aspect1 = "aspect1", aspect2 = "aspect2"), function(a) {
      (impact_layers[[a]][region$get_indices()][,1]*loss_rates[a]*
         x_with_delay)})
  expect_equal(impacts$incursion_impacts(raw = TRUE), expected_impacts)
  # density-based impacts
  template_vect <- template[region$get_indices()][,1]
  idx <- 5901:6000
  incursion <- Incursion(template*0, region, type = "density")
  aspects <- list(aspect1 = "aspect1", aspect2 = "aspect2")
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.4),
                        aspect2 = 200*(template > 0.3))
  expect_silent(impacts <- ValueImpacts(context, region, incursion,
                                        impact_layers,
                                        loss_rates = loss_rates))
  expect_silent(impacts$set_id(2))
  n_density <- n <- rep(0, region$get_locations())
  n[idx] <- round(runif(100, 1, 10))
  dens_idx <- idx[which(template_vect[idx] > 0)]
  n_density[dens_idx] <- pmin(n[dens_idx]/(template_vect[dens_idx]*50), 1)
  expected_impacts <- lapply(aspects, function(l) {
    n_density*impact_layers[[l]][region$get_indices()][,1]*loss_rates[l]
  })
  x <- n_density
  incursion$set_values(x) # 0
  expect_equal(impacts$incursion_impacts(raw = TRUE), expected_impacts)
  attr(x, "recovery_delay") <- list(3, 2)
  attr(attr(x, "recovery_delay"), "incursions") <- list(n_density)
  x[idx[1:10]] <- 0
  x[idx[11:20]] <- x[idx[11:20]]*0.6
  mask1 <- +(x > 0)
  mask1[idx[11:20]] <- mask1[idx[11:20]]*0.6
  incursion$set_values(x) # 1
  expect_equal(impacts$incursion_impacts(raw = TRUE), expected_impacts)
  attr(attr(x, "recovery_delay"), "incursions") <-
    list(mask1*n_density, n_density)
  x[idx[11:30]] <- 0
  mask2 <- +(x > 0)
  incursion$set_values(x) # 2
  expect_equal(impacts$incursion_impacts(raw = TRUE), expected_impacts)
  attr(attr(x, "recovery_delay"), "incursions") <-
    list(mask2*n_density, mask1*n_density, n_density)
  mask3 <- +(x > 0)
  expected_impacts <- lapply(expected_impacts, function(impact) {
    impact[idx[1:10]] <- 0
    impact[idx[11:20]] <- impact[idx[11:20]]*0.6
    impact
  })
  incursion$set_values(x) # 3
  expect_equal(impacts$incursion_impacts(raw = TRUE), expected_impacts)
  attr(attr(x, "recovery_delay"), "incursions") <-
    list(mask3*n_density, mask2*n_density, mask1*n_density)
  x[idx[21:30]] <- n_density[idx[21:30]]
  expected_impacts <- lapply(expected_impacts, function(impact) {
    impact[idx[11:20]] <- 0
    impact
  })
  incursion$set_values(x) # 4
  expect_equal(impacts$incursion_impacts(raw = TRUE), expected_impacts)
  # spatially implicit area-based impacts
  region <- Region()
  incursion <- Incursion(0, region, type = "area")
  impact_layers <- list(aspect1 = 100, aspect2 = 200)
  expect_silent(impacts <- ValueImpacts(context, region, incursion,
                                        impact_layers,
                                        loss_rates = loss_rates))
  expect_silent(impacts$set_id(3))
  x <- 50
  incursion$set_values(x) # 0
  expect_equal(impacts$incursion_impacts(raw = TRUE),
               list(aspect1 = 100*50*0.3, aspect2 = 200*50*0.4))
  x[1] <- 30
  attr(x, "recovery_delay") <- list(NULL, NULL, 2)
  attr(attr(x, "recovery_delay"), "incursions") <- 50
  incursion$set_values(x) # 1
  expect_equal(impacts$incursion_impacts(raw = TRUE),
               list(aspect1 = 100*50*0.3, aspect2 = 200*50*0.4))
  x[1] <- 0
  attr(attr(x, "recovery_delay"), "incursions") <- c(30, 50)
  incursion$set_values(x) # 2
  expect_equal(impacts$incursion_impacts(raw = TRUE),
               list(aspect1 = 100*50*0.3, aspect2 = 200*50*0.4))
  attr(attr(x, "recovery_delay"), "incursions") <- c(0, 30, 50)
  incursion$set_values(x) # 3
  expect_equal(impacts$incursion_impacts(raw = TRUE),
               list(aspect1 = 100*30*0.3, aspect2 = 200*30*0.4))
  attr(attr(x, "recovery_delay"), "incursions") <- c(0, 0, 30, 50)
  incursion$set_values(x) # 4
  expect_equal(impacts$incursion_impacts(raw = TRUE),
               list(aspect1 = 0, aspect2 = 0))
})
