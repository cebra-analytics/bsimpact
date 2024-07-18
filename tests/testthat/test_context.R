context("Context")

test_that("initializes with parameters", {
  expect_error(conteX <- Context("My species",
                                 impact_scope = c("aspect1", "aspect2"),
                                 impact_measures = c("$", "cents")),
               "Monetary impacts must use the same impact measure.")
  expect_silent(conteX <- Context("My species",
                                  impact_scope = c("aspect1", "aspect2")))
  expect_is(conteX, "Context")
  expect_equal(conteX$get_species_name(), "My species")
  expect_equal(conteX$get_species_type(), "pest")
  expect_equal(conteX$get_impact_types(),
               c("ecological", "social", "economic"))
  expect_equal(conteX$get_impact_scope(), c("aspect1", "aspect2"))
  expect_equal(conteX$get_valuation_type(), "monetary")
  expect_equal(conteX$get_impact_measures(), "$")
  expect_equal(conteX$get_mgmt_cost_unit(), "$")
  expect_error(conteX <- Context("My species",
                                 impact_scope = c("aspect1", "aspect2"),
                                 valuation_type = "non-monetary",
                                 impact_measures = c("$", "HCAS", "example")),
               paste("The number of non-monetary impact measures must be",
                     "consistent with the number of impact scope aspects."))
  expect_silent(conteX <- Context("My species",
                                  impact_scope = c("aspect1", "aspect2"),
                                  valuation_type = "non-monetary",
                                  impact_measures = c("$", "HCAS")))
  expect_equal(conteX$get_valuation_type(), "non-monetary")
  expect_equal(conteX$get_impact_measures(), c("$", "HCAS"))
})
