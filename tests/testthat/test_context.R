context("Context")

test_that("initializes with parameters", {
  expect_silent(conteX <- Context("My species",
                                  impact_scope = c("aspect1", "aspect2"),
                                  impact_measures = "$"))
  expect_is(conteX, "Context")
  expect_equal(conteX$get_species_name(), "My species")
  expect_equal(conteX$get_species_type(), "pest")
  expect_equal(conteX$get_impact_types(),
               c("ecological", "social", "economic"))
  expect_equal(conteX$get_impact_scope(), c("aspect1", "aspect2"))
  expect_equal(conteX$get_valuation_type(), "monetary")
  expect_equal(conteX$get_impact_measures(), "$")
})
