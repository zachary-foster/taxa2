context("hierarchy")

## Creating test data
plantae <- taxon(
  name = taxon_name("Plantae")
)
solanaceae <- taxon(
  name = taxon_name("Solanaceae")
)
solanum <- taxon(
  name = taxon_name("Solanum")
)
lycopersicum <- taxon(
  name = taxon_name("lycopersicum")
)


test_that("Characters as inputs", {
  aa <- hierarchy(plantae, solanaceae, solanum, lycopersicum)

  expect_is(aa, "Hierarchy")
  expect_is(aa$taxa, "list")
  expect_is(aa$taxa[[1]], "Taxon")
  expect_is(aa$print, "function")
  expect_equal(aa,
               hierarchy("Plantae", "Solanaceae", "Solanum", "lycopersicum"))
})


test_that("hierarchy - empty", {
  aa <- hierarchy()

  expect_is(aa, "Hierarchy")
  expect_null(aa$taxa)
  expect_null(aa$ranklist)
})



test_that("hierarchy fails well", {
  expect_error(
    hierarchy(4),
    "all inputs to 'hierarchy' must be of class 'Taxon' or 'character'")
  expect_error(
    hierarchy(solanum, 5),
    "all inputs to 'hierarchy' must be of class 'Taxon' or 'character'")
})
