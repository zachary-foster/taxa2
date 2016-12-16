## Testing `hierarchy` class

library(taxa)
context("Testing the `hierarchy` object")

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
  expect_equal(hierarchy(plantae, solanaceae, solanum, lycopersicum),
               hierarchy("Plantae", "Solanaceae", "Solanum", "lycopersicum"))
})
