## Testing `taxon` class

library(taxa)
context("Testing the `taxon` object")


test_that("Characters as inputs", {
  expect_equal(taxon(name = taxon_name("Solanum")), taxon("Solanum"))
})
