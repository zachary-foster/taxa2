## Testing `taxon_authority` class

library(taxa)
library(testthat)
context("taxon_authority")


test_that("Constructor", {
  # Basic
  x <- taxon_authority(c('A', 'B', 'C'))
  expect_equal(as.character(x), c('A', 'B', 'C'))
  expect_true(is_taxon_authority(x))
})
