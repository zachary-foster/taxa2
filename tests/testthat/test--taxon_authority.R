## Testing `taxon_authority` class

library(taxa)
library(testthat)
context("taxon_authority")


test_that("Constructor", {
  x <- taxon_authority(c('Bowie, 2001', 'L., 1998', 'Billy 1678', 'Joe'),
                       citation = c('A', 'B', 'C', 'D'), extract_date = FALSE)
  expect_true(is_taxon_authority(x))
  expect_equal(tax_author(x), c('Bowie, 2001', 'L., 1998', 'Billy 1678', 'Joe'))

  x <- taxon_authority(c('Bowie, 2001', 'L., 1998', 'Billy 1678', 'Joe'),
                       citation = c('A', 'B', 'C', 'D'))
  expect_equal(tax_author(x), c("Bowie", "L.", "Billy", "Joe"))
  expect_equal(tax_date(x), c("2001", "1998", "1678", NA))
  expect_equal(tax_cite(x), c('A', 'B', 'C', 'D'))
})


test_that("Subsetting", {
  x <- taxon_authority(c('A', 'B', 'C'))
  expect_equal(length(x[1:2]), 2)
  expect_equal(as.character(x[[2]]), 'B')
  expect_equal(as.character(x[-2]), c('A', 'C'))
})
