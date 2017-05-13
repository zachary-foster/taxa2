context("taxa")

x <- taxon(
  name = taxon_name("Poa annua"),
  rank = taxon_rank("species"),
  id = taxon_id(93036)
)

test_that("taxa works", {
  aa <- taxa_(x, x, x)

  expect_is(aa, "taxa")
  expect_is(unlist(aa), "list")
  expect_is(aa[[1]], "Taxon")
  expect_is(aa[[2]], "Taxon")
  expect_is(aa[[3]], "Taxon")
})

test_that("taxa - empty", {
  aa <- taxa_()

  expect_is(aa, "taxa")
  expect_is(unclass(aa), "list")
  expect_equal(length(aa), 0)
})

test_that("taxa fails well", {
  expect_error(taxa_(5), "all inputs to 'taxa_' must be of class 'Taxon'")
  expect_error(taxa_(mtcars), "all inputs to 'taxa_' must be of class 'Taxon'")
  expect_error(taxa_(4, x, "adff"),
               "all inputs to 'taxa_' must be of class 'Taxon'")
})
