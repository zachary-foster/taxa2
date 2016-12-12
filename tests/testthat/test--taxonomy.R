## Testing `taxonomy` class

library(taxa)
context("Testing the `taxonomy` object")

## Creating test data
notoryctidae <- taxon(
  name = taxon_name("Notoryctidae"),
  rank = taxon_rank("family"),
  id = taxon_id(4479)
)
notoryctes <- taxon(
  name = taxon_name("Notoryctes"),
  rank = taxon_rank("genus"),
  id = taxon_id(4544)
)
typhlops <- taxon(
  name = taxon_name("typhlops"),
  rank = taxon_rank("species"),
  id = taxon_id(93036)
)
mammalia <- taxon(
  name = taxon_name("Mammalia"),
  rank = taxon_rank("class"),
  id = taxon_id(9681)
)
felidae <- taxon(
  name = taxon_name("Felidae"),
  rank = taxon_rank("family"),
  id = taxon_id(9681)
)
puma <- taxon(
  name = taxon_name("Puma"),
  rank = taxon_rank("genus"),
  id = taxon_id(146712)
)
concolor <- taxon(
  name = taxon_name("concolor"),
  rank = taxon_rank("species"),
  id = taxon_id(9696)
)
panthera <- taxon(
  name = taxon_name("Panthera"),
  rank = taxon_rank("genus"),
  id = taxon_id(146712)
)
tigris <- taxon(
  name = taxon_name("tigris"),
  rank = taxon_rank("species"),
  id = taxon_id(9696)
)
plantae <- taxon(
  name = taxon_name("Plantae"),
  rank = taxon_rank("kingdom"),
  id = taxon_id(33090)
)
solanaceae <- taxon(
  name = taxon_name("Solanaceae"),
  rank = taxon_rank("family"),
  id = taxon_id(4070)
)
solanum <- taxon(
  name = taxon_name("Solanum"),
  rank = taxon_rank("genus"),
  id = taxon_id(4107)
)
lycopersicum <- taxon(
  name = taxon_name("lycopersicum"),
  rank = taxon_rank("species"),
  id = taxon_id(49274)
)
tuberosum <- taxon(
  name = taxon_name("tuberosum"),
  rank = taxon_rank("species"),
  id = taxon_id(4113)
)
unidentified <- taxon(
  name = taxon_name("unidentified"),
  rank = taxon_rank("species"),
  id = taxon_id(0)
)

tiger <- hierarchy(mammalia, felidae, panthera, tigris)
cougar <- hierarchy(mammalia, felidae, puma, concolor)
mole <- hierarchy(mammalia, notoryctidae, notoryctes, typhlops)
tomato <- hierarchy(plantae, solanaceae, solanum, lycopersicum)
potato <- hierarchy(plantae, solanaceae, solanum, tuberosum)
potato_partial <- hierarchy(solanaceae, solanum, tuberosum)
unidentified_animal <- hierarchy(mammalia, unidentified)
unidentified_plant <- hierarchy(plantae, unidentified)

test_that("Simple usage", {
  x <- taxonomy(tiger, cougar, mole)
  expect_length(x$taxa, 9)
  expect_equal(dim(x$edge_list), c(9, 2))
  expect_length(x$roots(), 1)
})


test_that("Multiple roots", {
  x <- taxonomy(tiger, cougar, mole, tomato, potato)
  expect_length(x$taxa, 14)
  expect_equal(dim(x$edge_list), c(14, 2))
  expect_length(x$roots(), 2)
})


test_that("Hierarchies of different lengths", {
  x <- taxonomy(tiger, unidentified_animal)
  expect_length(x$taxa, 5)
  expect_equal(dim(x$edge_list), c(5, 2))
  expect_length(x$roots(), 1)
})


test_that("Same taxon name, different lineage", {
  x <- taxonomy(unidentified_plant, unidentified_animal)
  expect_length(x$taxa, 4)
  expect_equal(dim(x$edge_list), c(4, 2))
  expect_length(x$roots(), 2)
  expect_equal(sum(sapply(x$taxa, function(x) x$name$name) == "unidentified"), 2)
})


test_that("Edge cases", {
  x <- taxonomy()
  expect_length(x$taxa, 0)
  expect_equal(dim(x$edge_list), c(0, 2))
  x <- taxonomy(hierarchy())
  expect_length(x$taxa, 0)
  expect_equal(dim(x$edge_list), c(0, 2))
})
