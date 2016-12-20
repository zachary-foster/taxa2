## Testing `taxmap` class

library(taxa)
context("Testing the `taxmap` object")

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
  name = taxon_name("unidentified")
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

  abund <- data.frame(name = c("tiger", "cougar", "mole"),
                      count = 1:3)
  counts <- tibble::as_tibble(data.frame(name = c("T", "C", "M"),
                                         count = c(2, 3, 4)))
  a_list <- list("a", "b", "c", "a", "b", "c")
  a_vector <- 1:3

  x <- taxmap(tiger, cougar, mole, data = list(counts = counts, a_list = a_list, a_vector = a_vector, abund = abund))
  expect_length(x$taxa, 9)
  expect_equal(dim(x$edge_list), c(9, 2))
  expect_length(x$roots(), 1)
})


# TODO: data as single frame and unnamed list
