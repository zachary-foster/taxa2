## Testing `taxonomy` class

library(taxa)
context("taxonomy")


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
unidentified <- taxon(name = taxon_name("unidentified"))

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
  expect_is(taxonomy(hierarchy()), "Taxonomy")
  expect_equal(length(taxonomy(hierarchy())$taxa), 0)
  expect_length(x$taxa, 0)
  expect_equal(dim(x$edge_list), c(0, 2))
})


test_that("Characters as inputs", {
  x <- taxonomy(c("a", "b", "c"), c("a", "d"))
  expect_length(x$taxa, 4)
  expect_equal(dim(x$edge_list), c(4, 2))
  expect_length(x$roots(), 1)

  # x <- taxonomy(list(c("a", "b", "c"), c("a", "d"))) # does not work yet
})


test_that("Finding roots", {
  x <- taxonomy(tiger, cougar, mole, tomato, potato,
                unidentified_plant, unidentified_animal)
  expect_equal(x$roots(), roots(x))

  # Index return type
  expect_type(x$roots(value = "taxon_indexes"), "integer")

  # Taxon ID return type
  expect_type(x$roots(value = "taxon_ids"), "character")
})


test_that("Finding branches", {
  x <- taxonomy(tiger, cougar, mole, tomato, potato,
                unidentified_plant, unidentified_animal)
  expect_equal(x$branches(), branches(x))

  # Index return type
  expect_type(x$branches(value = "taxon_indexes"), "integer")

  # Taxon ID return type
  expect_type(x$branches(value = "taxon_ids"), "character")

  # Expected output
  expect_equal(which(! x$is_root() & ! x$is_leaf()), x$branches())

})


test_that("Finding supertaxa", {
  x <- taxonomy(tiger, cougar, mole, tomato, potato,
                unidentified_plant, unidentified_animal)
  expect_equal(x$supertaxa(), supertaxa(x))

  # Index return type
  expect_type(x$supertaxa(value = "taxon_indexes")[[1]], "integer")
  expect_type(x$supertaxa(value = "taxon_indexes", simplify = TRUE), "integer")

  # Taxon ID return type
  expect_type(x$supertaxa(value = "taxon_ids")[[1]], "character")
  expect_type(x$supertaxa(value = "taxon_ids", simplify = TRUE), "character")

  # Recursion settings
  expect_equal(supertaxa(x, recursive = TRUE), supertaxa(x, recursive = -1))
  expect_equal(supertaxa(x, recursive = FALSE), supertaxa(x, recursive = 1))
  expect_equal(max(vapply(supertaxa(x, recursive = 2), length, numeric(1))), 2)

  # Duplicated inputs
  expect_equal(names(x$supertaxa(c(1, 2, 1, 1))), c("b", "c", "b", "b"))
})


test_that("Finding subtaxa", {
  x <- taxonomy(tiger, cougar, mole, tomato, potato,
                unidentified_plant, unidentified_animal)
  expect_equal(x$subtaxa(), subtaxa(x))

  # Index return type
  expect_type(x$subtaxa(value = "taxon_indexes")[[1]], "integer")
  expect_type(x$subtaxa(value = "taxon_indexes", simplify = TRUE), "integer")

  # Taxon ID return type
  expect_type(x$subtaxa(value = "taxon_ids")[[1]], "character")
  expect_type(x$subtaxa(value = "taxon_ids", simplify = TRUE), "character")

  # Recursion settings
  expect_equal(subtaxa(x, recursive = TRUE), subtaxa(x, recursive = -1))
  expect_equal(subtaxa(x, recursive = FALSE), subtaxa(x, recursive = 1))
  expect_equal(subtaxa(x, "b", recursive = 2, simplify = TRUE),
               subtaxa(x, subtaxa(x, "b", recursive = FALSE, simplify = TRUE),
                       recursive = FALSE, simplify = TRUE, include_input = TRUE))

  # Edge cases
  expect_equal(subtaxa(x, subset = rep(FALSE, length(x$taxa))), list())
  expect_equal(subtaxa(x, subset = rep(FALSE, length(x$taxa)), simplify = TRUE),
               character(0))
})


test_that("Finding stems", {
  x <- taxonomy(tiger, cougar, mole, tomato, potato,
                unidentified_plant, unidentified_animal)
  expect_equal(x$stems(), stems(x))

  # Index return type
  expect_type(x$stems(value = "taxon_indexes")[[1]], "integer")
  expect_type(x$stems(value = "taxon_indexes", simplify = TRUE), "integer")

  # Taxon ID return type
  expect_type(x$stems(value = "taxon_ids")[[1]], "character")
  expect_type(x$stems(value = "taxon_ids", simplify = TRUE), "character")
})


test_that("Finding leaves", {
  x <- taxonomy(tiger, cougar, mole, tomato, potato,
                unidentified_plant, unidentified_animal)
  expect_equal(x$leaves(), leaves(x))

  # Index return type
  expect_type(x$leaves(value = "taxon_indexes"), "integer")

  # Taxon ID return type
  expect_type(x$leaves(value = "taxon_ids"), "character")

})

test_that("Filtering taxa", {
  x <- taxonomy(tiger, cougar, mole, tomato, potato,
                unidentified_plant, unidentified_animal)

  result <- filter_taxa(x, taxon_names == "Solanum")
  expect_equal(result$taxon_names(), c("l" = "Solanum"))
  expect_warning(filter_taxa(x, taxon_names == "Solanum", drop_obs = FALSE))
  expect_warning(filter_taxa(x, taxon_names == "Solanum", reassign_obs = TRUE))

  # Check that filtering does not change order of taxa
  result <- filter_taxa(x, taxon_names != "tuberosum")
  expected_names <- x$taxon_names()
  expected_names <- expected_names[expected_names != "tuberosum"]
  expect_true(all(expected_names == result$taxon_names()))

  result <- filter_taxa(x, taxon_names == "Solanum", subtaxa = TRUE, invert = TRUE)
  expected_names <- x$taxon_names()
  expected_names <- expected_names[! expected_names %in% c("Solanum", "lycopersicum", "tuberosum")]
  expect_true(all(expected_names == result$taxon_names()))

})


test_that("Sampling taxa",  {
  x <- taxonomy(tiger, cougar, mole, tomato, potato,
                unidentified_plant, unidentified_animal)

  result <- sample_n_taxa(x, size = 3)
  expect_equal(length(result$taxon_ids()), 3)
  expect_warning(sample_n_taxa(x, size = 3, obs_weight = 1))
  expect_warning(sample_n_taxa(x, size = 3, obs_target = 1))
})

test_that("Mapping vairables",  {
  x <- taxonomy(tiger, cougar, mole, tomato, potato,
                unidentified_plant, unidentified_animal)
  result <- x$map_data(taxon_names, taxon_ranks)
  expect_equivalent(result, x$taxon_ranks())
  expect_equivalent(names(result), x$taxon_names())
  expect_warning(map_data(x, taxon_names, c("e" = "A", "e" = "B")))
  expect_silent(map_data(x, taxon_names, c("e" = "A", "e" = "B"), warn = FALSE))
  expect_error(map_data(x, taxon_names, 1:10))
})


test_that("dots and .list return the same output", {
  expect_equal(taxonomy(tiger, cougar, mole, tomato, potato),
               taxonomy(.list = list(tiger, cougar, mole, tomato, potato)))
})
