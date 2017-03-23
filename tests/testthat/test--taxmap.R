## Testing `taxmap` class

library(taxa)
context("Testing the `taxmap` object")

### NSE helpers

#### all_names

test_that("Names of table col names are accessible by NSE", {
  expected_col_names <- colnames(ex_taxmap$data$info)
  expected_col_names <- expected_col_names[expected_col_names != "taxon_id"]
  expect_true(all(expected_col_names %in% ex_taxmap$all_names()))
  expect_false(any(expected_col_names %in% ex_taxmap$all_names(tables = FALSE)))
})

test_that("Names of functions are accessible by NSE", {
  expected_funcs <- names(ex_taxmap$funcs)
  expect_true(all(expected_funcs %in% ex_taxmap$all_names()))
  expect_false(any(expected_funcs %in% ex_taxmap$all_names(funcs = FALSE)))
})

test_that("Names of functions are accessible by NSE", {
  expected_others <-
    names(ex_taxmap$data)[sapply(ex_taxmap$data,
                                 function(x) ! "data.frame" %in% class(x))]
  expect_true(all(expected_others %in% ex_taxmap$all_names()))
  expect_false(any(expected_others %in% ex_taxmap$all_names(others = FALSE)))
})

test_that("Names of built-in functions are accessible by NSE", {
  expected_builtin <- c("taxon_names", "taxon_ids", "n_supertaxa", "n_subtaxa",
                        "n_subtaxa_1")
  expect_true(all(expected_builtin %in% ex_taxmap$all_names()))
  expect_false(any(expected_builtin %in%
                     ex_taxmap$all_names(builtin_funcs = FALSE)))
})

test_that("Duplicate names give a warning", {
  x = ex_taxmap
  x$data$n_legs = 1:10
  expect_warning(x$all_names(warn = T),
                 "The following names are used more than once: n_legs")
  expect_silent(x$all_names(warn = F))
})

#### names_used

test_that("Names in basic expressions can be found by NSE", {
  expect_true(all(c("n_subtaxa", "n_legs", "taxon_ids")
                  %in% ex_taxmap$names_used(n_legs + n_subtaxa, taxon_ids + 19)))
})

test_that("Names in complex expressions can be found by NSE", {
  expect_true(all(c("n_subtaxa", "n_legs", "taxon_ids", "dangerous", "reaction") %in%
                    ex_taxmap$names_used((((((n_legs))))),
                                         function(x) length(taxon_ids) + x,
                                         {{n_subtaxa}},
                                         taxon_ids[n_subtaxa[dangerous]],
                                         reaction(n_subtaxa))))
})

test_that("Names in invalid expressions can be found by NSE", {
  expect_true(all(c("n_subtaxa")
                  %in% ex_taxmap$names_used(not_a_variable == n_subtaxa,
                                            aslkadsldsa)))
})

#### get_data

test_that("NSE values can be found", {
  expect_identical(ex_taxmap$get_data(c("n_subtaxa", "n_legs", "reaction")),
                   list(n_subtaxa = ex_taxmap$n_subtaxa(),
                        n_legs = ex_taxmap$data$info$n_legs,
                        reaction = ex_taxmap$funcs$reaction(ex_taxmap)))
})



### Mapping functions

#### obs

test_that("Mapping between table observations and the edge list works", {
  result <- ex_taxmap$obs("info")
  expect_true(all(sapply(result, class) == "integer"))
  expect_identical(names(result), ex_taxmap$taxon_ids())
  expect_identical(result[["1"]], 1:4)
})

test_that("Mapping between a subset of observations and the edge list works", {
  expect_identical(ex_taxmap$obs("info", subset = "1"), list("1" = 1:4))
  expect_identical(ex_taxmap$obs("info", subset = 1), list("1" = 1:4))
})

test_that("Mapping non-recursivly between observations and the edge list works", {
  result <- ex_taxmap$obs("info", recursive = FALSE)
  expect_true(all(sapply(result[roots(ex_taxmap)], length) == 0))
  expect_equal(result[["17"]], 6)
})

test_that("Mapping simplification between observations and the edge list works", {
  expect_equal(ex_taxmap$obs("info", simplify = TRUE), 1:6)
})



### Dplyr analogs

#### filter_taxa

test_that("Default taxon filtering works", {
  result <- filter_taxa(ex_taxmap, taxon_names == "Solanum")
  expect_equal(result$taxon_names(), c("11" = "Solanum"))
  expect_equal(as.character(result$data$info$name), c("tomato", "potato"))
  expect_true(length(result$data$phylopic_ids) == 2)
})

test_that("Subtaxa can be included when filtering taxa", {
  result <- filter_taxa(ex_taxmap, taxon_names == "Solanum", subtaxa = TRUE)
  expect_equivalent(result$taxon_names(),
                    c("Solanum", "lycopersicum", "tuberosum"))
})

test_that("Supertaxa can be included when filtering taxa", {
  result <- filter_taxa(ex_taxmap, taxon_names == "Solanum", supertaxa = TRUE)
  expect_equivalent(result$taxon_names(),
                    c("Solanum", "Solanaceae", "Plantae"))
})

test_that("Observations can be preserved when filtering taxa", {
  result <- filter_taxa(ex_taxmap, taxon_names == "Solanum", reassign_obs = FALSE)
  expect_equal(nrow(result$data$info), 0)
  result <- filter_taxa(ex_taxmap, taxon_names == "tuberosum", reassign_obs = FALSE)
  expect_equivalent(result$taxon_names(), "tuberosum")
})

test_that("Taxon ids can be preserved when filtering taxa", {
  result <- filter_taxa(ex_taxmap, taxon_names != "Solanum", reassign_taxa = FALSE)
  expect_true(all(c("lycopersicum", "tuberosum") %in% result$roots(return_type = "name")))
})

test_that("The selection of taxa to be filtered can be inverted", {
  result <- filter_taxa(ex_taxmap, taxon_names == "Solanum", subtaxa = TRUE, invert = TRUE)
  expect_true(all(! c("tuberosum", "lycopersicum", "Solanum") %in% taxon_names(result)))
  expect_true(all(c("Mammalia", "Plantae", "sapiens") %in% taxon_names(result)))
})

test_that("Edge cases return reasonable outputs", {
  expect_equal(filter_taxa(ex_taxmap), ex_taxmap)
})


#### filter_obs

test_that("Default observation filtering works", {
  result <- filter_obs(ex_taxmap, "info", n_legs == 2)
  expect_equivalent(as.character(result$data$info$name), "human")
})

test_that("Removing taxa when filtering observations work", {
  result <- filter_obs(ex_taxmap, "info", n_legs == 2, unobserved = FALSE)
  expect_equivalent(as.character(result$data$info$name), "human")
  expect_equivalent(result$taxon_names(),
                    c("Mammalia", "Hominidae", "homo", "sapiens"))
})

test_that("Edge cases return reasonable outputs", {
  expect_equal(filter_obs(ex_taxmap, "info"), ex_taxmap)
  expect_error(filter_obs(ex_taxmap, "not_valid",
                          "not the name of a data set. Valid targets "))
})


#### select_obs

test_that("Default observation column subsetting works",  {
  result <- select_obs(ex_taxmap, "info", dangerous)
  expect_equal(colnames(result$data$info), c("taxon_id", "dangerous"))
})

test_that("Edge cases return reasonable outputs during observation column subsetting", {
  result <- select_obs(ex_taxmap, "info")
  expect_equal(colnames(result$data$info), c("taxon_id"))
  expect_error(select_obs(ex_taxmap, "not_valid"),
                          "not the name of a data set. Valid targets ")
  expect_error(select_obs(ex_taxmap), " missing, with no default")
})


#### mutate_obs

test_that("Observation column addition works",  {
  result <- mutate_obs(ex_taxmap, "info",
                       new_col = "new",
                       newer_col = paste0(new_col, "er"))
  expect_true(all(c("new_col", "newer_col") %in% colnames(result$data$info)))
})

test_that("Observation column replacement works",  {
  result <- mutate_obs(ex_taxmap, "info", name = "replacement")
  expect_true(all(result$data$info$name == "replacement"))
})

test_that("Edge cases for observation column addition",  {
  expect_equal(mutate_obs(ex_taxmap, "info"), ex_taxmap)
  expect_error(mutate_obs(ex_taxmap, "not_valid"),
               "not the name of a data set. Valid targets ")
})


#### transmute_obs

test_that("Observation column addition (transmute) works",  {
  result <- transmute_obs(ex_taxmap, "info",
                          new_col = paste("new", name),
                          newer_col = paste0(new_col, "!!"))
  expect_equal(c("taxon_id", "new_col", "newer_col"),
               colnames(result$data$info))
})

test_that("Edge cases for observation column addition (transmute) ",  {
  result <- transmute_obs(ex_taxmap, "info")
  expect_equal("taxon_id", colnames(result$data$info))
  expect_error(transmute_obs(ex_taxmap, "not_valid"),
               "not the name of a data set. Valid targets ")
})


#### arrange_obs

test_that("Sorting observations work",  {
  result <- arrange_obs(ex_taxmap, "info", dangerous, name)
  expect_equal(ex_taxmap$data$info$taxon_id[order(ex_taxmap$data$info$dangerous,
                                                  ex_taxmap$data$info$name)],
               result$data$info$taxon_id)
  result <- arrange_obs(ex_taxmap, "info", desc(dangerous), desc(name))
  expect_equal(ex_taxmap$data$info$taxon_id[order(ex_taxmap$data$info$dangerous,
                                                  ex_taxmap$data$info$name,
                                                  decreasing = TRUE)],
               result$data$info$taxon_id)
})

test_that("Edge cases during observation sorting works",  {
  expect_equal(arrange_obs(ex_taxmap, "info"), ex_taxmap)
  expect_error(arrange_obs(ex_taxmap, "not_valid"),
               "not the name of a data set. Valid targets ")
})


#### arrange_taxa

test_that("Sorting taxa work",  {
  expect_equal(arrange_taxa(ex_taxmap, taxon_ids)$taxon_ids(),
               sort(ex_taxmap$taxon_ids()))
  expect_equal(arrange_taxa(ex_taxmap, desc(taxon_ids))$taxon_ids(),
               sort(ex_taxmap$taxon_ids(), decreasing = TRUE))
})

test_that("Edge cases during observation sorting works",  {
  expect_equal(arrange_taxa(ex_taxmap), ex_taxmap)
})


#### sample_n_obs

test_that("Sampling observations works",  {
  result <- sample_n_obs(ex_taxmap, "info", size = 3)
  expect_equal(nrow(result$data$info), 3)
  result <- sample_n_obs(ex_taxmap, "info", size = 30, replace = TRUE)
  expect_equal(nrow(result$data$info), 30)
})

test_that("Edge cases during sampling observations",  {
  expect_error(sample_n_obs(ex_taxmap),
               "missing, with no default")
  expect_error(sample_n_obs(ex_taxmap, "not_valid"),
               "not the name of a data set. Valid targets ")
})


#### sample_n_taxa

test_that("Sampling observations works",  {
  result <- sample_n_taxa(ex_taxmap, size = 3)
  expect_equal(length(result$taxon_ids()), 3)
})

test_that("Edge cases during sampling observations",  {
  expect_error(sample_n_taxa(ex_taxmap),
               "missing, with no default")
  expect_error(sample_n_taxa(),
               "missing, with no default")
})

