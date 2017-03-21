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
  expect_that(all(vapply(result, class,
                         character(1)) == "integer"))
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