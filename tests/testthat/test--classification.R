context("classification")
library(taxa)


# Creating classification objects

test_that("classification objects can be created from character input", {
  x <- classification(c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo',
                        'Panthera tigris', 'Ursidae', 'Ursus', 'Ursus arctos'),
                      supertaxa = c(NA, 1, 2, 3, 3, 1, 6, 7),
                      instances = c(3, 4, 4, 5, 5, 6, 8, 8, 2, 5, 6, 2))
  expect_equal(length(x), 12)
  expect_equal(class(x)[1], 'taxa_classification')
})

test_that("classification objects can be created from factor input", {
  x <- classification(as.factor(c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo',
                                  'Panthera tigris', 'Ursidae', 'Ursus', 'Ursus arctos')),
                      supertaxa = c(NA, 1, 2, 3, 3, 1, 6, 7),
                      instances = c(3, 4, 4, 5, 5, 6, 8, 8, 2, 5, 6, 2))
  expect_equal(length(x), 12)
  expect_equal(class(x)[1], 'taxa_classification')
})

test_that("classification objects can be created with names", {
  x <- classification(c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo',
                        'Panthera tigris', 'Ursidae', 'Ursus', 'Ursus arctos'),
                      supertaxa = c(NA, 1, 2, 3, 3, 1, 6, 7),
                      instances = c(3, 4, 4, 5, 5, 6, 8, 8, 2, 5, 6, 2),
                      .names = letters[1:12])
  expect_equal(length(x), 12)
  expect_equal(class(x)[1], 'taxa_classification')
  expect_equal(names(x), letters[1:12])
})


# Subsetting taxonomy objects with `[`

test_that("taxonomy objects can be `[` subset by index", {
  x <- classification(c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo',
                        'Panthera tigris', 'Ursidae', 'Ursus', 'Ursus arctos'),
                      supertaxa = c(NA, 1, 2, 3, 3, 1, 6, 7),
                      instances = c(3, 4, 4, 5, 5, 6, 8, 8, 2, 5, 6, 2),
                      .names = letters[1:12])

  expect_equal(length(x[2:5]), 4)
  expect_equal(class(x[2:5])[1], 'taxa_classification')
  expect_equal(names(x[2:5]), letters[2:5])

  # taxonomy is subset too
  expect_equal(attr(x[2:5], 'taxonomy'),
               taxonomy(c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo','Panthera tigris'),
                        supertaxa = c(NA, 1, 2, 3, 3)))

})
