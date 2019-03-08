library(taxa)
library(testthat)

context("taxa")

t1 <- taxon("Poa annua", "species", 93036)
t2 <- taxon("Homo sapiens", "species", 93036)

test_that("taxa - constructor", {
  # Empty
  expect_true("Taxa" %in% class(taxa()))
  expect_equal(length(taxa()), 0)

  # Taxon Objects
  x <- taxa(t1, t2)
  expect_true("Taxa" %in% class(x))
  expect_equal(length(x), 2)

  # Vectors
  expect_equal(taxa(taxon("a"), taxon("b")), taxa("a", "b"))
  expect_equal(taxa(taxon("1"), taxon("2")), taxa(1, 2))
  expect_equal(taxa(taxon("a"), taxon("b")), taxa(as.factor("a"), as.factor("b")))

  # TaxonName objects
  expect_equal(taxa(taxon("a"), taxon("b")), taxa(taxon_name("a"), taxon_name("b")))

  # Mixed
  expect_equal(taxa(taxon("a"), taxon("b"), taxon("c")), taxa("a", taxon_name("b"), taxon("c")))

  # List
  expect_equal(taxa("a", "b"), taxa(.list = list("a", "b")))

  # Bad inputs
  expect_error(taxa(mtcars))
  expect_error(taxa(c))
  expect_error(taxa(taxon("a"), .list = list(taxon("a"))),
               'Both `...` and `.list` were supplied')
})

test_that("taxa - print method", {
  expect_output(print(taxa()), "<taxa>")
  expect_output(print(taxa()), "no\\. taxa:  0")
  expect_output(print(taxa(t1)), "no\\. taxa:  1")
  expect_output(print(taxa(t1)), "Poa annua / species / 93036")
})

test_that("taxa - subsetting", {
  x <- taxa(t1, t2)

  # [ indexing
  expect_true("Taxa" %in% class(x[1]))
  expect_equal(x[1], taxa(t1))
  expect_equal(x[], taxa())
  expect_equal(x[0], taxa())
  expect_error(x[3], "Index out of bounds")
  expect_error(x[-3], "Index out of bounds")

  # [[ indexing
  expect_true("Taxon" %in% class(x[[1]]))
  expect_equal(x[[1]], t1)
  expect_error(x[[]])
  expect_error(x[[0]])
  expect_error(x[[3]], "Index out of bounds")
  expect_error(x[[-3]], "Index out of bounds")
})

test_that("taxa - replacing by index", {
  # [ indexing
  x <- taxa(t1, t2)
  x[1] <- "new"
  expect_equal(x, taxa("new", t2))

  x <- taxa(t1, t2)
  x[2] <- "new"
  expect_equal(x, taxa(t1, "new"))

  x <- taxa(t1, t2)
  x[-2] <- "new"
  expect_equal(x, taxa("new", t2))

  x <- taxa(t1, t2)
  x[c(2, 1)] <- c("a", "b")
  expect_equal(x, taxa("b", "a"))

  x <- taxa(t1, t2)
  x[c(2, 1)] <- taxa("a", "b")
  expect_equal(x, taxa("b", "a"))

  x <- taxa(t1, t2)
  x[] <- "new"
  expect_equal(x, taxa("new", "new"))

  x <- taxa(t1, t2)
  expect_error(x[3] <- "a", "Index out of bounds")
  expect_error(x[-3] <- "a", "Index out of bounds")

  # [[ indexing
  x <- taxa(t1, t2)
  x[[1]] <- "new"
  expect_equal(x, taxa("new", t2))

  x <- taxa(t1, t2)
  x[[2]] <- "new"
  expect_equal(x, taxa(t1, "new"))

  x <- taxa(t1, t2)
  x[[-2]] <- "new"
  expect_equal(x, taxa("new", t2))

  x <- taxa(t1, t2)
  expect_error(x[[3]] <- "a", "Index out of bounds")
  expect_error(x[[-3]] <- "a", "Index out of bounds")
  expect_error(x[[1:2]] <- c("a", "b"), "with more than one value")
  expect_error(x[[]] <- "a", "No index supplied")
})

test_that("taxa: taxon_name getters and setters", {

  # Can set the taxon names with a list of taxon_name objects

  # Can set the taxon names with a character/factor

  # Can set the taxon names with anything else that can be coerced into a character/object with a warning

  # Can get a list of taxon_names objects

  # Can coerce the list of taxon_name objects returned into a character/factor

  # Can get the character version of taxon names from S3 method

  # Can set the taxon names using a character with the S3 method

  # Can set the taxon names using a list of objects with the S3 method

})


test_that("taxa: taxon_id getters and setters", {

  # Can set the taxon ids with a list of taxon_id objects

  # Can set the taxon ids with a character/factor/nunmber

  # Can set the taxon ids with anything else that can be coerced into a character/object with a warning

  # Can get a list of taxon_id objects

  # Can coerce the list of taxon_id objects returned into a character/factor/number

  # Can get the character version of taxon ids from S3 method

  # Can set the taxon ids using a character with the S3 method

  # Can set the taxon ids using a list of objects with the S3 method

})


test_that("taxa: taxon_rank getters and setters", {

  # Can set the taxon ranks with a list of taxon_rank objects

  # Can set the taxon ranks with a character/factor/nunmber

  # Can set the taxon ranks with anything else that can be coerced into a character/object with a warning

  # Can get the list of taxon_rank object

  # Can coerce the list of taxon rank objects returned into a character/factor/number

  # Can get the character version of taxon ranks from S3 method

  # Can set the taxon ranks using a character with the S3 method

  # Can set the taxon ranks using a list of objects with the S3 method

  # Only valid ranks are accepted by setters if valid ranks are defined

})


test_that("taxa: authority getters and setters", {

  # Can set the authorities with a character/factor/nunmber

  # Can set the authorities with anything else that can be coerced into a character with a warning

  # Can get the authorities from the S3 method

  # Can set the authorities using a character with the S3 method

})

