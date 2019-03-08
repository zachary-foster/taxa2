context("taxa")

x <- taxon(
  name = taxon_name("Poa annua"),
  rank = taxon_rank("species"),
  id = taxon_id(93036)
)

test_that("taxa works", {
  aa <- taxa(x, x, x)

  expect_is(aa, "taxa")
  expect_is(unlist(aa), "list")
  expect_is(aa[[1]], "Taxon")
  expect_is(aa[[2]], "Taxon")
  expect_is(aa[[3]], "Taxon")
})

test_that("taxa - empty", {
  aa <- taxa()

  expect_is(aa, "taxa")
  expect_is(unclass(aa), "list")
  expect_equal(length(aa), 0)
})

test_that("taxa - print method", {
  # no inputs
  expect_output(print(taxa()), "<taxa>")
  expect_output(print(taxa()), "no\\. taxa:  0")
  expect_output(print(taxa(x)), "no\\. taxa:  1")
  expect_output(print(taxa(x)), "Poa annua / species / 93036")
})

test_that("taxa fails well", {
  expect_error(taxa(5), "all inputs to 'taxa' must be of class 'Taxon'")
  expect_error(taxa(mtcars), "all inputs to 'taxa' must be of class 'Taxon'")
  expect_error(taxa(4, x, "adff"),
               "all inputs to 'taxa' must be of class 'Taxon'")
})

test_that("dots and .list return the same output", {
  expect_equal(taxa(x, x, x), taxa(.list = list(x, x, x)))
  expect_error(taxa(taxon("a"), .list = list(taxon("a"))),
               'Both `...` and `.list` were supplied')
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

