context("pop")

test_that("pop: ranks", {
  expect_equal(names(ex_hierarchy1$ranklist), c('species', 'genus', 'family'))

  # without piping
  aa1 <- pop(ex_hierarchy1, ranks(family))
  expect_is(aa1, "Hierarchy")
  expect_equal(names(aa1$ranklist), c('genus', 'species'))
  expect_equal(length(aa1$taxa), 2)

  # with piping
  aa2 <- ex_hierarchy1 %>% pop(ranks(family))
  expect_is(aa2, "Hierarchy")
  expect_equal(names(aa2$ranklist), c('genus', 'species'))
  expect_equal(length(aa2$taxa), 2)
  expect_equal(aa1, aa2)

  # with piping, many entries
  aa3 <- ex_hierarchy1 %>% pop(ranks(family, genus))
  expect_is(aa3, "Hierarchy")
  expect_equal(names(aa3$ranklist), 'species')
  expect_equal(length(aa3$taxa), 1)
})

test_that("pop: names", {
  # without piping
  aa1 <- pop(ex_hierarchy1, nms(Poa))
  expect_is(aa1, "Hierarchy")
  expect_equal(names(aa1$ranklist), c('family', 'species'))
  expect_equal(length(aa1$taxa), 2)

  # with piping
  aa2 <- ex_hierarchy1 %>% pop(nms(Poa))
  expect_is(aa2, "Hierarchy")
  expect_equal(names(aa2$ranklist), c('family', 'species'))
  expect_equal(length(aa2$taxa), 2)
  expect_equal(aa1, aa2)

  # with piping, many entries
  aa3 <- ex_hierarchy1 %>% pop(nms(Poaceae, Poa))
  expect_is(aa3, "Hierarchy")
  expect_equal(names(aa3$ranklist), 'species')
  expect_equal(length(aa3$taxa), 1)
})

test_that("pop: ids", {
  # without piping
  aa1 <- pop(ex_hierarchy1, ids(4479))
  expect_is(aa1, "Hierarchy")
  expect_equal(names(aa1$ranklist), c('genus', 'species'))
  expect_equal(length(aa1$taxa), 2)

  # with piping
  aa2 <- ex_hierarchy1 %>% pop(ids(4479))
  expect_is(aa2, "Hierarchy")
  expect_equal(names(aa2$ranklist), c('genus', 'species'))
  expect_equal(length(aa2$taxa), 2)
  expect_equal(aa1, aa2)

  # with piping, many entries
  aa3 <- ex_hierarchy1 %>% pop(ids(4479, 4544))
  expect_is(aa3, "Hierarchy")
  expect_equal(names(aa3$ranklist), 'species')
  expect_equal(length(aa3$taxa), 1)
})


# no variables given to pop just gives back same thing
test_that("pop gives back same .data object if no things asked to be removed", {
  ## FIXME: the ranklist elements are actually different, which they probably
  ## shouldn’t be
  expect_equal(length(pop(ex_hierarchy1)$taxa), length(ex_hierarchy1$taxa))
})

test_that("pop fails well", {
  expect_error(pop(),
               "argument \".data\" is missing")
  expect_error(pop(5),
               "no 'pop' method for numeric")
  expect_error(pop("adf", 5),
               "no 'pop' method for character")
})

test_that("pop: mixed ranks, names and ids", {
  aa1 <- ex_hierarchy1 %>% pop(ranks(family), ids(4544))

  expect_is(aa1, "Hierarchy")
  expect_equal(names(aa1$ranklist), 'species')
  expect_equal(length(aa1$taxa), 1)
})
