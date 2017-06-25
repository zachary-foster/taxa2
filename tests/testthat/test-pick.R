context("pick")

test_that("pick: ranks", {
  expect_equal(names(ex_hierarchy1$ranklist), c('species', 'genus', 'family'))

  # without piping
  aa1 <- pick(ex_hierarchy1, ranks(family))
  expect_is(aa1, "Hierarchy")
  expect_equal(names(aa1$ranklist), c('genus', 'species'))
  expect_equal(length(aa1$taxa), 2)

  # with piping
  aa2 <- ex_hierarchy1 %>% pick(ranks(family))
  expect_is(aa2, "Hierarchy")
  expect_equal(names(aa2$ranklist), c('genus', 'species'))
  expect_equal(length(aa2$taxa), 2)
  expect_equal(aa1, aa2)

  # with piping, many entries
  aa3 <- ex_hierarchy1 %>% pick(ranks(family, genus))
  expect_is(aa3, "Hierarchy")
  expect_equal(names(aa3$ranklist), 'species')
  expect_equal(length(aa3$taxa), 1)
})

test_that("pick: names", {
  # without piping
  aa1 <- pick(ex_hierarchy1, nms(Poa))
  expect_is(aa1, "Hierarchy")
  expect_equal(names(aa1$ranklist), c('family', 'species'))
  expect_equal(length(aa1$taxa), 2)

  # with piping
  aa2 <- ex_hierarchy1 %>% pick(nms(Poa))
  expect_is(aa2, "Hierarchy")
  expect_equal(names(aa2$ranklist), c('family', 'species'))
  expect_equal(length(aa2$taxa), 2)
  expect_equal(aa1, aa2)

  # with piping, many entries
  aa3 <- ex_hierarchy1 %>% pick(nms(Poaceae, Poa))
  expect_is(aa3, "Hierarchy")
  expect_equal(names(aa3$ranklist), 'species')
  expect_equal(length(aa3$taxa), 1)
})

test_that("pick: ids", {
  # without piping
  aa1 <- pick(ex_hierarchy1, ids(4479))
  expect_is(aa1, "Hierarchy")
  expect_equal(names(aa1$ranklist), c('genus', 'species'))
  expect_equal(length(aa1$taxa), 2)

  # with piping
  aa2 <- ex_hierarchy1 %>% pick(ids(4479))
  expect_is(aa2, "Hierarchy")
  expect_equal(names(aa2$ranklist), c('genus', 'species'))
  expect_equal(length(aa2$taxa), 2)
  expect_equal(aa1, aa2)

  # with piping, many entries
  aa3 <- ex_hierarchy1 %>% pick(ids(4479, 4544))
  expect_is(aa3, "Hierarchy")
  expect_equal(names(aa3$ranklist), 'species')
  expect_equal(length(aa3$taxa), 1)
})


# no variables given to pick just gives back same thing
test_that("pick gives back same .data object if no things asked to be removed", {
  ## FIXME: the ranklist elements are actually different, which they probably
  ## shouldn’t be
  expect_equal(length(pick(ex_hierarchy1)$taxa), length(ex_hierarchy1$taxa))
})

test_that("pick fails well", {
  expect_error(pick(),
               "argument \".data\" is missing")
  expect_error(pick(5),
               "no 'pick' method for numeric")
  expect_error(pick("adf", 5),
               "no 'pick' method for character")
})

test_that("pick: mixed ranks, names and ids", {
  aa1 <- ex_hierarchy1 %>% pick(ranks(family), ids(4544))

  expect_is(aa1, "Hierarchy")
  expect_equal(names(aa1$ranklist), 'species')
  expect_equal(length(aa1$taxa), 1)
})
