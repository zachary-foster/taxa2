context("taxon_id")
library(taxa2)


# Creating taxon_id objects

test_that("taxon_id objects can be created from character input", {
  x <- taxon_id(c('1', '2', '3'))
  expect_equal(length(x), 3)
  expect_equal(class(x)[1], 'taxa_taxon_id')
  expect_equal(as.character(x[3]), '3')
})

test_that("taxon_id objects can be created from factor input", {
  x <- taxon_id(as.factor(c('1', '2', '3')))
  expect_equal(length(x), 3)
  expect_equal(class(x)[1], 'taxa_taxon_id')
  expect_equal(as.character(x[3]), '3')
})

test_that("taxon_id objects can be created with names", {
  x <- taxon_id(c('1', '2', '3'), .names = letters[1:3])
  expect_equal(length(x), 3)
  expect_equal(class(x)[1], 'taxa_taxon_id')
  expect_equal(as.character(x[3]), c(c = '3'))
  expect_equal(names(x), letters[1:3])
})


# Subsetting taxon_id objects with `[`

test_that("taxon_id objects can be `[` subset by index", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'))
  expect_equal(length(x[1:2]), 2)
  expect_equal(x[2:3], taxon_id(c('2', '3'), db = c('ncbi', 'itis')))
})

test_that("taxon_id objects can be `[` subset by logical vector", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'))
  expect_equal(length(x[c(FALSE, TRUE, FALSE)]), 1)
  expect_equal(x[c(FALSE, TRUE, TRUE)], taxon_id(c('2', '3'), db = c('ncbi', 'itis')))
})

test_that("taxon_id objects can be `[` subset by name", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'), .names = letters[1:3])
  expect_equal(length(x[c('a', 'b')]), 2)
  expect_equal(x[c('b', 'c')], taxon_id(c('2', '3'), db = c('ncbi', 'itis'), .names = letters[2:3]))
})


# Subsetting taxon_id objects with `[[`

test_that("taxon_id objects can be `[[` subset by index", {
  x <- taxon_id(c('1', '2', '3'))
  expect_equal(length(x[[1]]), 1)
  expect_equal(x[[3]], taxon_id('3'))
})

test_that("taxon_id objects can be `[[` subset by name", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'), .names = letters[1:3])
  expect_equal(length(x[['c']]), 1)
  expect_equal(x[['c']], taxon_id('3',db = 'itis')) # names are dropped for [[
})


# Setting names of taxon_id objects

test_that("taxon_id objects can be named", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'))
  names(x) <- letters[1:3]
  expect_equal(x, taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'), .names = letters[1:3]))
  expect_equal(names(x),  letters[1:3])
  names(x)[2] <- 'd'
  expect_equal(names(x), c('a', 'd', 'c'))
})


# Assigning values to taxon_id objects

test_that("taxon_id objects can have values assigned to them", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'), .names = letters[1:3])
  x[2] <- '4'
  expect_equal(x[2], taxon_id('4', .names = letters[2]))
  x['b'] <- '4'
  expect_equal(x[2], taxon_id('4', .names = letters[2]))
  x[c(FALSE, TRUE, FALSE)] <- '4'
  expect_equal(x[2], taxon_id('4', .names = letters[2]))
})

test_that("taxon_id assignment is recycled correctly", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'), .names = letters[1:3])
  x[1:3] <- '4'
  expect_equal(x,  taxon_id(c('4', '4', '4'), .names = letters[1:3]))
})


# Invalid values cannot be assigned

test_that("taxon_id objects cannot be created with invalid databases", {
  expect_error(taxon_id('1', db = 'not_valid'), "Taxon database names must be defined in `db_ref`")
})

test_that("taxon_id objects cannot be assigned invalid values woth `[`", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'), .names = letters[1:3])
  expect_error(tax_db(x)[2] <- 'not_valid', "Taxon database names must be defined in `db_ref`")
})

test_that("taxon_id objects cannot be assigned invalid values woth `[[`", {
  x <- taxon_id(c('1', '2', '3'), .names = letters[1:3])
  expect_error(tax_db(x)[[2]] <- 'not_valid', "Taxon database names must be defined in `db_ref`")
})


# Can be concatenated

test_that("taxon_id objects can be combined", {
  x <- taxon_id(c('1', '2', '3'))
  expect_equal(c(x, x), taxon_id(rep(c('1', '2', '3'), 2)))
  expect_equal(c(x, x, x), taxon_id(rep(c('1', '2', '3'), 3)))
})

test_that("named taxon_id objects can be combined", {
  x <- taxon_id(c('1', '2', '3'), .names = letters[1:3])
  expect_equal(c(x, x), taxon_id(rep(c('1', '2', '3'), 2), .names = rep(letters[1:3], 2)))
  expect_equal(c(x, x, x), taxon_id(rep(c('1', '2', '3'), 3), .names = rep(letters[1:3], 3)))
})


# Works with `rep`

test_that("taxon_id objects work with `rep`", {
  x <- taxon_id(c('1', '2', '3'))
  expect_equal(rep(x, 2), taxon_id(rep(c('1', '2', '3'), 2)))
  expect_equal(rep(x, 3), taxon_id(rep(c('1', '2', '3'), 3)))
})

test_that("named taxon_id objects work with `rep`", {
  x <- taxon_id(c('1', '2', '3'), .names = letters[1:3])
  expect_equal(rep(x, 2), taxon_id(rep(c('1', '2', '3'), 2), .names = rep(letters[1:3], 2)))
  expect_equal(rep(x, 3), taxon_id(rep(c('1', '2', '3'), 3), .names = rep(letters[1:3], 3)))
})

# Works with `seq_along`

test_that("taxon_id objects work with `seq_along`", {
  x <- taxon_id(c('1', '2', '3'))
  expect_equal(seq_along(x), 1:3)
})

test_that("named taxon_id objects work with `seq_along`", {
  x <- taxon_id(c('1', '2', '3'), .names = letters[1:3])
  expect_equal(seq_along(x), 1:3)
})


# Can be converted to character

test_that("taxon_id objects can be converted to characters", {
  x <- taxon_id(c('1', '2', '3'))
  expect_equal(as.character(x), c('1', '2', '3'))
})

test_that("named taxon_id objects can be converted to characters", {
  x <- taxon_id(c('1', '2', '3'), .names = letters[1:3])
  expect_equal(as.character(x), c(a = '1', b = '2', c = '3'))
})


# Can be converted to factor

test_that("taxon_id objects can be converted to factor", {
  x <- taxon_id(c('1', '2', '3'))
  expect_equal(as.factor(x), as.factor(c('1', '2', '3')))
})

test_that("named taxon_id objects can be converted to factor", {
  x <- taxon_id(c('1', '2', '3'), .names = letters[1:3])
  expect_equal(as.factor(x), as.factor(c(a = '1', b = '2', c= '3')))
})


# Can be converted to a data.frame

test_that("taxon_id objects can be converted to a data.frame", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'))
  expect_equal(
    as_data_frame(x),
    data.frame(tax_id = c('1', '2', '3'), tax_db = c('ncbi', 'ncbi', 'itis'))
  )
})

test_that("named taxon_id objects can be converted to data.frame", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'), .names = letters[1:3])
  expect_equal(
    as_data_frame(x),
    data.frame(tax_id = c('1', '2', '3'), tax_db = c('ncbi', 'ncbi', 'itis'))
  )
})


# Can be converted to a tibble

test_that("taxon_id objects can be converted to a tibble", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'))
  expect_equal(
    tibble::as_tibble(x),
    tibble::tibble(tax_id = c('1', '2', '3'), tax_db = c('ncbi', 'ncbi', 'itis'))
  )
})

test_that("named taxon_id objects can be converted to a tibble", {
  x <- taxon_id(c('1', '2', '3'), db = c('ncbi', 'ncbi', 'itis'), .names = letters[1:3])
  expect_equal(
    tibble::as_tibble(x),
    tibble::tibble(tax_id = c('1', '2', '3'), tax_db = c('ncbi', 'ncbi', 'itis'))
  )
})
