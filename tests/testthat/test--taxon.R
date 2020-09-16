context("taxon")
library(taxa)


# Creating taxon objects

test_that("taxon objects can be created from character input", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(length(x), 3)
  expect_equal(class(x)[1], 'taxa_taxon')
  expect_equal(as.character(x[3]), 'C')
})

test_that("taxon objects can be created from factor input", {
  x <- taxon(as.factor(c('A', 'B', 'C')))
  expect_equal(length(x), 3)
  expect_equal(class(x)[1], 'taxa_taxon')
  expect_equal(as.character(x[3]), 'C')
})

test_that("taxon objects can be created with names", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  expect_equal(length(x), 3)
  expect_equal(class(x)[1], 'taxa_taxon')
  expect_equal(as.character(x[3]), c(c = 'C'))
  expect_equal(names(x), letters[1:3])
})


# Subsetting taxon objects with `[`

test_that("taxon objects can be `[` subset by index", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(length(x[1:2]), 2)
  expect_equal(x[2:3], taxon(c('B', 'C')))
})

test_that("taxon objects can be `[` subset by logical vector", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(length(x[c(FALSE, TRUE, FALSE)]), 1)
  expect_equal(x[c(FALSE, TRUE, TRUE)], taxon(c('B', 'C')))
})

test_that("taxon objects can be `[` subset by name", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  expect_equal(length(x[c('a', 'b')]), 2)
  expect_equal(x[c('b', 'c')], taxon(c('B', 'C'), .names = letters[2:3]))
})


# Subsetting taxon objects with `[[`

test_that("taxon objects can be `[[` subset by index", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(length(x[[1]]), 1)
  expect_equal(x[[3]], taxon('C'))
})

test_that("taxon objects can be `[[` subset by name", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  expect_equal(length(x[['c']]), 1)
  expect_equal(x[['c']], taxon('C')) # names are dropped for [[
})


# Setting names of taxon objects

test_that("taxon objects can be named", {
  x <- taxon(c('A', 'B', 'C'))
  names(x) <- letters[1:3]
  expect_equal(x, taxon(c('A', 'B', 'C'), .names = letters[1:3]))
  expect_equal(names(x),  letters[1:3])
  names(x)[2] <- 'd'
  expect_equal(names(x), c('a', 'd', 'c'))
})


# Assigning values to taxon objects

test_that("taxon objects can have values assigned to them", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  x[2] <- '4'
  expect_equal(x[2], taxon('4', .names = letters[2]))
  x['b'] <- '4'
  expect_equal(x[2], taxon('4', .names = letters[2]))
  x[c(FALSE, TRUE, FALSE)] <- '4'
  expect_equal(x[2], taxon('4', .names = letters[2]))
})

test_that("taxon assignment is recycled correctly", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  x[1:3] <- '4'
  expect_equal(x,  taxon(c('4', '4', '4'), .names = letters[1:3]))
})


# Can be concatenated

test_that("taxon objects can be combined", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(c(x, x), taxon(rep(c('A', 'B', 'C'), 2)))
  expect_equal(c(x, x, x), taxon(rep(c('A', 'B', 'C'), 3)))
})

test_that("named taxon objects can be combined", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  expect_equal(c(x, x), taxon(rep(c('A', 'B', 'C'), 2), .names = rep(letters[1:3], 2)))
  expect_equal(c(x, x, x), taxon(rep(c('A', 'B', 'C'), 3), .names = rep(letters[1:3], 3)))
})


# Taxon can be compared with eachother

test_that("taxon objects can be compared with eachother", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(x == x[1], c(TRUE, FALSE, FALSE))
})


# Works with `rep`

test_that("taxon objects work with `rep`", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(rep(x, 2), taxon(rep(c('A', 'B', 'C'), 2)))
  expect_equal(rep(x, 3), taxon(rep(c('A', 'B', 'C'), 3)))
})

test_that("named taxon objects work with `rep`", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  expect_equal(rep(x, 2), taxon(rep(c('A', 'B', 'C'), 2), .names = rep(letters[1:3], 2)))
  expect_equal(rep(x, 3), taxon(rep(c('A', 'B', 'C'), 3), .names = rep(letters[1:3], 3)))
})

# Works with `seq_along`

test_that("taxon objects work with `seq_along`", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(seq_along(x), 1:3)
})

test_that("named taxon objects work with `seq_along`", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  expect_equal(seq_along(x), 1:3)
})


# Can be converted to character

test_that("taxon objects can be converted to characters", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(as.character(x), c('A', 'B', 'C'))
})

test_that("named taxon objects can be converted to characters", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  expect_equal(as.character(x), c(a = 'A', b = 'B', c = 'C'))
})


# Can be converted to factor

test_that("taxon objects can be converted to factor", {
  x <- taxon(c('A', 'B', 'C'))
  expect_equal(as.factor(x), as.factor(c('A', 'B', 'C')))
})

test_that("named taxon objects can be converted to factor", {
  x <- taxon(c('A', 'B', 'C'), .names = letters[1:3])
  expect_equal(as.factor(x), as.factor(c(a = 'A', b = 'B', c= 'C')))
})


# Can be converted to a data.frame

test_that("taxon objects can be converted to a data.frame", {
  x <- taxon(name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
             rank = c('species', 'genus', 'phylum', 'family'),
             id = taxon_id(c('9606', '1386', '4890', '4345'), db = 'ncbi'),
             auth = c('Linnaeus, 1758', 'Cohn 1872', NA, 'Juss., 1789'))
  expect_equal(
    as_data_frame(x),
    data.frame(tax_name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
               tax_rank = c('species', 'genus', 'phylum', 'family'),
               tax_id = c('9606', '1386', '4890', '4345'),
               tax_db = 'ncbi',
               tax_author = c('Linnaeus', 'Cohn', NA, 'Juss.'),
               tax_date = c('1758', '1872', NA, '1789'),
               tax_cite = NA_character_)
  )
})

test_that("named taxon objects can be converted to data.frame", {
  x <- taxon(name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
             rank = c('species', 'genus', 'phylum', 'family'),
             id = taxon_id(c('9606', '1386', '4890', '4345'), db = 'ncbi'),
             auth = c('Linnaeus, 1758', 'Cohn 1872', NA, 'Juss., 1789'),
             .names = letters[1:4])
  expect_equal(
    as_data_frame(x),
    data.frame(tax_name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
               tax_rank = c('species', 'genus', 'phylum', 'family'),
               tax_id = c('9606', '1386', '4890', '4345'),
               tax_db = 'ncbi',
               tax_author = c('Linnaeus', 'Cohn', NA, 'Juss.'),
               tax_date = c('1758', '1872', NA, '1789'),
               tax_cite = NA_character_)
  )
})


# Can be converted to a tibble

test_that("taxon objects can be converted to a tibble", {
  x <- taxon(name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
             rank = c('species', 'genus', 'phylum', 'family'),
             id = taxon_id(c('9606', '1386', '4890', '4345'), db = 'ncbi'),
             auth = c('Linnaeus, 1758', 'Cohn 1872', NA, 'Juss., 1789'))
  expect_equal(
    tibble::as_tibble(x),
    tibble::tibble(tax_name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
                   tax_rank = c('species', 'genus', 'phylum', 'family'),
                   tax_id = c('9606', '1386', '4890', '4345'),
                   tax_db = 'ncbi',
                   tax_author = c('Linnaeus', 'Cohn', NA, 'Juss.'),
                   tax_date = c('1758', '1872', NA, '1789'),
                   tax_cite = NA_character_)
  )
})

test_that("named taxon objects can be converted to a tibble", {
  x <- taxon(name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
             rank = c('species', 'genus', 'phylum', 'family'),
             id = taxon_id(c('9606', '1386', '4890', '4345'), db = 'ncbi'),
             auth = c('Linnaeus, 1758', 'Cohn 1872', NA, 'Juss., 1789'),
             .names = letters[1:4])
  expect_equal(
    tibble::as_tibble(x),
    tibble::tibble(tax_name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
                   tax_rank = c('species', 'genus', 'phylum', 'family'),
                   tax_id = c('9606', '1386', '4890', '4345'),
                   tax_db = 'ncbi',
                   tax_author = c('Linnaeus', 'Cohn', NA, 'Juss.'),
                   tax_date = c('1758', '1872', NA, '1789'),
                   tax_cite = NA_character_)
  )
})
