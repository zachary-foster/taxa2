maketax <- function(binomial, grouping){
  res <- Taxon$new(binomial = binomial, grouping = grouping)
  res
  #structure(res, class = c("Taxon", "stuff"))
}

`[.MultiTaxonId` <- function(x, i) {
  x$ids[i]
}

`[[.MultiTaxonId` <- function(x, i) {
  x$ids[[i]]
}

`[.TaxonId` <- function(x, i) {
  x$id[i]
}
