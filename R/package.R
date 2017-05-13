#' taxa
#'
#' `taxa` defines taxonomic classes and functions to manipulate them. The
#' goal is to use these classes as low level fundamental taxonomic classes
#' that other R packages can build on and use.
#'
#' There are two distinct types of classes in `taxa`:
#' * Classes that are concerned only with taxonomic information: `taxon`,
#' `taxonomy`, `hierarchy`, etc.
#' * A class called `taxmap` that is concerned with combining taxonomic
#' data with user-defined data of any type (e.g. molecular sequences,
#' abundance counts etc.)
#'
#' Checkout the vignette (`browseVignettes("taxa")`) for detailed introduction
#' and examples.
#'
#' @import R6 jsonlite
#' @name taxa-package
#' @aliases taxa
#' @docType package
#' @keywords package
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @author Zachary Foster \email{zacharyfoster1989@@gmail.com}
NULL
