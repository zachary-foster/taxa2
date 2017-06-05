#' Lazy
#'
#' @name lazy-helpers
#' @section How do these functions work?:
#' Each function is a light wrapper around [dplyr::vars()], to use lazy
#' evaluation to be able to pass in unquote variables to save some typing,
#' though you can still quote and get the same result
#'
#' These are not used by themselves but inside of [pop()], [pick()], [span()]
#'
#' @section Symbols:
#' \itemize{
#'  \item `:`: items between x and y, inclusive
#'  \item `::`: items between x and y, exclusive
#'  \item `. <`: all items below rank of x
#'  \item `. >`: all items above rank of x
#' }
#'
#' @section ranks:
#' asdfasdf
#'
#' @section nms:
#' asdfasdf
#'
#' @section ids:
#' asdfasdf
#'
#' @examples
#' ranks(order)
#' ranks(order, genus)
#' ranks(order:genus)
#' ranks(. > genus)
#'
#' nms(Poaceae)
#' nms(Poaceae, Poa)
#' nms(Poaceae:Poa)
#' nms(. < Poaceae)
#'
#' ids(4544)
#' ids(4544, 4479)
#' ids(4544:4479)
#' ids(. < 4479)
NULL

#' @export
#' @rdname lazy-helpers
ranks <- function(...) {
  rks <- dplyr::vars(...)
  clzzs <- vapply(rks, function(z) class(z$expr), "", USE.NAMES = FALSE)
  if (any(clzzs == "call")) {
    tmp <- as.character(rks[[1]]$expr)
    structure(list(ranks = tmp[2:3]), operator = tmp[1])
  } else {
    tmp <- vapply(rks, function(z) as.character(z$expr), "", USE.NAMES = FALSE)
    list(ranks = tmp)
  }
}

#' @export
#' @rdname lazy-helpers
nms <- function(...) {
  rks <- dplyr::vars(...)
  clzzs <- vapply(rks, function(z) class(z$expr), "", USE.NAMES = FALSE)
  if (any(clzzs == "call")) {
    tmp <- as.character(rks[[1]]$expr)
    structure(list(names = tmp[2:3]), operator = tmp[1])
  } else {
    tmp <- vapply(rks, function(z) as.character(z$expr), "", USE.NAMES = FALSE)
    list(names = tmp)
  }
}

#' @export
#' @rdname lazy-helpers
ids <- function(..., .list = NULL) {
  rks <- dplyr::vars(...)
  clzzs <- vapply(rks, function(z) class(z$expr), "", USE.NAMES = FALSE)
  if (any(clzzs == "call")) {
    tmp <- as.character(rks[[1]]$expr)
    structure(list(ids = tmp[2:3]), operator = tmp[1])
  } else {
    tmp <- vapply(rks, function(z) as.character(z$expr), "", USE.NAMES = FALSE)
    list(ids = tmp)
  }
}
