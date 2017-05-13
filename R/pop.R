#' @title Pop names out
#'
#' @description That is, drop them
#'
#' @export
#' @param .data Input, object of class taxon
#' @param ... unquoted rank names, e.g., family
#' @details supports `Hierarchy` and `hierarchies` objects
#' @return an object of the same class as passed in
#' @template pop_egs
pop <- function(.data, ...) {
  UseMethod("pop")
}

#' @export
pop.default <- function(.data, ...) {
  stop("no 'pop' method for ", class(.data), call. = FALSE)
}

#' @export
pop.Hierarchy <- function(.data, ...){
  nms <- dplyr::vars(...)
  nms <- vapply(nms, function(z) as.character(z$expr), "", USE.NAMES = FALSE)
  .data$pop(nms)
}

#' @export
pop.hierarchies <- function(.data, ...){
  hierarchies(.list = lapply(.data, pop, ...))
}
