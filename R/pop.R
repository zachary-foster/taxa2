#' @title Pop taxa out
#'
#' @description That is, drop them
#'
#' @export
#' @param .data Input, object of class [Hierarchy], or [hierarchies]
#' @param ... unquoted rank names (e.g., family), taxon names (e.g.,
#' Poa annua), or taxnomic IDs (e.g., 93036)
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
pop.Hierarchy <- function(.data, ...) {
  .data <- .data$clone(deep = TRUE)
  tmp <- c(...)
  .data$pop(ranks = tmp$ranks, names = tmp$names, ids = tmp$ids)
}

#' @export
pop.hierarchies <- function(.data, ...){
  hierarchies(.list = lapply(.data, pop, ...))
}

# pop.Taxonomy <- function(.data, ...){
#   .data <- .data$clone(deep = TRUE)
#   tmp <- unlist(lapply(lazyeval::lazy_dots(...), lazyeval::lazy_eval), FALSE)
#   .data$pop(ranks = tmp$ranks, names = tmp$names, ids = tmp$ids)
# }
