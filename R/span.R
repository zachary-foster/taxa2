#' @title Span taxa
#'
#' @description Select a range of taxa
#'
#' @export
#' @param .data Input, object of class [Hierarchy], or [hierarchies]
#' @param ... unquoted rank names (e.g., family), taxon names (e.g.,
#' Poa annua), or taxnomic IDs (e.g., 93036)
#' @details supports `Hierarchy` and `hierarchies` objects
#' @return an object of the same class as passed in
#' @template pick_egs
span <- function(.data, ...) {
  UseMethod("span")
}

#' @export
span.default <- function(.data, ...) {
  stop("no 'span' method for ", class(.data), call. = FALSE)
}

#' @export
span.Hierarchy <- function(.data, ...) {
  .data <- .data$clone(deep = TRUE)
  tmp <- Taxapickers$new(...)
  .data$span(ranks = tmp$ranks(), names = tmp$names(), ids = tmp$ids())
}

#' @export
span.hierarchies <- function(.data, ...) {
  hierarchies(.list = lapply(.data, span, ...))
}
