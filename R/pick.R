#' @title Pick taxa
#'
#' @description Pick out specific taxa, while others are dropped
#'
#' @export
#' @param .data Input, object of class `Hierarchy`, or `hierarchies`
#' @param ... unquoted rank names (e.g., family), taxon names (e.g.,
#' Poa annua), or taxnomic IDs (e.g., 93036)
#' @details supports `Hierarchy` and `hierarchies` objects
#' @return an object of the same class as passed in
#' @template pick_egs
pick <- function(.data, ...) {
  UseMethod("pick")
}

#' @export
pick.default <- function(.data, ...) {
  stop("no 'pick' method for ", class(.data), call. = FALSE)
}

#' @export
pick.Hierarchy <- function(.data, ...) {
  .data <- .data$clone(deep = TRUE)
  tmp <- Taxapickers$new(...)
  if (length(tmp$x) == 0) stop("no acceptable selectors passed in")
  .data$pick(ranks = unlist(lapply(tmp$ranks(), function(z) z$ranks)),
             names = unlist(lapply(tmp$names(), function(z) z$names)),
             ids = unlist(lapply(tmp$ids(), function(z) z$ids)))
}

#' @export
pick.hierarchies <- function(.data, ...) {
  hierarchies(.list = lapply(.data, pick, ...))
}
