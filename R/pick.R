#' @title Pick names
#'
#' @description Pick out specific names, while others are dropped
#'
#' @export
#' @param .data Input, object of class taxon
#' @param ... unquoted rank names, e.g., family
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
pick.Hierarchy <- function(.data, ...){
  nms <- dplyr::vars(...)
  nms <- vapply(nms, function(z) as.character(z$expr), "",
                USE.NAMES = FALSE)
  .data$pick(nms)
}

#' @export
pick.hierarchies <- function(.data, ...){
  hierarchies(.list = lapply(.data, pick, ...))
}
