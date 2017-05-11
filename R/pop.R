#' @title Pop names out
#'
#' @description That is, drop them
#'
#' @export
#' @param .data Input, object of class taxon
#' @param ... unquoted rank names, e.g., family
#' @details Only supports `Hierarchy` objects for now
#' @return an object of the same class as passed in
#' @examples
#' (x <- taxon(
#'   name = taxon_name("Poaceae"),
#'   rank = taxon_rank("family"),
#'   id = taxon_id(4479)
#' ))
#'
#' (y <- taxon(
#'   name = taxon_name("Poa"),
#'   rank = taxon_rank("genus"),
#'   id = taxon_id(4544)
#' ))
#'
#' (z <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' ))
#'
#' (res <- hierarchy(z, y, x))
#'
#' ## single taxonomic group
#' res %>% pop(family)
#' ### more than 1 - remake res object above first
#' # res %>% pop(family, genus)

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

# pop.taxa <- function(.data, ...){
#   taxa(lapply(.data, pop, ...))
# }

# pop.taxondf <- function(.data, ...){
#   var <- vars(...)
#   check_vars(var, names(.data))
#   select_(.data, .dots = paste("-", var, sep = ""))
# }
