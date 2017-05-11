#' @title Pop names out
#'
#' @description That is, drop them
#'
#' @export
#'
#' @param .data Input, object of class taxon
#' @param ... Further unnamed args, see examples
#' @return For \code{taxon} inputs gives back a \code{taxon} object. For
#' \code{taxa} inputs gives back a \code{taxa} object. For \code{taxondf}
#' inputs, gives back a \code{taxondf} object.
#' @examples
#' # operating on `hierarchy` objects
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

pop <- function(.data, ...) {
  UseMethod("pop")
}

#' @export
pop.Hierarchy <- function(.data, ...){
  name <- vars(...)
  if (!name %in% names(res$ranklist)) stop(name, " not found", call. = FALSE)
  which(name == vapply(.data$taxa, function(x) x$rank$name, ""))
  #hierarchy()
}

# pop.taxa <- function(.data, ...){
#   taxa(lapply(.data, pop, ...))
# }

# pop.taxondf <- function(.data, ...){
#   var <- vars(...)
#   check_vars(var, names(.data))
#   select_(.data, .dots = paste("-", var, sep = ""))
# }
