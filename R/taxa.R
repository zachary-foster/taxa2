#' A class for multiple taxon objects
#'
#' Stores one or more [taxon()] objects. This is just a thin wrapper for a list
#' of [taxon()] objects.
#'
#' @export
#' @param ... Any number of object of class `Taxon`
#' @return An `R6Class` object of class `Taxon`
#' @family classes
#' @examples
#' (x <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' ))
#' taxa_(x, x, x)
taxa_ <- function(...) {
  tt <- list(...)
  #if (length(tt) < 1) stop("must give at least 1 input", call. = FALSE)
  if (!all(vapply(tt, inherits, logical(1), what = "Taxon"))) {
    stop("all inputs to 'taxa_' must be of class 'Taxon'",
         call. = FALSE)
  }
  structure(tt, class = "taxa")
}

#' @export
print.taxa <- function(x, ...) {
  cat("<taxa>", "\n")
  cat("  no. taxa: ", length(x), "\n")
  if (length(x)) {
    for (i in seq_along(x[1:min(10, length(x))])) {
      cat(
        sprintf("  %s / %s / %s",
                x[[i]]$name$name %||% "",
                x[[i]]$rank$name %||% "",
                x[[i]]$id$id %||% ""
        ), "\n")
    }
  }
  if (length(x) > 10) cat("  ...")
}
