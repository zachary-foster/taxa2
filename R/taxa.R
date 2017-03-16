#' Make a set of many Taxon class objects
#'
#' @export
#' @param ... Any number of object of class `Taxon`
#' @return An `R6Class` object of class `Taxon`
#' @examples
#' (x <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' ))
#' taxa_(x, x, x)
taxa_ <- function(...) {
  structure(list(...), class = "taxa")
}

#' @export
print.taxa <- function(x, ...) {
  cat("<taxa>", "\n")
  cat("  no. taxa: ", length(x), "\n")
  for (i in seq_along(x[1:min(10, length(x))])) {
    cat(
      sprintf("  %s / %s / %s",
              x[[i]]$name$name %||% "",
              x[[i]]$rank$name %||% "",
              x[[i]]$id$id %||% ""
    ), "\n")
  }
  if (length(x) > 10) cat("  ...")
}
