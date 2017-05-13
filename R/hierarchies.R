#' Make a set of many Hierarchy class objects
#'
#' @export
#' @param ... Any number of object of class `Hierarchy`
#' @return An `R6Class` object of class `Hierarchy`
#' @examples
#' x <- taxon(
#'   name = taxon_name("Poaceae"),
#'   rank = taxon_rank("family"),
#'   id = taxon_id(4479)
#' )
#' y <- taxon(
#'   name = taxon_name("Poa"),
#'   rank = taxon_rank("genus"),
#'   id = taxon_id(4544)
#' )
#' z <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' )
#' hier1 <- hierarchy(z, y, x)
#'
#' a <- taxon(
#'   name = taxon_name("Felidae"),
#'   rank = taxon_rank("family"),
#'   id = taxon_id(9681)
#' )
#' b <- taxon(
#'   name = taxon_name("Puma"),
#'   rank = taxon_rank("genus"),
#'   id = taxon_id(146712)
#' )
#' c <- taxon(
#'   name = taxon_name("Puma concolor"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(9696)
#' )
#' hier2 <- hierarchy(c, b, a)
#'
#' hierarchies(hier1, hier2)
hierarchies <- function(...) {
  hiers <- list(...)
  if (!all(vapply(hiers, inherits, logical(1), what = "Hierarchy"))) {
    stop("all inputs to 'hierarchies' must be of class 'Hierarchy'",
         call. = FALSE)
  }
  structure(hiers, class = "hierarchies")
}

#' @export
print.hierarchies <- function(x, ...) {
  cat("<Hierarchies>", "\n")
  cat("  no. hierarchies: ", length(x), "\n")
  if (length(x)) {
    for (i in seq_along(x[1:min(10, length(x))])) {
      if (is.null(x[[i]]$taxa)) {
        cat("  Empty hierarchy", sep = "\n")
      } else {
        cat(
          paste0("  ", paste0(vapply(x[[i]]$taxa, function(x) x$name$name, ""),
                              collapse = " / ")),
          "\n"
        )
      }
    }
  }
  if (length(x) > 10) cat("  ...")
}
