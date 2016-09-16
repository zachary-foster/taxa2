#' Taxonomy class
#'
#' @export
#' @param ... Any number of object of class \code{Taxon}
#' @return An \code{R6Class} object of class \code{Taxonomy}
#'
#' @details on initialize, we parse the inputs and find all duplicate
#' taxonomic names of the same rank, make an edge list
#'
#' @template taxonomyegs

taxonomy <- function(...) {
  Taxonomy$new(...)
}

Taxonomy <- R6::R6Class(
  "Taxonomy",
  lock_class = TRUE,
  public = list(
    taxa = NULL,
    edge_list = NULL,

    initialize = function(...) {
      private$unique_taxa(list(...))
      private$make_edge_list(list(...))
    },

    print = function(indent = "") {
      max_chars <- getOption("width") - 10
      cat(paste0(indent, "<Taxonomy>\n"))
      # taxon_names <- vapply(self$taxa, function(x) x$name, character(1))
      cat(paste0("  no. unique taxa: ", length(self$taxa)), "\n")
      cat(paste0("  graph: ", paste0(private$make_graph(), collapse = " ")), "\n")
      invisible(self)
    }
  ),

  private = list(
    unique_taxa = function(x) {
      tx <- unlist(lapply(x, function(z) {
        z$taxa
      }))
      utx <- unique(tx)
      self$taxa <- stats::setNames(utx, seq_along(utx))
    },

    make_edge_list = function(x) {
      edge_lists <- lapply(x, function(z) {
        tmp <- as.numeric(vapply(z$taxa, function(w) {
          names(which(vapply(self$taxa, function(n) identical(n, w), logical(1))))
        }, ""))
        res <- tibble::data_frame(to = NA_integer_, from = NA_integer_)
        for (i in 1:(length(tmp) - 1)) {
          res[i,] <- c(tmp[c(i, i + 1)])
        }
        res
      })
      self$edge_list <- unique(dplyr::bind_rows(edge_lists))
    },

    make_graph = function() {
      apply(self$edge_list, 1, paste0, collapse = "->")
    }
  )
)
