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
    uniqtaxa = NULL,
    edgelists = NULL,
    edgelist = NULL,
    graph = NULL,

    initialize = function(...) {
      self$taxa <- list(...)
      private$get_unique_taxa(list(...))
      private$edge_lists(list(...))
      private$edge_list()
      private$make_graph()
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Taxonomy>\n"))
      cat(paste0("  no. hierarchies: ", length(self$taxa)), "\n")
      cat(paste0("  no. unique taxa: ", length(self$uniqtaxa)), "\n")
      cat(paste0("  graph: ", paste0(self$graph, collapse = " ")), "\n")
      invisible(self)
    }
  ),

  private = list(
    get_unique_taxa = function(x) {
      tx <- unlist(lapply(x, function(z) {
        z$taxa
      }))
      utx <- unique(tx)
      self$uniqtaxa <- stats::setNames(utx, seq_along(utx))
    },

    edge_lists = function(x) {
      self$edgelists <- lapply(x, function(z) {
        tmp <- as.numeric(vapply(z$taxa, function(w) {
          names(which(vapply(self$uniqtaxa, function(n) identical(n, w), logical(1))))
        }, ""))
        res <- tibble::data_frame(to = NA_integer_, from = NA_integer_)
        for (i in 1:(length(tmp) - 1)) {
          res[i,] <- c(tmp[c(i, i + 1)])
        }
        res
      })
    },

    edge_list = function() {
      self$edgelist <- unique(dplyr::bind_rows(self$edgelists))
    },

    make_graph = function() {
      self$graph <- apply(self$edgelist, 1, paste0, collapse = "->")
    }
  )
)
