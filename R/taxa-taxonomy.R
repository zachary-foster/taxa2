#' Taxonomy class
#'
#' Used to store taxonomic tree structures.
#'
#' @export
#' @param ... Any number of object of class \code{\link{hierarchy}}
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
    edge_list = NULL, # Note: this should be made of taxon ids, not indexes, for consistency between subsets

    initialize = function(...) {
      private$unique_taxa(list(...))
      private$make_edge_list(list(...))
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Taxonomy>\n"))
      taxon_names <- vapply(self$taxa, function(x) x$name$name, character(1))
      taxon_ids <- names(self$taxa)
      limited_print(paste(taxon_ids, taxon_names, sep = ". "),
                    prefix = paste0(indent, "  ", length(self$taxa), " taxa:"))
      limited_print(private$make_graph(),
                    prefix = paste0(indent, "  ", nrow(self$edge_list), " edges:"))
      invisible(self)
    },

    supertaxa = function(subset = NULL, recursive = TRUE, simplify = FALSE,
                         include_input = FALSE, index = FALSE, na = FALSE) {
      # Parse arguments
      subset <- format_taxon_subset(names(self$taxa), subset)

      # Get supertaxa
      parent_index <- match(self$edge_list$from, self$edge_list$to) # precomputing makes it much faster
      recursive_part <- function(taxon) {
        supertaxon <- parent_index[taxon]
        if (recursive) {
          if (is.na(supertaxon)) {
            output <- c(taxon, supertaxon)
          } else {
            output <- c(taxon, recursive_part(supertaxon))
          }
        } else {
          output <- c(taxon, supertaxon)
        }
        return(unname(output))
      }
      output <- lapply(subset, recursive_part)

      # Remove query taxa from output
      if (! include_input) {
        output <- lapply(output, `[`, -1)
      }

      # Remove NAs from output
      if (! na) {
        output <- lapply(output, function(x) x[!is.na(x)])
      }

      # Convert to taxon_ids
      if (! index) {
        output <- lapply(output, function(x) names(self$taxa)[x])
      }

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
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



#' @export
supertaxa <- function(obj, ...) {
  UseMethod("supertaxa")
}

#' @export
supertaxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
supertaxa.Taxonomy <- function(obj, ...) {
  obj$supertaxa(...)
}


#' Get all supertaxa of a taxon
#'
#' Return the taxon IDs or indexes of all supertaxa (i.e. all taxa the target taxa
#' are a part of) in an object of type \code{\link{taxonomy}} or \code{\link{taxmap}}.
#' \preformatted{
#' obj$supertaxa(subset = NULL, recursive = TRUE,
#'               simplify = FALSE, include_input = FALSE,
#'               index = FALSE, na = FALSE)
#' supertaxa(obj, ...)}
#'
#' @param obj The \code{taxonomy} or \code{taxmap} object containing taxon information to be
#'   queried.
#' @param subset (\code{character}) \code{taxon_ids} or indexes of \code{taxon_data} for which
#'   supertaxa will be returned. Default: All taxa in \code{obj} will be used.
#' @param recursive (\code{logical}) If \code{FALSE}, only return the supertaxa one level above the
#'   target taxa. If \code{TRUE}, return all the supertaxa of every supertaxa, etc.
#' @param simplify (\code{logical}) If \code{TRUE}, then combine all the results into a single
#'   vector of unique values.
#' @param include_input (\code{logical}) If \code{TRUE}, the input taxa are included in the output
#' @param index (\code{logical}) If \code{TRUE}, return the indexes of supertaxa in
#'   \code{taxon_data} instead of \code{taxon_ids}
#' @param na (\code{logical}) If \code{TRUE}, return \code{NA} where information is not available.
#' @param ... Used to pass the parameters to the R6 method of \code{\link{taxonomy}}
#'
#' @return If \code{simplify = FALSE}, then a list of vectors are returned corresponding to the
#'   \code{subset} argument. If \code{simplify = TRUE}, then unique values are returned in a single
#'   vector.
#'
#' @family taxmap taxonomy functions
#'
#' @name supertaxa
NULL
