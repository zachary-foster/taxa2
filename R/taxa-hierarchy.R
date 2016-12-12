#' Hierarchy class
#'
#' @export
#' @param ... Any number of object of class \code{Taxon}
#' @return An \code{R6Class} object of class \code{Hierarchy}
#'
#' @details on initialize, function sorts the taxon list, see
#' \code{\link{ranks_ref}} for the reference rank names and orders
#'
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
#' res$taxa
#' res$ranklist

hierarchy <- function(...) {
  Hierarchy$new(...)
}

Hierarchy <- R6::R6Class(
  "Hierarchy",
  public = list(
    taxa = NULL,
    ranklist = NULL,

    initialize = function(...) {
      self$taxa <- private$sort_hierarchy(list(...))
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Hierarchy>\n"))
      cat("  no. taxon's: ", length(self$taxa), "\n")
      if (length(self$taxa) > 0) {
        for (i in seq_along(self$taxa[1:min(10, length(self$taxa))])) {
          cat(
            sprintf("  %s / %s / %s",
                    self$taxa[[i]]$name$name %||% "",
                    self$taxa[[i]]$rank$name %||% "",
                    self$taxa[[i]]$id$id %||% ""
            ), "\n")
        }
        if (length(self$taxa) > 10) cat("  ...")
      }
      invisible(self)
    }
  ),

  private = list(
    sort_hierarchy = function(x) {
      if (length(x) == 0) {
        return(x)
      }
      ranks <- tolower(vapply(x, function(z) z$rank$name, ""))
      # check that each rank is in the acceptable set
      invisible(lapply(ranks, function(z) {
        if (!z %in% private$poss_ranks()) {
          stop(z, " not in the acceptable set of rank names", call. = FALSE)
        }
      }))
      self$ranklist <- as.list(vapply(ranks, which_ranks, numeric(1)))
      x[order(unname(unlist(self$ranklist)))]
    },

    poss_ranks = function() {
      unique(
        do.call(
          c,
          sapply(ranks_ref$ranks, strsplit, split = ",", USE.NAMES = FALSE)
        )
      )
    }
  )
)

which_ranks <- function(x) {
  as.numeric(ranks_ref[which(sapply(ranks_ref$ranks, function(z) {
    any(unlist(strsplit(z, split = ",")) == x)
  }, USE.NAMES = FALSE)), "rankid"])
}
