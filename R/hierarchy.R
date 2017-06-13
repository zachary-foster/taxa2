#' The Hierarchy class
#'
#' A class containing an ordered list of [taxon()] objects that represent a
#' hierachical classification.
#'
#' @export
#' @param ... Any number of object of class `Taxon` or taxonomic names as
#' character strings
#' @return An `R6Class` object of class `Hierarchy`
#'
#' @details On initialization, taxa are sorted if they have ranks with a known
#'   order.
#'
#' **Methods**
#'   \describe{
#'     \item{`pop(rank_names)`}{
#'       Remove `Taxon` elements by rank name. The change happens in place,
#'       so you don't need to assign output to a new object. returns self
#'       - rank_names (character) a vector of rank names
#'     }
#'   }
#'
#' @family classes
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
#'
#' # pop off a rank
#' res$pop("family")

hierarchy <- function(...) {
  Hierarchy$new(...)
}

Hierarchy <- R6::R6Class(
  "Hierarchy",
  lock_objects = TRUE,
  public = list(
    taxa = NULL,
    ranklist = NULL,

    initialize = function(...) {
      input <- unlist(list(...))

      if (!all(vapply(input, function(x)
        any(class(x) %in% c('character', 'Taxon')), logical(1)))
      ) {
        stop(
          "all inputs to 'hierarchy' must be of class 'Taxon' or 'character'",
          call. = FALSE)
      }

      # If character strings are supplied, convert to taxa
      char_input_index <- which(lapply(input, class) == "character")
      input[char_input_index] <- lapply(input[char_input_index], taxon)

      # Parse input
      all_have_ranks <- all(vapply(input,
                                   function(x) !is.null(x$rank$name),
                                   logical(1)))
      if (all_have_ranks) {
        self$taxa <- private$sort_hierarchy(input)
      } else {
        self$taxa <- input
      }
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Hierarchy>\n"))
      if (length(self$taxa) > 0) {
        cat("  no. taxon's: ", length(self$taxa), "\n")
        for (i in seq_along(self$taxa[1:min(10, length(self$taxa))])) {
          cat(
            sprintf("  %s / %s / %s",
                    self$taxa[[i]]$name$name %||% "",
                    self$taxa[[i]]$rank$name %||% "",
                    self$taxa[[i]]$id$id %||% ""
            ), "\n")
        }
        if (length(self$taxa) > 10) cat("  ...")
      } else {
        cat("  Empty hierarchy")
      }
      invisible(self)
    },

    pop = function(rank_names) {
      taxa_rks <- vapply(self$taxa, function(x) x$rank$name, "")
      todrop <- which(taxa_rks %in% rank_names)
      self$taxa[todrop] <- NULL
      self$ranklist[names(self$ranklist) %in% rank_names] <- NULL
      self
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
