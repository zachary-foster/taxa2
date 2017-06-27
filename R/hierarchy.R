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
#'       Remove `Taxon` elements by rank name, taxon name or taxon ID. The
#'       change happens in place, so you don't need to assign output to a new
#'       object. returns self - rank_names (character) a vector of rank names
#'     }
#'     \item{`pick(rank_names)`}{
#'       Select `Taxon` elements by rank name, taxon name or taxon ID. The
#'       change happens in place, so you don't need to assign output to a new
#'       object. returns self - rank_names (character) a vector of rank names
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
#'
#' # pick a rank
#' (res <- hierarchy(z, y, x))
#' res$pick("family")

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

      # Convert factors to characters
      fact_input_index <- which(lapply(input, class) == "factor")
      input[fact_input_index] <- lapply(input[fact_input_index], as.character)

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

    pop = function(ranks = NULL, names = NULL, ids = NULL) {
      alldat <- ct(unlist(c(ranks, names, ids), TRUE))
      if (is.null(alldat) || length(alldat) == 0) {
        stop("one of 'ranks', 'names', or 'ids' must be used")
      }
      taxa_rks <- vapply(self$taxa, function(x) x$rank$name, "")
      taxa_nms <- vapply(self$taxa, function(x) x$name$name, "")
      taxa_ids <- vapply(self$taxa, function(x) x$id$id, numeric(1))
      todrop <- which(taxa_rks %in% ranks |
                        taxa_nms %in% names |
                        taxa_ids %in% ids)
      self$taxa[todrop] <- NULL
      private$sort_hierarchy(self$taxa)
      return(self)
    },

    pick = function(ranks = NULL, names = NULL, ids = NULL) {
      alldat <- ct(unlist(c(ranks, names, ids), TRUE))
      if (is.null(alldat) || length(alldat) == 0) {
        stop("one of 'ranks', 'names', or 'ids' must be used")
      }
      taxa_rks <- vapply(self$taxa, function(x) x$rank$name, "")
      taxa_nms <- vapply(self$taxa, function(x) x$name$name, "")
      taxa_ids <- vapply(self$taxa, function(x) x$id$id, numeric(1))
      todrop <- which(!(taxa_rks %in% ranks |
                        taxa_nms %in% names |
                        taxa_ids %in% ids))
      self$taxa[todrop] <- NULL
      private$sort_hierarchy(self$taxa)
      return(self)
    },

    span = function(ranks = NULL, names = NULL, ids = NULL) {
      alldat <- ct(unlist(c(ranks, names, ids), TRUE))
      if (is.null(alldat) || length(alldat) == 0) {
        stop("one of 'ranks', 'names', or 'ids' must be used")
      }

      taxa_rks <- vapply(self$taxa, function(x) x$rank$name, "")

      if (length(ranks) != 0) {
        if (!is.null(attr(ranks[[1]], "operator"))) {
          ranks <- private$make_ranks(ranks[[1]])
        } else {
          # if no operator, names must be length > 1
          if (length(ranks[[1]]$ranks) != 2) {
            stop("if no operator, must pass in 2 names")
          }
          ranks <- private$do_ranks(ranks[[1]]$ranks)
        }
      }
      if (length(names) != 0) {
        if (!is.null(attr(names[[1]], "operator"))) {
          ranks <- private$do_ranks(private$taxa2rank(names[[1]]))
        } else {
          # if no operator, names must be length > 1
          if (length(names[[1]]$names) != 2) {
            stop("if no operator, must pass in 2 names")
          }
          ranks <- private$do_ranks(private$taxa2rank(names[[1]]$names))
        }
      }
      if (length(ids) != 0) {
        if (!is.null(attr(ids[[1]], "operator"))) {
          ranks <- private$do_ranks(private$ids2rank(ids[[1]]))
        } else {
          # if no operator, names must be length > 1
          if (length(ids[[1]]$ids) != 2) {
            stop("if no operator, must pass in 2 names")
          }
          ranks <- private$do_ranks(private$ids2rank(ids[[1]]$ids))
        }
      }

      todrop <- which(!taxa_rks %in% unique(ranks))
      self$taxa[todrop] <- NULL
      private$sort_hierarchy(self$taxa)
      return(self)
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
    },

    do_ranks = function(x) {
      idz <- vapply(x, which_ranks, numeric(1), USE.NAMES = FALSE)
      keep <- ranks_ref[ranks_ref$rankid >= idz[1] &
                          ranks_ref$rankid <= idz[2], ]
      csep2vec(keep$ranks)
    },

    make_ranks = function(x) {
      idz <- vapply(x$ranks, which_ranks, numeric(1), USE.NAMES = FALSE)
      op <- attr(x, "operator")
      keep <- switch(op,
        `:` = {
          ranks_ref[ranks_ref$rankid >= idz[1] &
                      ranks_ref$rankid <= idz[2], ]
        },
        `::` = {
          ranks_ref[ranks_ref$rankid > idz[1] &
                      ranks_ref$rankid < idz[2], ]
        },
        `. <` = {
          ranks_ref[ranks_ref$rankid <= idz[2], ]
        },
        `. >` = {
          ranks_ref[ranks_ref$rankid > idz[1], ]
        },
        stop("operator ", op, " not supported - see ?`lazy-helpers`", call. = FALSE)
      )
      csep2vec(keep$ranks)
    },

    taxa2rank = function(x) {
      tmp <- vapply(self$taxa, function(z) z$name$name, "")
      rcks <- vapply(self$taxa, function(z) z$rank$name, "")
      rcks[which(tmp %in% x)]
    },

    ids2rank = function(w) {
      tmp <- vapply(self$taxa, function(z) z$id$id, 1)
      rcks <- vapply(self$taxa, function(z) z$rank$name, "")
      rcks[which(tmp %in% w)]
    }
  )
)

which_ranks <- function(x) {
  as.numeric(ranks_ref[which(sapply(ranks_ref$ranks, function(z) {
    any(unlist(strsplit(z, split = ",")) == x)
  }, USE.NAMES = FALSE)), "rankid"])
}
