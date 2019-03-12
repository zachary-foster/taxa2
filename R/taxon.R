#' Taxon class
#'
#' A class used to define a single taxon. Most other classes in the taxa package
#' include one or more objects of this class.
#'
#' @export
#' @param name a TaxonName object [taxon_name()] or character string. if character
#' passed in, we'll coerce to a TaxonName object internally, required
#' @param rank a TaxonRank object [taxon_rank()] or character string. if character
#' passed in, we'll coerce to a TaxonRank object internally, required
#' @param id a TaxonId object [taxon_id()], numeric/integer, or character string.
#' if numeric/integer/character passed in, we'll coerce to a TaxonId object
#' internally, required
#' @param authority (character) a character string, optional
#'
#' @details Note that there is a special use case of this function - you can
#' pass `NULL` as the first parameter to get an empty `taxon` object. It makes
#' sense to retain the original behavior where nothing passed in to the first
#' parameter leads to an error, and thus creating a `NULL` taxon is done very
#' explicitly.
#'
#' @return An `R6Class` object of class `Taxon`
#' @family classes
#'
#' @examples
#' (x <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' ))
#' x$name
#' x$rank
#' x$id
#'
#' # a null taxon object
#' taxon(NULL)
#' ## with all NULL objects from the other classes
#' taxon(
#'   name = taxon_name(NULL),
#'   rank = taxon_rank(NULL),
#'   id = taxon_id(NULL)
#' )
taxon <- function(name, rank = NULL, id = NULL, authority = NULL) {
  Taxon$new(
    name = clone_if_r6(name),
    rank = clone_if_r6(rank),
    id = clone_if_r6(id),
    authority = clone_if_r6(authority)
  )
}

#' @export
Taxon <- R6::R6Class(
  "Taxon",

  public = list(

    initialize = function(name = NULL, rank = NULL, id = NULL, authority = NULL) {
      self$name <- name
      self$rank <- rank
      self$id <- id
      self$authority <- authority
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Taxon>\n"))
      cat(paste0(indent, paste0("  name: ", char_or_placeholder(self$name), "\n")))
      cat(paste0(indent, paste0("  rank: ", char_or_placeholder(self$rank), "\n")))
      cat(paste0(indent, paste0("  id: ", char_or_placeholder(self$id), "\n")))
      cat(paste0(indent, paste0("  authority: ", char_or_placeholder(self$authority), "\n")))
      invisible(self)
    },

    is_empty = function(x) {
      is.null(self$name) && is.null(self$rank) && is.null(self$id)
    }
  ),

  active = list(
    name = function(value) {
      if (missing(value)) { # GET
        return(private$my_name)
      }
      else { # SET
        if (is.null(value)) {
          private$my_name <- NULL
        } else {
          private$my_name <- as_TaxonName(value)
        }
      }
    },

    rank = function(value) {
      if (missing(value)) { # GET
        return(private$my_rank)
      }
      else { # SET
        if (is.null(value)) {
          private$my_rank <- NULL
        } else {
          private$my_rank <- as_TaxonRank(value)
        }
      }
    },

    id = function(value) {
      if (missing(value)) { # GET
        return(private$my_id)
      }
      else { # SET
        if (is.null(value)) {
          private$my_id <- NULL
        } else {
          private$my_id <- as_TaxonId(value)
        }
      }
    },

    authority = function(value) {
      if (missing(value)) { # GET
        return(private$my_authority)
      }
      else { # SET
        if (is.null(value)) {
          private$my_authority <- NULL
        } else {
          check_arg_class(value, c("character", "numeric", "factor", "integer"), "authority")
          private$my_authority <- as.character(value)
        }
      }
    }

  ),

  private = list(
    my_name = NULL,
    my_rank = NULL,
    my_id = NULL,
    my_authority = NULL
  )
)


#' @export
as.character.Taxon <- function(obj) {
  as.character(obj$name)
}

#' @export
as.Taxon <- function(input) {
  if ("Taxon" %in% class(input)) {
    return(input)
  } else {
    return(taxon(input))
  }
}

#' @export
as_Taxon <- as.Taxon
