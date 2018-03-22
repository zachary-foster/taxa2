#' Taxon class
#'
#' A class used to define a taxon.
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
    name = name,
    rank = rank,
    id = id,
    authority = authority
  )
}

Taxon <- R6::R6Class(
  "Taxon",

  public = list(
    my_name = NULL,
    my_rank = NULL,
    my_id = NULL,
    my_authority = NULL,

    initialize = function(
      name = NULL, rank = NULL, id = NULL, authority = NULL
    ) {
      assert(name, private$valid_name_classes)
      assert(rank, private$valid_rank_classes)
      assert(id, private$valid_id_classes)
      assert(authority, private$valid_authority_classes)

      # Convert characters to appropriate classes
      if (is.character(name)) {
        name <- taxon_name(name)
      }
      if (is.character(rank)) {
        rank <- taxon_rank(rank)
      }
      if (is.character(id)) {
        id <- taxon_id(id)
      }

      # Save values
      self$my_name <- name
      self$my_rank <- rank
      self$my_id <- id
      self$my_authority <- authority
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Taxon>\n"))
      cat(paste0(indent, paste0("  name: ",
                                self$name %||% "none", "\n")))
      cat(paste0(indent, paste0("  rank: ",
                                self$rank %||% "none", "\n")))
      cat(paste0(indent, paste0("  id: ",
                                self$id %||% "none", "\n")))
      cat(paste0(indent, paste0("  authority: ",
                                self$authority %||% "none", "\n")))
      invisible(self)
    },

    is_empty = function(x) {
      is.null(self$name) && is.null(self$rank) && is.null(self$id)
    }
  ),

  active = list(
    name = function(value) {
      if (missing(value)) { # GET
        return(as.character(self$my_name))
      }
      else { # SET
        private$check_input_class(value, private$valid_name_classes)
        self$my_name <- as.TaxonRank(value)
      }
    },

    rank = function(value) {
      if (missing(value)) { # GET
        return(as.character(self$my_rank))
      }
      else { # SET
        private$check_input_class(value, private$valid_rank_classes)
        self$my_rank <- as.TaxonRank(value)
      }
    },

    id = function(value) {
      if (missing(value)) { # GET
        return(as.character(self$my_id))
      }
      else { # SET
        private$check_input_class(value, private$valid_id_classes)
        self$my_id <- as.TaxonId(value)
      }
    },

    authority = function(value) {
      if (missing(value)) { # GET
        return(self$my_authority)
      }
      else { # SET
        private$check_input_class(value, private$valid_authority_classes)
        self$my_authority <- value
      }
    }
  ),

  private = list(

    valid_name_classes = c("character", "factor", "TaxonName"),
    valid_rank_classes = c("character", "factor", "TaxonRank"),
    valid_id_classes = c("character", "factor", "TaxonId"),
    valid_authority_classes = c("factor", "character"),

    check_input_class = function(input, valid_classes) {
      if (! class(input) %in% valid_classes) {
        stop(call. = FALSE,
             'Input was of type "', class(input),
             '", but only values of the following types are valid:\n  ',
             paste0(valid_classes, collapse = ", "))
      }
    }
  )
)
