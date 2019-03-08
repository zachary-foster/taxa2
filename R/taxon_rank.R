#' Taxon rank class
#'
#' Stores the rank of a taxon. This is typically used to store where taxon
#' information came from in [taxon()] objects.
#'
#' @export
#' @param name (character) rank name. required
#' @param database (character) database class object, optional
#'
#' @return An `R6Class` object of class `TaxonRank`
#' @family classes
#'
#' @examples
#' taxon_rank("species")
#' taxon_rank("genus")
#' taxon_rank("kingdom")
#'
#' (x <- taxon_rank(
#'   "species",
#'   database_list$ncbi
#' ))
#' x$rank
#' x$database
#'
#' # a null taxon_name object
#' taxon_name(NULL)
taxon_rank <- function(name, database = NULL) {
  TaxonRank$new(
    name = name,
    database = database
  )
}

TaxonRank <- R6::R6Class(
  "TaxonRank",
  public = list(

    initialize = function(name = NULL, database = NULL) {
      self$database <- database
      self$name <- name
    },

    print = function(indent = "") {
      cat(paste0(indent, sprintf("<TaxonRank> %s\n", char_or_placeholder(self$name))))
      cat(paste0(indent, paste0("  database: ", char_or_placeholder(self$database), "\n")))
      invisible(self)
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
          check_arg_class(value, c("character", "TaxonRank", "numeric", "factor", "integer"), "rank name")
          private$my_name <- as.character(value)
        }
      }
    },

    database = function(value) {
      if (missing(value)) { # GET
        return(private$my_database)
      }
      else { # SET
        if (is.null(value)) {
          private$my_database <- NULL
        } else {
          private$my_database <- as_TaxonDatabase(value)
        }
      }
    }

  ),

  private = list(
    my_name = NULL,
    my_database = NULL
  )
)


#' @export
as.character.TaxonRank <- function(obj) {
  as.character(obj$name)
}


#' @export
as.TaxonRank <- function(input) {
  if ("TaxonRank" %in% class(input)) {
    return(input)
  } else {
    return(taxon_rank(input))
  }
}


#' @export
as_TaxonRank <- as.TaxonRank