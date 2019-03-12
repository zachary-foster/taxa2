#' Taxon name class
#'
#' Used to store the name of taxa. This is typically used to
#' store where taxon names in [taxon()] objects.
#'
#' @export
#' @param name (character) a taxonomic name. required
#' @param database (character) database class object, optional
#'
#' @return An `R6Class` object of class `TaxonName`
#'
#' @family classes
#' @examples
#' (poa <- taxon_name("Poa"))
#' (undef <- taxon_name("undefined"))
#' (sp1 <- taxon_name("species 1"))
#' (poa_annua <- taxon_name("Poa annua"))
#' (x <- taxon_name("Poa annua L."))
#'
#' x$name
#' x$database
#'
#' (x <- taxon_name(
#'   "Poa annua",
#'   database_list$ncbi
#' ))
#' x$rank
#' x$database
#'
#' # a null taxon_name object
#' taxon_name(NULL)
taxon_name <- function(name, database = NULL) {
  database <- clone_if_r6(database)
  TaxonName$new(name = name, database = database)
}

#' @export
TaxonName <- R6::R6Class(
  "TaxonName",
  public = list(

    initialize = function(name = NULL, database = NULL) {
      self$database <- database
      self$name <- name
    },

    print = function(indent = "") {
      cat(paste0(indent, sprintf("<TaxonName> %s\n", char_or_placeholder(self$name))))
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
          check_arg_class(value, c("character", "TaxonName", "numeric", "factor", "integer", "Taxon"), "taxon name")
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
as.character.TaxonName <- function(obj) {
  as.character(obj$name)
}

#' @export
as.TaxonName <- function(input) {
  if ("TaxonName" %in% class(input)) {
    return(input)
  } else {
    return(taxon_name(input))
  }
}

#' @export
as_TaxonName <- as.TaxonName
