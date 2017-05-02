#' TaxonId class
#'
#' @export
#' @param id (character) a taxonomic id required
#' @param database (character) database class object, optional
#'
#' @return An `R6Class` object of class `TaxonId`
#'
#' @section Methods:
#' \itemize{
#'  \item print - print method
#' }
#'
#' @examples
#' (x <- taxon_id(12345))
#' x$id
#' x$database
#'
#' (x <- taxon_id(
#'   12345,
#'   database_list$ncbi
#' ))
#' x$id
#' x$database
taxon_id <- function(id, database = NULL) {
  TaxonId$new(
    id = id,
    database = database
  )
}

TaxonId <- R6::R6Class(
  "TaxonId",
  public = list(
    id = NULL,
    database = NULL,

    initialize = function(
      id = NULL, database = NULL
    ) {
      self$id <- id
      self$database <- database
    },

    print = function(indent = "") {
      cat(paste0(indent, sprintf("<TaxonId> %s\n", self$id)))
      cat(paste0(indent, paste0("  database: ",
                                get_database_name(self$database) %||% "none",
                                "\n")))
      invisible(self)
    }
  )
)
