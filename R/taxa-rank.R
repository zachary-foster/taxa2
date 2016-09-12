#' TaxonRank class
#'
#' @export
#' @param name (character) rank name. required
#' @param database (character) database class object, optional
#'
#' @return An \code{R6Class} object of class \code{TaxonRank}
#'
#' @section Methods:
#' \itemize{
#'  \item print - print method
#' }
#'
#' @examples
#' taxon_rank("species")
#' taxon_rank("genus")
#' taxon_rank("kingdom")
#' # taxon_rank("stuff")  # should fail
#'
#' (x <- taxon_rank(
#'   "species",
#'   database_list$ncbi
#' ))
#' x$rank
#' x$database
taxon_rank <- function(name, database = NULL) {
  TaxonRank$new(
    name = name,
    database = database
  )
}

TaxonRank <- R6::R6Class(
  "TaxonRank",
  public = list(
    name = NULL,
    database = NULL,

    initialize = function(
      name = NULL, database = NULL
    ) {
      self$name <- name
      self$database <- database
    },

    print = function(indent = "") {
      cat(paste0(indent, sprintf("<TaxonRank> %s\n", self$name)))
      cat(paste0(indent, paste0("  database: ", self$database$name %||% "none", "\n")))
      invisible(self)
    }
  )
)
