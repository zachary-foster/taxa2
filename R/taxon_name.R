#' TaxonName class
#'
#' @export
#' @param name (character) a taxonomic name. required
#' @param database (character) database class object, optional
#'
#' @return An \code{R6Class} object of class \code{TaxonName}
#'
#' @section Methods:
#' \itemize{
#'  \item print - print method
#' }
#'
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
taxon_name <- function(name, database = NULL) {
  TaxonName$new(
    name = name,
    database = database
  )
}

TaxonName <- R6::R6Class(
  "TaxonName",
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
      cat(paste0(indent, sprintf("<TaxonName> %s\n", self$name)))
      cat(paste0(indent, paste0("  database: ", self$database$name %||% "none", "\n")))
      invisible(self)
    }
  )
)
