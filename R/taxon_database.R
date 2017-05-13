#' TaxonDatabase class
#'
#' @export
#' @param name (character) name of the database
#' @param url (character) url for the database
#' @param description (character) description of the database
#' @param id_regex (character) id regex
#'
#' @return An `R6Class` object of class `TaxonDatabase`
#'
#' @examples
#' # create a database entry
#' (x <- taxon_database(
#'   "ncbi",
#'   "http://www.ncbi.nlm.nih.gov/taxonomy",
#'   "NCBI Taxonomy Database",
#'   "*"
#' ))
#' x$name
#' x$url
#'
#' # use pre-created database objects
#' database_list
#' database_list$ncbi
taxon_database <- function(name = NULL, url = NULL, description = NULL,
                     id_regex = NULL) {
  TaxonDatabase$new(
    name = name,
    url = url,
    description = description,
    id_regex = id_regex
  )
}

TaxonDatabase <- R6::R6Class(
  "TaxonDatabase",
  public = list(
    name = NULL,
    url = NULL,
    description = NULL,
    id_regex = NULL,

    initialize = function(
      name = NULL, url = NULL, description = NULL, id_regex = NULL
    ) {
      self$name <- name
      self$url <- url
      self$description <- description
      self$id_regex <- id_regex
    },

    print = function(indent = "") {
      cat(paste0(indent, sprintf("<database> %s\n", self$name)))
      cat(paste0(indent, paste0("  url: ", self$url, "\n")))
      cat(paste0(indent, paste0("  description: ", self$description, "\n")))
      cat(paste0(indent, paste0("  id regex: ", self$id_regex, "\n")))
      invisible(self)
    }
  )
)

#' database list
#' @rdname database
#' @export
database_list <- list(
  ncbi = taxon_database(
    "ncbi",
    "http://www.ncbi.nlm.nih.gov/taxonomy",
    "NCBI Taxonomy Database",
    ".*"
  ),

  gbif = taxon_database(
    "gbif",
    "http://www.gbif.org/developer/species",
    "GBIF Taxonomic Backbone",
    ".*"
  ),

  bold = taxon_database(
    "bold",
    "http://www.boldsystems.org",
    "Barcode of Life",
    ".*"
  ),

  col = taxon_database(
    "col",
    "http://www.catalogueoflife.org",
    "Catalogue of Life",
    ".*"
  ),

  eol = taxon_database(
    "eol",
    "http://eol.org",
    "Encyclopedia of Life",
    ".*"
  ),

  nbn = taxon_database(
    "nbn",
    "https://nbn.org.uk",
    "UK National Biodiversity Network",
    ".*"
  ),

  tps = taxon_database(
    "tps",
    "http://www.tropicos.org/",
    "Tropicos",
    ".*"
  ),

  itis = taxon_database(
    "itis",
    "http://www.itis.gov",
    "Integrated Taxonomic Information System",
    ".*"
  )
)
