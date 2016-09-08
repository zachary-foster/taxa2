#' Name class
#'
#' @export
#' @param name (character) any name
#' @param genus (character) genus name
#' @param epithet (character) epithet name
#' @param canonical (character) canonical name
#' @param species (character) genus + epithet
#' @param authority (character) taxonomic authority
#' @param id (character/numeric) a taxonomic ID, such as from a online
#' taxonomic database, or perhaps one created by the user
#' @param src (character) source of the \code{id}, e.g. NCBI
#' @param uri (character) a URI for the taxon, should be that for the
#' source given in \code{src}
#'
#' @return An \code{R6Class} object of class \code{Name}
#'
#' @section Methods:
#' \itemize{
#'  \item print - print method
#'  \item to_json - serialize all data to JSON, useful for writing out
#'  to disk, etc.
#' }
#'
#' @examples
#' (poa <- name("Poa"))
#' (undef <- name("undefined"))
#' (sp1 <- name("species 1"))
#' (poa_annua <- name(genus = "Poa", epithet = "annua"))
#' (poa_annua_l <- name(genus = "Poa", epithet = "annua", authority = "L."))
#' (bison_bison <- name(
#'    genus = "Bison", epithet = "Bison", id = 2441176,
#'    src = "gbif", uri = "http://gbif.org/species/2441176"
#' ))
#' bison_bison$genus
#' bison_bison$name
#' bison_bison$id
#' bison_bison$to_json()
#' bison_bison$to_json(pretty = TRUE)
#'
#' # convert to json
#' poa_annua$to_json()
name <- function(name = NULL, genus = NULL, epithet = NULL,
                 canonical = NULL, species = NULL, authority = NULL,
                 id = NULL, src = NULL, uri = NULL) {
  Name$new(
    name = name,
    genus = genus,
    epithet = epithet,
    canonical = canonical,
    species = species,
    authority = authority,
    id = id,
    src = src,
    uri = uri
  )
}

Name <- R6::R6Class(
  "Name",
  public = list(
    name = NULL,
    genus = NULL,
    epithet = NULL,
    canonical = NULL,
    species = NULL,
    authority = NULL,
    id = NULL,
    src = NULL,
    uri = NULL,

    initialize = function(
      name = NULL, genus = NULL, epithet = NULL,
      canonical = NULL, species = NULL, authority = NULL,
      id = NULL, src = NULL, uri = NULL
    ) {
      self$name <- name
      self$genus <- genus
      self$epithet <- epithet
      self$canonical <- canonical
      self$species <- species
      self$authority <- authority
      self$id <- id
      self$src <- src
      self$uri <- uri
    },

    print = function(indent = "") {
      cat(paste0(indent, "<name>\n"))
      if (!is.null(self$name)) {
        cat(paste0(indent, "  name: ", self$name %||% ""), "\n")
      } else {
        cat(
          paste0(indent, "  name: ",
                 paste(self$genus %||% "",
                       self$epithet %||% "",
                       self$authority %||% "")),
          "\n")
      }
      if (is.null(self$genus) && is.null(self$epithet)) {
        cat(paste0(indent, "  genus / epithet: \n"))
      } else {
        cat(
          sprintf(paste0(indent, "  genus / epithet: %s / %s"),
                  self$genus %||% "", self$epithet %||% ""), "\n")
      }
      invisible(self)
    },

    to_json = function(...){
      jsonlite::toJSON(
        list(
          name = self$name,
          genus = self$genus,
          epithet = self$epithet,
          canonical = self$canonical,
          species = self$species,
          authority = self$authority,
          id = self$id,
          src = self$src,
          uri = self$uri
        ),
        auto_unbox = TRUE, ...
      )
    }
  )
)
