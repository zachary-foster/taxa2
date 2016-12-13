#' Taxon class
#'
#' @export
#' @param name (character) an TaxonName object or character. if character
#' passed in, we'll coerce to a TaxonName object internally, required
#' @param rank (character) an TaxonRank object. if character
#' passed in, we'll coerce to a TaxonRank object internally, required
#' @param id (character) an TaxonId object. if character
#' passed in, we'll coerce to a TaxonId object internally, required
#' @param authority (character) an TaxonAuthority object, optional
#'
#' @return An \code{R6Class} object of class \code{Taxon}
#'
#' @section Methods:
#' \itemize{
#'  \item print - print method
#' }
#'
#' @examples
#' (x <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' ))
#' x$name
#' x$id
#' x$database
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
    name = NULL,
    rank = NULL,
    id = NULL,
    authority = NULL,

    initialize = function(
      name = NULL, rank = NULL, id = NULL, authority = NULL
    ) {
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

      self$name <- name
      self$rank <- rank
      self$id <- id
      self$authority <- authority
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Taxon>\n"))
      cat(paste0(indent, paste0("  name: ", self$name$name %||% "none", "\n")))
      cat(paste0(indent, paste0("  rank: ", self$rank$name %||% "none", "\n")))
      cat(paste0(indent, paste0("  id: ", self$id$id %||% "none", "\n")))
      cat(paste0(indent, paste0("  authority: ", self$authority$name %||% "none", "\n")))
      invisible(self)
    }
  )
)
