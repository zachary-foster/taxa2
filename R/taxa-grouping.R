#' Classification, aka "grouping"
#'
#' @export
#' @param ... key-value pairs of rank and name. See Details.
#'
#' @return An \code{R6Class} object of class \code{Grouping}
#'
#' @details allowed keys:
#' \itemize{
#'  \item superkingdom
#'  \item kingdom
#'  \item subkingdom
#'  \item infrakingdom
#'  \item division
#'  \item phylum
#'  \item subphylum
#'  \item subdivision
#'  \item infradavision
#'  \item superclass
#'  \item class
#'  \item subclass
#'  \item infraclass
#'  \item superorder
#'  \item order
#'  \item suborder
#'  \item infraorder
#'  \item superfamily
#'  \item family
#'  \item subfamily
#'  \item tribe
#'  \item subtribe
#'  \item genus
#'  \item subgenus
#'  \item section
#'  \item subsection
#'  \item species
#'  \item subspecies
#'  \item variety
#'  \item race
#'  \item subvariety
#'  \item stirp
#'  \item morph
#'  \item form
#'  \item aberration
#'  \item subform
#'  \item unspecified
#' }
#'
#' @section Methods:
#' \itemize{
#'  \item print - print method
#'  \item names_non_null - collect all entries that are non-NULL
#'  \item as_df - coerce to a tibble/data.frame
#' }
#'
#' @examples
#' grouping(kingdom = "Animalia")
#'
#' res <- grouping(
#'   kingdom = "Animalia",
#'   phylum = "Chordata",
#'   class = "Mammalia",
#'   order = "Artiodactyla",
#'   family = "Bovidae",
#'   genus = "Bison",
#'   species = "Bison bison"
#' )
#' res
#' res$names_non_null()
#' res$as_df()
#' res$as_df(wide = TRUE)
#'
#' # data.frame
#' data(bison_bison)
#' bison_bison
#' gg <- grouping(bison_bison)
#' gg$names_non_null()
#' gg
grouping <- function(...) {
  UseMethod("grouping")
}

#' @export
grouping.Grouping <- function(...) list(...)[[1]]

#' @export
grouping.character <- function(...) {
  do.call(Grouping$new, list(...))
}

#' @export
grouping.data.frame <- function(...) {
  tmp <- list(...)[[1]]
  xx <- as.list(stats::setNames(tmp$name, tmp$rank))
  xx <- xx[names(xx) != "no rank"]
  do.call(Grouping$new, xx)
}

#' @export
grouping.tbl_df <- function(...) {
  grouping(unclass(...))
}

# grouping.default <- function(...) {
#   stop("no method for ", class(...), call. = FALSE)
# }

Grouping <- R6::R6Class(
  "Grouping",
  public = list(
    superkingdom = NULL,
    kingdom = NULL,
    subkingdom = NULL,
    infrakingdom = NULL,
    division = NULL,
    phylum = NULL,
    subphylum = NULL,
    subdivision = NULL,
    infradavision = NULL,
    superclass = NULL,
    class = NULL,
    subclass = NULL,
    infraclass = NULL,
    superorder = NULL,
    order = NULL,
    suborder = NULL,
    infraorder = NULL,
    superfamily = NULL,
    family = NULL,
    subfamily = NULL,
    tribe = NULL,
    subtribe = NULL,
    genus = NULL,
    subgenus = NULL,
    section = NULL,
    subsection = NULL,
    species = NULL,
    subspecies = NULL,
    variety = NULL,
    race = NULL,
    subvariety = NULL,
    stirp = NULL,
    morph = NULL,
    form = NULL,
    aberration = NULL,
    subform = NULL,
    unspecified = NULL,

    initialize = function(
      superkingdom = NULL, kingdom = NULL, subkingdom = NULL,
      infrakingdom = NULL, division = NULL,
      phylum = NULL, subphylum = NULL, subdivision = NULL, infradavision = NULL,
      superclass = NULL, class = NULL, subclass = NULL, infraclass = NULL,
      superorder = NULL, order = NULL, suborder = NULL, infraorder = NULL,
      superfamily = NULL, family = NULL, subfamily = NULL, tribe = NULL,
      subtribe = NULL, genus = NULL, subgenus = NULL, section = NULL,
      subsection = NULL, species = NULL, subspecies = NULL, variety = NULL,
      race = NULL, subvariety = NULL, stirp = NULL, morph = NULL, form = NULL,
      aberration = NULL, subform = NULL, unspecified = NULL
    ) {

      self$superkingdom <- superkingdom
      self$kingdom <- kingdom
      self$subkingdom <- subkingdom
      self$infrakingdom <- infrakingdom
      self$division <- division
      self$phylum <- phylum
      self$subphylum <- subphylum
      self$subdivision <- subdivision
      self$infradavision <- infradavision
      self$superclass <- superclass
      self$class <- class
      self$subclass <- subclass
      self$infraclass <- infraclass
      self$superorder <- superorder
      self$order <- order
      self$suborder <- suborder
      self$infraorder <- infraorder
      self$superfamily <- superfamily
      self$family <- family
      self$subfamily <- subfamily
      self$tribe <- tribe
      self$subtribe <- subtribe
      self$genus <- genus
      self$subgenus <- subgenus
      self$section <- section
      self$subsection <- subsection
      self$species <- species
      self$subspecies <- subspecies
      self$variety <- variety
      self$race <- race
      self$subvariety <- subvariety
      self$stirp <- stirp
      self$morph <- morph
      self$form <- form
      self$aberration <- aberration
      self$subform <- subform
      self$unspecified <- unspecified
    },

    print = function(indent = "") {
      cat(paste0(indent, "<grouping>\n"))
      xx <- self$names_non_null()
      for (i in seq_along(xx)) {
        cat(sprintf(paste0(indent, "  %s: %s\n"), names(xx)[i], xx[[i]]))
      }
      invisible(self)
    },

    names_non_null = function() {
      ct(
        add_names(
          self$superkingdom, self$kingdom, self$subkingdom, self$infrakingdom,
          self$division, self$phylum, self$subphylum, self$subdivision,
          self$infradavision, self$superclass, self$class, self$subclass,
          self$infraclass, self$superorder, self$order, self$suborder,
          self$infraorder, self$superfamily, self$family, self$subfamily,
          self$tribe, self$subtribe, self$genus, self$subgenus, self$section,
          self$subsection, self$species, self$subspecies, self$variety,
          self$race, self$subvariety, self$stirp, self$morph, self$form,
          self$aberration, self$subform, self$unspecified
        )
      )
    },

    as_df = function(wide = FALSE) {
      if (wide) {
        tibble::as_data_frame(self$names_non_null())
      } else {
        tibble::data_frame(
          rank = names(self$names_non_null()),
          name = unname(unlist(self$names_non_null()))
        )
      }
    }
  )
)
