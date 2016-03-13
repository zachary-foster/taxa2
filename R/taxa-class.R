#' binomial
#'
#' @export
#' @param genus genus name
#' @param epithet epithet name
#' @param canonical canonical name
#' @param species genus + epithet
#' @param authority taxonomic authority
#' @examples
#' poa <- binomial("Poa")
#' poa$tojson()
binomial <- function(genus, epithet = NULL, canonical = NULL, species = NULL, authority = NULL){
  Binomial$new(genus = genus,
               epithet = epithet,
               canonical = canonical,
               species = species,
               authority = authority)
}

Binomial <- R6::R6Class("Binomial",
  public = list(
    genus = NULL,
    epithet = NULL,
    canonical = NULL,
    species = NULL,
    authority = NULL,

    initialize = function(genus = NULL, epithet = NULL, canonical = NULL, species = NULL, authority = NULL){
      self$genus <- genus
      self$epithet <- epithet
      self$canonical <- canonical
      self$species <- species
      self$authority <- authority
    },

    print = function() {
      cat("<binomial> ", "\n")
      cat(paste0("  taxon: ", paste(self$genus, self$epithet)), "\n")
      cat(paste0("  genus: ", self$genus), "\n")
      cat(paste0("  epithet: ", self$epithet), "\n")
      invisible(self)
    },

    tojson = function(...){
      jsonlite::toJSON(list(genus = self$genus,
                            epithet = self$epithet,
                            canonical = self$canonical,
                            species = self$species,
                            authority = self$authority), auto_unbox = TRUE, ...)
    }
  )
)

#' taxonref
#'
#' @export
#' @examples
#' Taxonref$new("genus", "Poa", 54, "http://scottchamberlain.info/")
Taxonref <- R6::R6Class("Taxonref",
    public = list(
      rank = NULL,
      name = NULL,
      id = NULL,
      uri = NULL,

      initialize = function(rank = NULL, name = NULL, id = NULL, uri = NULL){
        self$rank <- rank
        self$name <- name
        self$id <- id
        self$uri <- uri
      }
    )
)

#' Classification, or grouping
#'
#' @export
#' @examples
#' Grouping$new()
Grouping <- R6::R6Class("Grouping",
  public = list(
    kingdom = NULL,
    subkingdom = NULL,
    infrakingdom = NULL,
    division = NULL,
    phylum = NULL,
    subdivision = NULL,
    infradavision = NULL,
    superclass = NULL,
    clazz = NULL,
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

    initialize = function(kingdom = NULL, subkingdom = NULL, infrakingdom = NULL, division = NULL,
        phylum = NULL, subdivision = NULL, infradavision = NULL, superclass = NULL, clazz = NULL,
        subclass = NULL, infraclass = NULL, superorder = NULL, order = NULL, suborder = NULL,
        infraorder = NULL, superfamily = NULL, family = NULL, subfamily = NULL, tribe = NULL,
        subtribe = NULL, genus = NULL, subgenus = NULL, section = NULL, subsection = NULL,
        species = NULL, subspecies = NULL, variety = NULL, race = NULL, subvariety = NULL,
        stirp = NULL, morph = NULL, form = NULL, aberration = NULL, subform = NULL, unspecified = NULL){
      self$kingdom <- kingdom
      self$subkingdom <- subkingdom
      self$infrakingdom <- infrakingdom
      self$division <- division
      self$phylum <- phylum
      self$subdivision <- subdivision
      self$infradavision <- infradavision
      self$superclass <- superclass
      self$clazz <- clazz
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
    }
  )
)

#' taxon
#'
#' @export
#' @examples
#' #maketax("Poa", Grouping$new(genus = 'Poa'))
Taxon <- R6::R6Class("Taxon",
  public = list(
    binomial = NULL,
    classification = NULL,
    x = NULL,

    initialize = function(binomial = NULL, grouping = NULL){
      self$binomial <- binomial
      self$grouping <- grouping
    },

    hier = function(){
      tmp <- self$grouping
      names(tmp)
#       nn <- slotNames(tmp)[-1]
#       vals <- vapply(nn, function(g) slot(tmp, g)@name, "", USE.NAMES = FALSE)
#       data.frame(rank=nn, value=vals, stringsAsFactors = FALSE)
    }
#     ,
#     `[[` = function(x) {
#       self$classification[[x]]
#     }
  )
)

maketax <- function(binomial, grouping){
  res <- Taxon$new(binomial = binomial, grouping = grouping)
  res
  #structure(res, class = c("Taxon", "stuff"))
}

# foo <- function(x) UseMethod("foo")
# foo.stuff <- function(x) x

# `[[`.Taxon <- function(x, y) {
#   x$classification[y]
# }
