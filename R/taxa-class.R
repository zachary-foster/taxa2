#' taxon
#'
#' @export
#' @param name An object of class \code{Name}. Required.
#' @param grouping An object of class \code{Grouping}. Required.
#' @param ... One or more objects of class \code{Taxon}
#'
#' @return An \code{R6Class} object of class \code{Taxon} for function
#' \code{taxon}, and an object of class \code{Taxa} for function \code{taxa}
#'
#' @section Methods:
#' \itemize{
#'  \item print - print method, both \code{taxon} and \code{taxa}
#'  \item as_df - coerce all groupings to a tibble/data.frame, only \code{taxa}
#' }
#'
#' @examples
#' # a taxon
#' nm1 <- name("Bison bison")
#' gr1 <- grouping(
#'   kingdom = "Animalia",
#'   family = "Bovidae",
#'   species = "Bison bison"
#' )
#' (t1 <- taxon(nm1, gr1))
#'
#' # another taxon
#' nm2 <- name("Poa annua")
#' gr2 <- grouping(
#'   kingdom = "Plantae",
#'   family = "Poaceae",
#'   species = "Poa annua"
#' )
#' (t2 <- taxon(nm2, gr2))
#'
#' # a taxa object
#' t3 <- taxons(t1, t2)
#' t3$taxa
#' t3$taxa[[1]]
#' t3$taxa[[2]]
#' t3$as_df()
#' t3$taxa[[2]]$name
#' t3$taxa[[2]]$grouping
#' t3$taxa[[2]]$grouping$as_df()

taxon <- function(name, grouping) {
  Taxon$new(
    name = name,
    grouping = grouping
  )
}

Taxon <- R6::R6Class(
  "Taxon",
  public = list(
    name = NULL,
    grouping = NULL,

    initialize = function(name = NULL, grouping = NULL){
      self$name <- name
      self$grouping <- grouping
    },

    print = function() {
      cat("<taxon>\n")
      print(self$name, indent = "  ")
      print(self$grouping, indent = "  ")
      invisible(self)
    }
  )
)

#' @export
#' @rdname taxon
taxons <- function(...) Taxa$new(...)

Taxa <- R6::R6Class(
  "Taxa",
  public = list(
    taxa = NULL,

    initialize = function(...){
      self$taxa <- list(...)
    },

    print = function() {
      cat("<taxa>\n")
      for (i in seq_along(self$taxa)) {
        cat(
          "  name:",
          self$taxa[[i]]$name$name,
          "- no. grouping: ",
          length(self$taxa[[i]]$grouping$names_non_null()),
          "\n"
        )
      }
      invisible(self)
    },

    as_df = function() {
      res <- list()
      for (i in seq_along(self$taxa)) {
        df <- self$taxa[[i]]$grouping$as_df()
        df$taxon <- self$taxa[[i]]$name$name
        res[[i]] <- df
      }
      return(do.call("rbind.data.frame", res))
    }
  )
)
