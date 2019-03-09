#' A class for multiple taxon objects
#'
#' Stores one or more [taxon()] objects. This is just a thin wrapper for a list
#' of [taxon()] objects.
#'
#' This is the documentation for the class called `taxa`. If you are looking for
#' the documentation for the package as a whole: [taxa-package].
#'
#' @export
#' @param ... Any number of object of class [taxon()]
#' @param .list An alternate to the `...` input. Any number of object of class
#'   [taxon()]. Cannot be used with `...`.
#' @return An `R6Class` object of class `Taxon`
#' @family classes
#' @examples
#' catus <- taxon(
#'   name = taxon_name("catus"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(9685)
#' )
#' tigris <- taxon(
#'   name = taxon_name("tigris"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(9696)
#' )
#' sapiens <- taxon(
#'   name = taxon_name("sapiens"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(9606)
#' )
#' (x <- taxa(catus, tigris, sapiens))
#'
#' # a null set
#' x <- taxon(NULL)
#' taxa(x, x, x)
#'
#' # combo non-null and null
#' taxa(catus, x, sapiens)
taxa <- function(..., .list = NULL) {
  inputs <- get_dots_or_list(..., .list = .list)
  inputs <- lapply(inputs, function(x) {
    if (length(x) > 0 && "R6" %in% class(x)) {
      return(x$clone(deep = TRUE))
    } else {
      return(x)
    }
  })
  Taxa$new(.list = inputs)
}

#' @export
Taxa <- R6::R6Class(
  classname = "Taxa",
  public = list(
    taxa = list(),

    initialize = function(..., .list = NULL) {
      inputs <- get_dots_or_list(..., .list = .list)
      self$taxa <- lapply(inputs, as_Taxon)
    },

    print = function() {
      cat("<taxa>", "\n")
      cat("  no. taxa: ", length(self$taxa), "\n")
      if (length(self$taxa) > 0) {
        if (all(vapply(self$taxa, function(z) z$is_empty(), logical(1)))) {
          cat("   empty set", "\n")
        } else {
          for (i in seq_len(min(10, length(self$taxa)))) {
            if (self$taxa[[i]]$is_empty()) {
              cat("  empty", "\n")
            } else {
              cat(
                sprintf("  %s / %s / %s",
                        char_or_placeholder(self$taxa[[i]]$name),
                        char_or_placeholder(self$taxa[[i]]$rank),
                        char_or_placeholder(self$taxa[[i]]$id)
                ), "\n")
            }
          }
        }
      }
      if (length(self$taxa) > 10) cat("  ...")
    }
  ),

  active = list(
    names = function(values) {
      if (missing(values)) { # GET
        return(lapply(self$taxa, function(x) x$name))
      }
      else { # SET
        if (length(values) == length(self$taxa)) {
          for (i in seq_len(length(self$taxa))) {
            self$taxa[[i]]$name <- values[[i]]
          }
        } else {
          stop(call. = FALSE,
               'Length of input must be equal to the number of taxa (',
               length(self$taxa), '). ', length(values), ' inputs received.')
        }
      }
    },

    ranks = function(values) {
      if (missing(values)) { # GET
        return(lapply(self$taxa, function(x) x$rank))
      }
      else { # SET
        if (length(values) == 1) {
          for (i in seq_len(length(self$taxa))) {
            self$taxa[[i]]$rank <- values[[1]]
          }
        } else if (length(values) == length(self$taxa)) {
          for (i in seq_len(length(self$taxa))) {
            self$taxa[[i]]$rank <- values[[i]]
          }
        } else {
          stop(call. = FALSE,
               'Length of input must be 1 or equal to the number of taxa (',
               length(self$taxa), '). ', length(values), ' inputs received.')
        }
      }
    },

    ids = function(values) {
      if (missing(values)) { # GET
        return(lapply(self$taxa, function(x) x$id))
      }
      else { # SET
        if (length(values) == length(self$taxa)) {
          for (i in seq_len(length(self$taxa))) {
            self$taxa[[i]]$id <- values[[i]]
          }
        } else {
          stop(call. = FALSE,
               'Length of input must be equal to the number of taxa (',
               length(self$taxa), '). ', length(values), ' inputs received.')
        }
      }
    },

    authorities = function(values) {
      if (missing(values)) { # GET
        return(vapply(self$taxa, function(x) x$authority %||% NA_character_, character(1)))
      }
      else { # SET
        if (length(values) == 1) {
          for (i in seq_len(length(self$taxa))) {
            self$taxa[[i]]$authority <- values
          }
        } else if (length(values) == length(self$taxa)) {
          for (i in seq_len(length(self$taxa))) {
            self$taxa[[i]]$authority <- values[[i]]
          }
        } else {
          stop(call. = FALSE,
               'Length of input must be either 1 or equal to the number of taxa (',
               length(self$taxa), '). ', length(values), ' inputs received.')
        }
      }
    }


  ),

  private = list(
  )

)


#' @export
`[<-.Taxa` <- function(x, i = NULL, j = NULL, values) {
  # If an index is not provided, set all values
  if (missing(i)) {
    i  = seq_len(length(x$taxa))
  }

  if (! missing(i) && any(abs(i) > length(x))) {
    stop(call. = FALSE, "Index out of bounds.")
  }

  if (length(values) == 1) {
    for (index in i) {
      x$taxa[[index]] <- as_Taxon(values[[1]])
    }
  } else if (length(i) == length(values)) {
    for (index in seq_len(length(values))) {
      x$taxa[[i[[index]]]] <- as_Taxon(values[[index]])
    }
  } else {
    stop(call. = FALSE,
         'Length of input must be either 1 or equal to the number of taxa to be replaced (',
         length(values), '). ', length(i), ' indexes supplied.')
  }

  return(x)
}


#' @export
`[.Taxa` <- function(obj, i = NULL, j = NULL) {
  if (! missing(i) && any(abs(i) > length(obj))) {
    stop(call. = FALSE, "Index out of bounds.")
  }
  obj <- obj$clone(deep = TRUE)
  obj$taxa <- obj$taxa[i]
  return(obj)
}


#' @export
`[[<-.Taxa` <- function(x, i = NULL, j = NULL, value) {
  if(missing(i)) {
    stop(call. = FALSE, "No index supplied.")
  }
  if (any(abs(i) > length(x))) {
    stop(call. = FALSE, "Index out of bounds.")
  }
  if (length(i) > 1) {
    stop("Attempted to use [[]] <- with more than one value. Use [] <- instead.")
  }
  x[i] <- value
  x
}


#' @export
`[[.Taxa` <- function(obj, i = NULL, j = NULL) {
  if (! missing(i) && any(abs(i) > length(obj))) {
    stop(call. = FALSE, "Index out of bounds.")
  }
  obj$taxa[[i]]$clone(deep = TRUE)
}


#' @export
as.list.Taxa <- function(obj) {
  lapply(obj$taxa, function(X) x$clone(deep = TRUE))
}


#' @export
as.data.frame.Taxa <- function(obj, ...) {
  data.frame(name = taxon_names(obj),
             rank =  taxon_ranks(obj),
             taxon_id = taxon_ids(obj),
             authority = taxon_auths(obj),
             ...)
}


#' @export
length.Taxa <- function(x) {
  length(x$taxa)
}
