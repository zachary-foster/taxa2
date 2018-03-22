#' A class for multiple taxon objects
#'
#' Stores one or more [taxon()] objects. This is just a thin wrapper for a list
#' of [taxon()] objects.
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
  Taxa$new(
    ...,
    .list = .list
  )
}


Taxa <- R6::R6Class(
  classname = "Taxa",
  public = list(

    values = list (),

    initialize = function(..., .list = NULL) {
      tt <- get_dots_or_list(..., .list = .list)
      if (! all(vapply(tt, inherits, logical(1), what = "Taxon"))) {
        stop("all inputs to 'taxa' must be of class 'Taxon'",
             call. = FALSE)
      }
      self$values <- tt
    },

    print = function() {
      cat("<taxa>", "\n")
      cat("  no. taxa: ", length(self$values), "\n")
      if (length(self$values)) {
        if (all(vapply(self$values, function(z) z$is_empty(), logical(1)))) {
          cat("   empty set", "\n")
        } else {
          for (i in seq_along(self$values[1:min(10, length(self$values))])) {
            if (self$values[[i]]$is_empty()) {
              cat("  empty", "\n")
            } else {
              cat(
                sprintf("  %s / %s / %s",
                        self$values[[i]]$name %||% "",
                        self$values[[i]]$rank %||% "",
                        self$values[[i]]$id %||% ""
                ), "\n")
            }
          }
        }
      }
      if (length(self$values) > 10) cat("  ...")
    }
  ),

  active = list(
    names = function(values) {
      if (missing(values)) { # GET
        return(vapply(self$values, function(x) x$name %||% NA_character_, character(1)))
      }
      else { # SET
        if (length(values) == 1) {
          for (i in seq_len(length(self$values))) {
            self$values[[i]]$name <- values
          }
        } else if (length(values) == length(self$values)) {
          for (i in seq_len(length(self$values))) {
            self$values[[i]]$name <- values[i]
          }
        } else {
          stop(call. = FALSE,
               'Length of input must be either 1 or equal to the number of taxa (',
               length(self$values), ').')
        }
      }
    },

    ranks = function(values) {
      if (missing(values)) { # GET
        return(vapply(self$values, function(x) x$rank %||% NA_character_, character(1)))
      }
      else { # SET
        if (length(values) == 1) {
          for (i in seq_len(length(self$values))) {
            self$values[[i]]$rank <- values
          }
        } else if (length(values) == length(self$values)) {
          for (i in seq_len(length(self$values))) {
            self$values[[i]]$rank <- values[i]
          }
        } else {
          stop(call. = FALSE,
               'Length of input must be either 1 or equal to the number of taxa (',
               length(self$values), ').')
        }
      }
    },

    ids = function(values) {
      if (missing(values)) { # GET
        return(vapply(self$values, function(x) x$id %||% NA_character_, character(1)))
      }
      else { # SET
        if (length(values) == 1) {
          for (i in seq_len(length(self$values))) {
            self$values[[i]]$id <- values
          }
        } else if (length(values) == length(self$values)) {
          for (i in seq_len(length(self$values))) {
            self$values[[i]]$id <- values[i]
          }
        } else {
          stop(call. = FALSE,
               'Length of input must be either 1 or equal to the number of taxa (',
               length(self$values), ').')
        }
      }
    },

    authorities = function(values) {
      if (missing(values)) { # GET
        return(vapply(self$values, function(x) x$authority %||% NA_character_, character(1)))
      }
      else { # SET
        if (length(values) == 1) {
          for (i in seq_len(length(self$values))) {
            self$values[[i]]$authority <- values
          }
        } else if (length(values) == length(self$values)) {
          for (i in seq_len(length(self$values))) {
            self$values[[i]]$authority <- values[i]
          }
        } else {
          stop(call. = FALSE,
               'Length of input must be either 1 or equal to the number of taxa (',
               length(self$values), ').')
        }
      }
    }


  ),

  private = list(
  ))


#' @export
`[<-.Taxa` <- function(x, i = NULL, j = NULL, values) {
  accepted_classes <- c("character", "Taxon")
  is_valid <- vapply(values, FUN.VALUE = logical(1),
                     function(x) any(class(x) %in% accepted_classes))
  if (any(! is_valid)) {
    stop(call. = FALSE,
         'Values in the `taxa` class must be one of the following types:\n  ',
         paste0(accepted_classes, collapse = ", "), "\n",
         "The following ", sum(! is_valid), " inputs are not one of these types:\n  ",
         limited_print(type = "silent", which(! is_valid)))
  }
  x$values[i] <- values
  return(x)
}


#' @export
`[.Taxa` <- function(obj, i = NULL, j = NULL) {
  obj <- obj$clone(deep = TRUE)
  obj$values <- obj$values[i]
  return(obj)
}


#' @export
`[[<-.Taxa` <- function(x, i = NULL, j = NULL, value) {
  if (length(i) > 1) {
    stop("Attempted to use [[]] <- with more than one value. Use [] <- instead.")
  }
  x[i] <- value
  x
}


#' @export
`[[.Taxa` <- function(obj, i = NULL, j = NULL) {
  obj$values[[i]]
}


#' @export
as.list.Taxa <- function(obj) {
  obj$values
}


#' @export
as.data.frame <- function(obj, ...) {
  data.frame(name = obj$names,
             rank =  obj$ranks,
             taxon_id = obj$ids,
             authority = obj$authorities,
             ...)
}

