#' @export
`taxon_names<-.Taxa` <- function(obj, value) {
  if (length(value) > 0) {
    value <- lapply(value, function(x) {
      if ("R6" %in% class(x)) {
        return(x$clone(deep = TRUE))
      } else {
        return(x)
      }
    })
  }
  obj$names <- value
  return(obj)
}


#' @export
`taxon_names.Taxa` <- function(obj, ...) {
  return(unname(vapply(obj$names, FUN.VALUE = character(1),
                       function(x) {
                         if (length(x) == 0) {
                           return(NA_character_)
                         } else {
                           return(as.character(x))
                         }
                       })))
}


#' @export
`taxon_ids<-.Taxa` <- function(obj, value) {
  if (length(value) > 0) {
    value <- lapply(value, function(x) {
      if ("R6" %in% class(x)) {
        return(x$clone(deep = TRUE))
      } else {
        return(x)
      }
    })
  }
  obj$ids <- value
  return(obj)
}


#' @export
`taxon_ids.Taxa` <- function(obj, ...) {
  return(unname(vapply(obj$ids, FUN.VALUE = character(1),
                       function(x) {
                         if (length(x) == 0) {
                           return(NA_character_)
                         } else {
                           return(as.character(x))
                         }
                       })))
}


#' @export
`taxon_ranks<-.Taxa` <- function(obj, value) {
  if (length(value) > 0) {
    value <- lapply(value, function(x) {
      if ("R6" %in% class(x)) {
        return(x$clone(deep = TRUE))
      } else {
        return(x)
      }
    })
  }
  obj$ranks <- value
  return(obj)
}


#' @export
`taxon_ranks.Taxa` <- function(obj, ...) {
  return(unname(vapply(obj$ranks, FUN.VALUE = character(1),
                       function(x) {
                         if (length(x) == 0) {
                           return(NA_character_)
                         } else {
                           return(as.character(x))
                         }
                       })))
}


#' @export
`taxon_auths<-.Taxa` <- function(obj, value) {
  if (length(value) > 0) {
    value <- lapply(value, function(x) {
      if ("R6" %in% class(x)) {
        return(x$clone(deep = TRUE))
      } else {
        return(x)
      }
    })
  }
  obj$authorities <- value
  return(obj)
}


#' @export
`taxon_auths.Taxa` <- function(obj, ...) {
  return(unname(vapply(obj$authorities, FUN.VALUE = character(1),
                       function(x) {
                         if (length(x) == 0) {
                           return(NA_character_)
                         } else {
                           return(as.character(x))
                         }
                       })))
}


#' @export
as.character.Taxa <- function(x, ...) {
  taxon_names(x)
}