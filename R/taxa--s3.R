# -----------------------------------------------------------------------------
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
  return(unname(vapply(obj$names, as.character, character(1))))
}


# -----------------------------------------------------------------------------
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
  return(unname(vapply(obj$ids, as.character, character(1))))
}


# -----------------------------------------------------------------------------
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
  return(unname(vapply(obj$ranks, as.character, character(1))))
}


# -----------------------------------------------------------------------------
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
  return(unname(vapply(obj$authorities, as.character, character(1))))
}
