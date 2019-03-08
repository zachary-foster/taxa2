# -----------------------------------------------------------------------------
#' @export
`taxon_names<-` <- function(obj, value) {
  UseMethod("taxon_names<-")
}

#' @export
`taxon_names<-.Taxon` <- function(obj, value) {
  if (length(value) > 0 && "R6" %in% class(value)) {
    value <- value$clone(deep = TRUE)
  }
  obj$name <- value
  return(obj)
}

#' @export
`taxon_names.Taxon` <- function(obj, ...) {
  return(as.character(obj$name))
}


# -----------------------------------------------------------------------------
#' @export
`taxon_ids<-` <- function(obj, value) {
  UseMethod("taxon_ids<-")
}


#' @export
`taxon_ids<-.Taxon` <- function(obj, value) {
  if (length(value) > 0 && "R6" %in% class(value)) {
    value <- value$clone(deep = TRUE)
  }
  obj$id <- value
  return(obj)
}


#' @export
`taxon_ids.Taxon` <- function(obj, ...) {
  return(as.character(obj$id))
}


# -----------------------------------------------------------------------------
#' @export
`taxon_ranks<-` <- function(obj, value) {
  UseMethod("taxon_ranks<-")
}


#' @export
`taxon_ranks<-.Taxon` <- function(obj, value) {
  if (length(value) > 0 && "R6" %in% class(value)) {
    value <- value$clone(deep = TRUE)
  }
  obj$rank <- value
  return(obj)
}


#' @export
`taxon_ranks.Taxon` <- function(obj, ...) {
  return(as.character(obj$rank))
}


# -----------------------------------------------------------------------------
#' @export
taxon_auths <- function(obj) {
  UseMethod("taxon_auths")
}

#' @export
`taxon_auths<-` <- function(obj, value) {
  UseMethod("taxon_auths<-")
}

#' @export
`taxon_auths<-.Taxon` <- function(obj, value) {
  if (length(value) > 0 && "R6" %in% class(value)) {
    value <- value$clone(deep = TRUE)
  }
  obj$authority <- value
  return(obj)
}

#' @export
`taxon_auths.Taxon` <- function(obj, ...) {
  return(as.character(obj$authority))
}


