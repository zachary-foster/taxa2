#' @export
taxon_ids <- function(obj, ...) {
  UseMethod("taxon_ids")
}

#' @export
taxon_ids.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
taxon_ids.Taxonomy <- function(obj, ...) {
  obj$taxon_ids(...)
}


#' @export
taxon_names <- function(obj, ...) {
  UseMethod("taxon_names")
}

#' @export
taxon_names.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
taxon_names.Taxonomy <- function(obj, ...) {
  obj$taxon_names(...)
}


#' @export
supertaxa <- function(obj, ...) {
  UseMethod("supertaxa")
}

#' @export
supertaxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
supertaxa.Taxonomy <- function(obj, ...) {
  obj$supertaxa(...)
}


#' @export
roots <- function(obj, ...) {
  UseMethod("roots")
}

#' @export
roots.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
roots.Taxonomy <- function(obj, ...) {
  obj$roots(...)
}


#' @export
subtaxa <- function(obj, ...) {
  UseMethod("subtaxa")
}

#' @export
subtaxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
subtaxa.Taxonomy <- function(obj, ...) {
  obj$subtaxa(...)
}


#' @export
stems <- function(obj, ...) {
  UseMethod("stems")
}

#' @export
stems.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
stems.Taxonomy <- function(obj, ...) {
  obj$stems(...)
}


#' @export
leaves <- function(obj, ...) {
  UseMethod("leaves")
}

#' @export
leaves.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
leaves.Taxonomy <- function(obj, ...) {
  obj$leaves(...)
}


#' @export
id_classifications <- function(obj, ...) {
  UseMethod("id_classifications")
}

#' @export
id_classifications.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
id_classifications.Taxonomy <- function(obj, ...) {
  obj$id_classifications(...)
}


#' @export
name_classifications <- function(obj, ...) {
  UseMethod("name_classifications")
}

#' @export
name_classifications.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
name_classifications.Taxonomy <- function(obj, ...) {
  obj$name_classifications(...)
}


#' @export
n_supertaxa <- function(obj) {
  UseMethod("n_supertaxa")
}

#' @export
n_supertaxa.default <- function(obj) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
n_supertaxa.Taxonomy <- function(obj) {
  obj$n_supertaxa()
}


#' @export
n_subtaxa <- function(obj) {
  UseMethod("n_subtaxa")
}

#' @export
n_subtaxa.default <- function(obj) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
n_subtaxa.Taxonomy <- function(obj) {
  obj$n_subtaxa()
}


#' @export
n_subtaxa_1 <- function(obj) {
  UseMethod("n_subtaxa_1")
}

#' @export
n_subtaxa_1.default <- function(obj) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
n_subtaxa_1.Taxonomy <- function(obj) {
  obj$n_subtaxa_1()
}