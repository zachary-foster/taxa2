#' Get classification of taxa
#'
#' Get classification strings of taxa in an object of type \code{\link{taxmap}}.
#' Each classification is constructed by concatenating the taxon ids of the given taxon and its supertaxa.
#'
#' @param obj (\code{\link{taxmap}})
#' @param sep (\code{character} of length 1)
#' The character(s) to place between taxon IDs
#'
#' @return \code{character} of length equal to \code{subset}
#'
#' @examples
#' classifications(ex_taxmap)
#'
#' @family taxon_funcs
#'
#' @export
classifications <- function(obj, sep = ";") {
  vapply(supertaxa(obj, recursive = TRUE, include_input = TRUE, return_type = "index", na = FALSE),
         function(x) paste0(rev(x), collapse = sep), character(1))
}


#' Get number of supertaxa
#'
#' Get  number of supertaxa for each taxon in an object of type \code{\link{taxmap}}
#'
#' @param obj (\code{\link{taxmap}})
#'
#' @return \code{numeric}
#'
#' @examples
#' n_supertaxa(ex_taxmap)
#'
#' @family taxon_funcs
#'
#' @export
n_supertaxa <- function(obj) {
  vapply(supertaxa(obj, recursive = TRUE, include_input = FALSE, return_type = "index", na = FALSE),
         length, numeric(1))
}

#' Get number of subtaxa
#'
#' Get number of subtaxa for each taxon in an object of type \code{\link{taxmap}}
#'
#' @param obj (\code{\link{taxmap}})
#'
#' @return \code{numeric}
#'
#' @family taxon_funcs
#'
#' @export
n_subtaxa <- function(obj) {
  vapply(subtaxa(obj, recursive = TRUE, include_input = FALSE, return_type = "index"),
         length, numeric(1))
}

#' Get number of subtaxa
#'
#' Get number of subtaxa for each taxon in an object of type \code{\link{taxmap}}, not including subtaxa of subtaxa etc.
#' This does not include subtaxa assigned to subtaxa.
#'
#' @param obj (\code{\link{taxmap}})
#'
#' @return \code{numeric}
#'
#' @examples
#' n_subtaxa_1(ex_taxmap)
#'
#' @family taxon_funcs
#'
#' @export
n_subtaxa_1 <- function(obj) {
  vapply(subtaxa(obj, recursive = FALSE, include_input = FALSE, return_type = "index"),
         length, numeric(1))
}


#' Count observations in \code{\link{taxmap}}
#'
#' Count observations for each taxon in \code{\link{taxmap}}.
#' This includes observations for the specific taxon and its subtaxa.
#'
#' @param obj (\code{\link{taxmap}})
#' @param target The name of the list/vector/table in \code{obj$data}
#'
#' @return \code{numeric}
#'
#' @examples
#' n_obs(ex_taxmap, "info")
#'
#' @family taxon_funcs
#'
#' @export
n_obs <- function(obj, target) {
  vapply(obs(obj, target, recursive = TRUE, simplify = FALSE), length, numeric(1))
}


#' Count observation assigned in \code{\link{taxmap}}
#'
#' Count observations assigned to a specific taxon in \code{\link{taxmap}}.
#' This does not include observations assigned to subtaxa.
#'
#' @param obj (\code{\link{taxmap}})
#' @param target The name of the list/vector/table in \code{obj$data}
#'
#' @return \code{numeric}
#'
#' @examples
#' n_obs_1(ex_taxmap, "info")
#'
#' @family taxon_funcs
#'
#' @export
n_obs_1 <- function(obj, target) {
  vapply(obs(obj, target, recursive = FALSE, simplify = FALSE), length, numeric(1))
}