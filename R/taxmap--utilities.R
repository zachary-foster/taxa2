
#' Get observations associated with taxa
#'
#' Given one or more taxa IDs and a \code{\link{taxmap}} object, return the observation indexes
#' (e.g. sequence information) associated with each taxon.
#'
#' @param obj (\code{taxmap})
#' The \code{taxmap} object containing taxon information to be queried.
#' @param subset (\code{character}) \code{taxon_ids} or indexes of \code{taxon_data} for which
#'   supertaxa will be returned. Default: All taxa in \code{obj} will be used.
#' @param recursive (\code{logical})
#' If \code{FALSE}, only return the observation assigned to the specified input taxa, not subtaxa.
#' If \code{TRUE}, return all the observations of every subtaxa, etc.
#' @param simplify (\code{logical}) If \code{TRUE}, then combine all the results into a single
#'   vector of unique observation indexes.
#'
#' @return If \code{simplify = FALSE}, then a list of vectors of observation indexes are returned
#'   corresponding to the \code{target} argument. If \code{simplify = TRUE}, then the observation indexes
#'   for all \code{target} taxa are returned in a single vector.
#'
#' @family taxmap taxonomy functions
#'
#' @export
obs <- function(obj, subset = NULL, recursive = TRUE, simplify = FALSE) {
  # Parse arguments --------------------------------------------------------------------------------
  subset <- format_taxon_subset(obj$taxon_data$taxon_ids, subset)

  # Get observations of taxa ------------------------------------------------------------------------------
  my_subtaxa <- subtaxa(obj, subset = subset, recursive = recursive, include_input = TRUE, index = TRUE)
  unique_subtaxa <- unique(unlist(my_subtaxa))
  obs_taxon_index <- match(obj$obs_data$obs_taxon_ids, obj$taxon_data$taxon_ids)
  obs_key <- split(seq_along(obj$obs_data$obs_taxon_ids), obs_taxon_index)
  output <- stats::setNames(lapply(my_subtaxa, function(x) unname(unlist(obs_key[as.character(x)]))),
                            names(subset))
  is_null <- vapply(output, is.null, logical(1))
  output[is_null] <- lapply(1:sum(is_null), function(x) numeric(0))

  # Reduce dimensionality --------------------------------------------------------------------------
  if (simplify) {
    output <- unique(unname(unlist(output)))
  }

  return(output)
}


#' Get column names of taxon_data
#'
#' Get column names of taxon_data without calculating columns
#'
#' @param obj a \code{taxmap} object
#'
#' @return \code{character}
#'
#' @export
taxon_data_colnames <- function(obj) {
  c(colnames(obj$taxon_data), names(obj$taxon_funcs))
}

#' Get column names of obs_data
#'
#' Get column names of obs_data without calculating columns
#'
#' @param obj a \code{taxmap} object
#'
#' @return \code{character}
#'
#' @export
obs_data_colnames <- function(obj) {
  c(colnames(obj$obs_data), names(obj$obs_funcs))
}
