#' Format taxon subset value
#'
#' Format an input to a \code{subset} option on functions like \code{\link{supertaxa}}.
#' Converts logical and \code{taxon_ids} into indexes of \code{taxon_data}.
#'
#' @param obj (\code{taxmap})
#' The \code{taxmap} object containing taxon information to be queried.
#' @param index
#' If a \code{character}, then it should be values of \code{taxon_ids}.
#' If a \code{numeric}, then it should be row indexes of \code{taxon_data}.
#' If a \code{logical}, then it should correspond to rows of \code{taxon_data}.
#'
#' @return \code{numeric}
#'
#' @keywords internal
format_taxon_subset <- function(taxon_ids, index) {
  if (is.null(index)) {
    output <- stats::setNames(seq_along(taxon_ids), taxon_ids)
  } else {
    if (is.null(names(index))) {
      names(index) <- index
    }
    if (is.numeric(index)) {
      output <- index
      my_names <- names(index)
    } else if (is.character(index)) {
      output <- match(index, taxon_ids)
      my_names <- names(index)
    } else if (is.logical(index)) {
      output <- which(index)
      my_names <- names(index)[output]
    } else {
      stop("Invalid subset value.")
    }
    names(output) <- my_names
  }
  return(output)
}






