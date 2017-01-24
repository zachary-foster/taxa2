
#' Filter observations with a list of conditions
#'
#' Filter observations in a \code{\link{taxmap}} object with a list of conditions. Any column name that
#' appears in \code{obs_data(.data)} can be used as if it was a vector on its own. See
#' \code{\link[dplyr]{filter}} for inspiration and more information.
#'
#' @param .data \code{\link{taxmap}}
#' @param ... One or more filtering conditions. This can be one of two things: \describe{
#'   \item{\code{integer}}{One or more indexes of \code{obs_data}} \item{\code{logical}}{A
#'   \code{TRUE}/\code{FALSE} vector of length equal to the number of rows in \code{obs_data}} }
#'   Any column name that appears in \code{obs_data(.data)} can be used as if it was a vector on
#'   its own.
#' @param unobserved (\code{logical} of length 1) If \code{TRUE}, preserve taxa even if all of their
#'   observations are filtered out. If \code{FALSE}, remove taxa for which all observations were filtered out.
#'   Note that only taxa that are unobserved due to this filtering will be removed; there might be
#'   other taxa without observations to begin with that will not be removed.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @family dplyr-like functions
#'
#' @examples
#' \dontrun{
#' # Filter by sequence name, but preserve all taxa
#' filter_obs(unite_ex_data, grepl("Lachnum", seq_name))
#' # Filter by sequence name and only keep taxa with sequences that pass the filter
#' filter_obs(unite_ex_data, grepl("Lachnum", seq_name), unobserved = FALSE)
#' }
#'
#' @export
filter_obs <- function(.data, ..., unobserved = TRUE) {
  # non-standard argument evaluation ---------------------------------------------------------------
  selection <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), data = obs_data(.data))

  # convert taxon_ids to indexes -------------------------------------------------------------------
  is_char <- vapply(selection, is.character, logical(1))
  if (sum(is_char) > 0) {
    stop("observation filtering with taxon IDs or observation IDs (which dont exist yet) is not currently supported. If you want to filter observation by taxon IDs, use something like: `obs_taxon_ids %in% my_subset`")
  }
  # selection[is_char] <- lapply(selection[is_char], function(x) match(x, .data$taxon_data$taxon_ids))

  # convert logical to indexes ---------------------------------------------------------------------
  is_logical <- vapply(selection, is.logical, logical(1))
  selection[is_logical] <- lapply(selection[is_logical], which)

  # combine filters --------------------------------------------------------------------------------
  intersect_with_dups <-function(a, b) {
    #taken from http://r.789695.n4.nabble.com/intersect-without-discarding-duplicates-td2225377.html
    rep(sort(intersect(a, b)), pmin(table(a[a %in% b]), table(b[b %in% a])))
  }
  selection <- Reduce(intersect_with_dups, selection)

  # Remove observations -----------------------------------------------------------------------------------
  unobserved_taxa <- supertaxa(.data, unique(.data$obs_data$obs_taxon_ids[-selection]), na = FALSE,
                             recursive = TRUE, simplify = TRUE, include_input = TRUE, index = TRUE)
  .data$obs_data <- .data$obs_data[selection, , drop = FALSE]

  # Remove unobserved taxa ---------------------------------------------------------------------------
  if (! unobserved) {
    taxa_to_remove <- 1:nrow(.data$taxon_data) %in% unobserved_taxa & n_obs(.data) == 0
    .data$taxon_data <- .data$taxon_data[! taxa_to_remove, , drop = FALSE]
    .data$taxon_data[! .data$taxon_data$supertaxon_ids %in% .data$taxon_data$taxon_ids, "supertaxon_ids"] <- as.character(NA)
  }

  return(.data)
}

