#' Minimal taxon_id constructor
#'
#' Minimal taxon_id constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param id Zero or more taxonomic ids. Inputs will be transformed to a `character` vector.
#' @param db The name(s) of the database(s) associated with the IDs. If not `NA` (the
#'   default), the input must consist of names of databases in [database_list]. The length must be
#'   0, 1, or equal to the number of IDs.
#'
#' @return An `S3` object of class `taxa_taxon_id`
#'
#' @keywords internal
new_taxon_id <- function(id = character(), db = character()) {
  vctrs::vec_assert(id, ptype = character())
  vctrs::vec_assert(db, ptype = character())

  vctrs::new_rcrd(list(id = id, db = db), class = "taxa_taxon_id")
}


#' Taxon ID class
#'
#' Used to store taxon IDs, either arbitrary or from a taxonomy database. This is typically used to
#' store taxon IDs in [taxon()] objects.
#'
#' @export
#' @inheritParams new_taxon_id
#'
#' @return An `S3` object of class `taxa_taxon_id`
#' @family classes
#'
#' @examples
#' (x <- taxon_id(12345))
#' x$id
#' x$database
#'
#' (x <- taxon_id(
#'   12345,
#'   database_list$ncbi
#' ))
#' x$id
#' x$database
#'
#' # a null taxon_name object
#' taxon_name(NULL)
#'
taxon_id <- function(id = character(), db = NA) {
  id <- vctrs::vec_cast(id, character())
  db <- vctrs::vec_cast(db, character())
  c(id, db) %<-% vec_recycle_common(id, db)

  validate_id_for_database(id, db)

  new_taxon_id(id, db)
}


#' @keywords internal
validate_id_for_database <- function(id, db) {
  is_invalid <- ! is_valid_database_id(id, db)
  if (sum(is_invalid) > 0) {
    stop(call. = FALSE, 'Taxon IDs must follow the database ID conventions if a database with a defined ID regex is specified. ',
         'The following IDs do not match the regex "', self$database$id_regex, '" for their database:\n',
         limited_print(paste0(id[is_invalid], ' (', db[is_invalid], ')'), type = 'silent', prefix = '  '))
  }

}

#' @export
as.character.TaxonId <- function(obj) {
  as.character(obj$id)
}


#' @export
as.TaxonId <- function(input) {
  if ("TaxonId" %in% class(input)) {
    return(input)
  } else {
    return(taxon_id(input))
  }
}

#' @export
as_TaxonId <- as.TaxonId
