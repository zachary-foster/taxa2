#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

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
new_taxon_id <- function(id = character(), db = taxa_taxon_db()) {
  vctrs::vec_assert(id, ptype = character())
  vctrs::vec_assert(db, ptype = taxon_db())

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
#' @importFrom vctrs %<-%
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
  db <- vctrs::vec_cast(db, taxon_db())
  c(id, db) %<-% vctrs::vec_recycle_common(id, db)

  validate_id_for_database(id, db)

  new_taxon_id(id, db)
}


#' @importFrom methods setOldClass
methods::setOldClass(c("taxa_taxon_id", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------

#' @export
`taxon_db<-.taxa_taxon_id` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_db())
  value <- vctrs::vec_recycle(value, length(x))

  vctrs::field(x, "db") <- value

  return(x)
}


#' @export
taxon_db.taxa_taxon_id <- function(db = character()) {
  vctrs::field(db, "db")
}



#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' @keywords internal
#' @export
format.taxa_taxon_id <- function(x, ...) {
  out <- vctrs::field(x, 'id')
  db <- vctrs::field(x, 'db')
  out <- paste0(out, ifelse(is.na(db), '', font_secondary(paste0(' (', db, ')'))))
  # out <- paste0(out, ifelse(is.na(db), '', paste0(' (', db, ')')))
  return(out)
}


#' @keywords internal
#' @export
obj_print_data.taxa_taxon_id <- function(x) {
  # if (length(x) == 0)
  #   return()
  print_with_color(x, quote = FALSE)
}


#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_id <- function(x) {
  "tax_id"
}


#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_id <- function(x) {
  paste0("taxon_id")
}


#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @method vec_type2 taxa_taxon_id
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon_id
#' @keywords internal
vec_type2.taxa_taxon_id <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon_id", y)


#' @method vec_type2.taxa_taxon_id default
#' @export
vec_type2.taxa_taxon_id.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.taxa_taxon_id vctrs_unspecified
#' @export
vec_type2.taxa_taxon_id.vctrs_unspecified <- function(x, y, ...) x


#' @method vec_type2.taxa_taxon_id taxa_taxon_id
#' @export
vec_type2.taxa_taxon_id.taxa_taxon_id <- function(x, y, ...) new_taxon_id()


#' @method vec_type2.taxa_taxon_id character
#' @export
vec_type2.taxa_taxon_id.character <- function(x, y, ...) character()


#' @method vec_type2.character taxa_taxon_id
#' @importFrom vctrs vec_type2.character
#' @export
vec_type2.character.taxa_taxon_id <- function(x, y, ...) character()


#' @method vec_type2.taxa_taxon_id factor
#' @export
vec_type2.taxa_taxon_id.factor <- function(x, y, ...) factor()


#' @method vec_type2.factor taxa_taxon_id
#' @importFrom vctrs vec_type2.factor
#' @export
vec_type2.factor.taxa_taxon_id <- function(x, y, ...) factor()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @method vec_cast taxa_taxon_id
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_id
#' @keywords internal
vec_cast.taxa_taxon_id <- function(x, to) UseMethod("vec_cast.taxa_taxon_id")


#' @method vec_cast.taxa_taxon_id default
#' @export
vec_cast.taxa_taxon_id.default <- function(x, to) vctrs::vec_default_cast(x, to)


#' @method vec_cast.taxa_taxon_id taxa_taxon_id
#' @export
vec_cast.taxa_taxon_id.taxa_taxon_id <- function(x, to) x


#' @method vec_cast.taxa_taxon_id character
#' @export
vec_cast.taxa_taxon_id.character <- function(x, to) taxon_id(x)


#' @method vec_cast.character taxa_taxon_id
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon_id <- function(x, to) vctrs::field(x, "id")


#' @method vec_cast.taxa_taxon_id factor
#' @export
vec_cast.taxa_taxon_id.factor <- function(x, to) taxon_id(x)


#' @method vec_cast.factor taxa_taxon_id
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_taxon_id <- function(x, to) factor(vctrs::field(x, "id"))


#' @method vec_cast.taxa_taxon_id double
#' @export
vec_cast.taxa_taxon_id.double <- function(x, to) taxon_id(x)


#' @method vec_cast.double taxa_taxon_id
#' @importFrom vctrs vec_cast.double
#' @export
vec_cast.double.taxa_taxon_id <- function(x, to) as.numeric(vctrs::field(x, "id"))



#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' Check if is a taxon id
#'
#' Check if an object is the taxon id class
#'
#' @param x An object to test
#'
#' @export
is_taxon_id <- function(x) {
  inherits(x, "taxa_taxon_id")
}



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

#' @keywords internal
validate_id_for_database <- function(id, db) {
  is_invalid <- ! is_valid_database_id(id, db)
  if (sum(is_invalid) > 0) {
    stop(call. = FALSE, 'Taxon IDs must follow the database ID conventions if a database with a defined ID regex is specified. ',
         'The following IDs do not match the pattern for their database:\n',
         limited_print(paste0(id[is_invalid], ' (', db[is_invalid], ')'), type = 'silent', prefix = '  '))
  }
}


#' @keywords internal
is_valid_database_id <- function(id, db) {
  mapply(function(i, r) {
    grepl(i, pattern = r)
  }, i = id, r = db_regexs(db))
}


#' ID validation regexs from database names
#'
#' ID validation regexs from database names
#'
#' @param db The names of the databases
#'
#' @return regexes as character vector
#'
#' @keywords internal
db_regexs <- function(db) {
  db_defs <- database_definitions$get()
  vapply(db, FUN.VALUE = character(1), function(d) {
    if (is.na(d)) {
      return(".*")
    } else {
      return(db_defs[[d]]$id_regex)
    }
  })
}
