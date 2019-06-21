#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_db constructor
#'
#' Minimal taxon_db constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param db Zero or more taxonomic database names. Should be a name contained in
#'   `names(database_definitions)`. Inputs will be transformed to a `character` vector.
#'
#' @return An `S3` object of class `taxa_taxon_db`
#'
#' @keywords internal
new_taxon_db <- function(db = character()) {
  vctrs::vec_assert(db, character())

  vctrs::new_vctr(db, class = "taxa_taxon_db")
}


#' Taxon database class
#'
#' Used to store the names of taxon databases defined in `database_definitions`.
#'
#' @inheritParams new_taxon_db
#'
#' @return An `S3` object of class `taxa_taxon_db`
#'
#' @family classes
#'
#' @examples
#'
#' @export
taxon_db <- function(db = character()) {
  UseMethod("taxon_db")
}


#' @export
taxon_db.default <- function(db = character()) {
  db <- vctrs::vec_cast(db, character())
  db <- tolower(db)

  validate_db_names(db)

  new_taxon_db(db)
}

#' @export
`taxon_db<-` <- function(x, value) {
  UseMethod('taxon_db<-')
}


#' @importFrom methods setOldClass
methods::setOldClass(c("taxa_taxon_db", "vctrs_vctr"))


#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' @export
#' @keywords internal
format.taxa_taxon_db <- function(x, ...) {
  out <- formatC(vctrs::vec_data(x))
  out
}


#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_db <- function(x) {
  "tax_db"
}


#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_db <- function(x) {
  paste0("taxon_db")
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @method vec_type2 taxa_taxon_db
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon_db
#' @keywords internal
vec_type2.taxa_taxon_db <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon_db", y)


#' @method vec_type2.taxa_taxon_db default
#' @export
vec_type2.taxa_taxon_db.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.taxa_taxon_db vctrs_unspecified
#' @export
vec_type2.taxa_taxon_db.vctrs_unspecified <- function(x, y, ...) x


#' @method vec_type2.taxa_taxon_db taxa_taxon_db
#' @export
vec_type2.taxa_taxon_db.taxa_taxon_db <- function(x, y, ...) new_taxon_db()


#' @method vec_type2.taxa_taxon_db character
#' @export
vec_type2.taxa_taxon_db.character <- function(x, y, ...) character()


#' @method vec_type2.character taxa_taxon_db
#' @importFrom vctrs vec_type2.character
#' @export
vec_type2.character.taxa_taxon_db <- function(x, y, ...) character()


#' @method vec_type2.taxa_taxon_db factor
#' @export
vec_type2.taxa_taxon_db.factor <- function(x, y, ...) factor()


#' @method vec_type2.factor taxa_taxon_db
#' @importFrom vctrs vec_type2.factor
#' @export
vec_type2.factor.taxa_taxon_db <- function(x, y, ...) factor()


#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @method vec_cast taxa_taxon_db
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_db
#' @keywords internal
vec_cast.taxa_taxon_db <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon_db")


#' @method vec_cast.taxa_taxon_db default
#' @export
vec_cast.taxa_taxon_db.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @method vec_cast.taxa_taxon_db taxa_taxon_db
#' @export
vec_cast.taxa_taxon_db.taxa_taxon_db <- function(x, to, x_arg, to_arg) x


#' @method vec_cast.taxa_taxon_db character
#' @export
vec_cast.taxa_taxon_db.character <- function(x, to, x_arg, to_arg) taxon_db(x)


#' @method vec_cast.character taxa_taxon_db
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon_db <- function(x, to, x_arg, to_arg) vctrs::vec_data(x)


#' @method vec_cast.taxa_taxon_db factor
#' @export
vec_cast.taxa_taxon_db.factor <- function(x, to, x_arg, to_arg) taxon_db(x)


#' @method vec_cast.factor taxa_taxon_db
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_taxon_db <- function(x, to, x_arg, to_arg) factor(vctrs::vec_data(x))


#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' Check if is a taxond database
#'
#' Check if an object is a taxon database class
#'
#' @param x An object to test
#'
#' @export
is_taxon_db <- function(x) {
  inherits(x, "taxa_taxon_db")
}



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

#' @keywords internal
is_valid_db_name <- function(db_names) {
  db_names %in% c(database_definitions$get(value = 'name'), NA)
}


#' @keywords internal
validate_db_names <- function(db_names) {
  invalid_names <- db_names[! is_valid_db_name(db_names)]
  if (length(invalid_names) > 0) {
    stop(call. = FALSE, 'Taxon database names must be defined in `database_definitions`.',
         ' See `?database_definitions` for help on adding new database definitions if needed.',
         ' The following names are not known databases:\n',
         limited_print(unique(invalid_names), type = "silent", prefix = "  "),
         'The following databases are defined:\n',
         limited_print(database_definitions$get(value = 'name'), type = "silent", prefix = "  "))
  }
}
