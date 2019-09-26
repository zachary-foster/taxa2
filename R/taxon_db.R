#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_db constructor
#'
#' Minimal taxon_db constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param db Zero or more taxonomic database names. Should be a name contained in
#'   `names(db_ref)`. Inputs will be transformed to a `character` vector.
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
#' \Sexpr[results=rd, stage=render]{taxa:::lifecycle("experimental")}
#' Used to store the names of taxon databases defined in `db_ref`.
#'
#' @param ... Used to pass arguments to methods and allow methods to used additional arguments.
#'
#' @return An `S3` object of class `taxa_taxon_db`
#'
#' @family classes
#'
#' @examples
#'
#' # Making new objects
#' x <- taxon_db(c('ncbi', 'ncbi', 'itis'))
#' x
#'
#' # Manipulating objects
#' as.character(x)
#' x[2:3]
#' x[x == 'itis'] <- 'gbif'
#' x
#' y = tibble::tibble(x = x)
#' y
#' y = data.frame(x = x)
#' y
#' y$x
#'
#' # Trying to use an invalid database generates an error
#' # x <- taxon_db(c('ncbi', 'ncbi', 'my_custom_db'))
#' # x[x == 'itis'] <- 'my_custom_db'
#'
#' # Listing known databases and their properties
#' db_ref$get()
#'
#' # Adding and using a new database
#' db_ref$set(name = 'my_custom_db', desc = 'I just made this up')
#' db_ref$get()
#' x <- taxon_db(c('ncbi', 'ncbi', 'my_custom_db'))
#'
#' @export
taxon_db <- function(...) {
  UseMethod("taxon_db")
}


#' @rdname taxon_db
#'
#' @param db Zero or more taxonomic database names. Should be a name contained in
#'   [db_ref]. Inputs will be transformed to a `character` vector if possible.
#'
#' @export
taxon_db.default <- function(db = character(), ...) {
  db <- vctrs::vec_cast(db, character())
  db <- tolower(db)
  validate_db_names(db)
  new_taxon_db(db)
}


#' @rdname taxon_db
#'
#' @param x An object to set taxon databases for.
#' @param value The taxon databases to use. Inputs will be coerced into a [taxon_db()] vector.
#'
#' @export
`taxon_db<-` <- function(x, value) {
  UseMethod('taxon_db<-')
}


setOldClass(c("taxa_taxon_db", "vctrs_vctr"))


#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' @rdname taxa_printing_funcs
#' @keywords internal
#' @export
format.taxa_taxon_db <- function(x, ...) {
  out <- formatC(vctrs::vec_data(x))
  out
}


#' @rdname taxa_printing_funcs
#' @keywords internal
#' @export
vec_ptype_abbr.taxa_taxon_db <- function(x) {
  "tax_db"
}


#' @rdname taxa_printing_funcs
#' @keywords internal
#' @export
vec_ptype_full.taxa_taxon_db <- function(x) {
  paste0("taxon_db")
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @rdname taxa_coercion_funcs
#' @method vec_ptype2 taxa_taxon_db
#' @importFrom vctrs vec_ptype2
#' @export
#' @export vec_ptype2.taxa_taxon_db
#' @keywords internal
vec_ptype2.taxa_taxon_db <- function(x, y, ...) UseMethod("vec_ptype2.taxa_taxon_db", y)


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_db default
#' @keywords internal
#' @export
vec_ptype2.taxa_taxon_db.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_db vctrs_unspecified
#' @keywords internal
#' @export
vec_ptype2.taxa_taxon_db.vctrs_unspecified <- function(x, y, ...) x


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_db taxa_taxon_db
#' @keywords internal
#' @export
vec_ptype2.taxa_taxon_db.taxa_taxon_db <- function(x, y, ...) new_taxon_db()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_db character
#' @keywords internal
#' @export
vec_ptype2.taxa_taxon_db.character <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.character taxa_taxon_db
#' @importFrom vctrs vec_ptype2.character
#' @keywords internal
#' @export
vec_ptype2.character.taxa_taxon_db <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_db factor
#' @keywords internal
#' @export
vec_ptype2.taxa_taxon_db.factor <- function(x, y, ...) factor()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.factor taxa_taxon_db
#' @importFrom vctrs vec_ptype2.factor
#' @keywords internal
#' @export
vec_ptype2.factor.taxa_taxon_db <- function(x, y, ...) factor()


#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @rdname taxa_casting_funcs
#' @method vec_cast taxa_taxon_db
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_db
#' @keywords internal
vec_cast.taxa_taxon_db <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon_db")


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_db default
#' @export
vec_cast.taxa_taxon_db.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_db taxa_taxon_db
#' @export
vec_cast.taxa_taxon_db.taxa_taxon_db <- function(x, to, x_arg, to_arg) x


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_db character
#' @export
vec_cast.taxa_taxon_db.character <- function(x, to, x_arg, to_arg) taxon_db(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.character taxa_taxon_db
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon_db <- function(x, to, x_arg, to_arg) vctrs::vec_data(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_db factor
#' @export
vec_cast.taxa_taxon_db.factor <- function(x, to, x_arg, to_arg) taxon_db(x)


#' @rdname taxa_casting_funcs
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


#' @rdname taxon_db
#' @export
is.na.taxa_taxon_db <- function(x) {
  is.na(vctrs::vec_cast(x, character()))
}


#' @export
`%in%.taxa_taxon_db` <- function(x, table) {
  UseMethod("%in%.taxa_taxon_db", table)
}


#' @export
`%in%.taxa_taxon_db.default` <- function(x, table) {
  as.character(x) %in% table
}


#' @export
`%in%.character.taxa_taxon_db` <- function(x, table) {
  x %in% as.character(table)
}


#' @export
`%in%.factor.taxa_taxon_db` <- function(x, table) {
  x %in% as.character(table)
}



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

#' @keywords internal
is_valid_db_name <- function(db_names) {
  db_names %in% c(db_ref$get(value = 'name'), NA)
}


#' @keywords internal
validate_db_names <- function(db_names) {
  invalid_names <- db_names[! is_valid_db_name(db_names)]
  if (length(invalid_names) > 0) {
    stop(call. = FALSE, 'Taxon database names must be defined in `db_ref`.',
         ' See `?db_ref` for help on adding new database definitions if needed.',
         ' The following names are not known databases:\n',
         limited_print(unique(invalid_names), type = "silent", prefix = "  "),
         'The following databases are defined:\n',
         limited_print(db_ref$get(value = 'name'), type = "silent", prefix = "  "))
  }
}
