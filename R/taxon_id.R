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
#'   default), the input must consist of names of databases in [database_ref]. The length must be
#'   0, 1, or equal to the number of IDs.
#'
#' @return An `S3` object of class `taxa_taxon_id`
#'
#' @keywords internal
new_taxon_id <- function(id = character(), db = taxon_db()) {
  vctrs::vec_assert(id, ptype = character())
  vctrs::vec_assert(db, ptype = taxon_db())

  vctrs::new_rcrd(list(id = id, db = db), class = "taxa_taxon_id")
}


#' Taxon ID class
#'
#' \Sexpr[results=rd, stage=render]{taxa:::lifecycle("experimental")}
#' The function used to create `taxon_id` objects and access `taxon_id` objects in other classes
#' that contain them, such as [taxon()], Used to store taxon IDs, either arbitrary or from a
#' taxonomy database. This is typically used to store taxon IDs in [taxon()] objects.
#'
#' @param ... Used to pass arguments to methods and allow methods to use additional arguments.
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon_id`
#' @family classes
#'
#' @examples
#'
#' # Making new objects
#' x <- taxon_id(c('A', 'B', 'C'))
#' x <- taxon_id(c(1232, 3232, 1242))
#' x <- taxon_id(c('9606', '1386', '4890', '4345'), db = 'ncbi')
#' x <- taxon_id(c('9606', '1386', '4890', '4345'),
#'               db = c('ncbi', 'ncbi', 'itis', 'itis'))
#'
#' # Manipulating objects
#' as.character(x)
#' x[2:3]
#' x[2:3] <- 'ABC'
#'
#' # Using as columns in tables
#' tibble::tibble(x = x, y = 1:4)
#' data.frame(x = x, y = 1:4)
#'
#' # Trying to use an invalid ID with a specified database causes an error
#' #taxon_id('NOLETTERS', db = 'ncbi')
#'
#' @export
taxon_id <- function(...) {
  UseMethod("taxon_id")
}


#' @rdname taxon_id
#'
#' @param id Zero or more taxonomic ids. Inputs will be transformed to a `character` vector if
#'   possible.
#' @param db The name(s) of the database(s) associated with the IDs. If not `NA` (the default), the
#'   input must consist of names of databases in [database_ref]. The length must be 0, 1, or equal
#'   to the number of IDs.
#'
#' @export
taxon_id.default <- function(id = character(), db = NA, ...) {
  id <- vctrs::vec_cast(id, character())
  db <- vctrs::vec_cast(db, taxon_db())
  c(id, db) %<-% vctrs::vec_recycle_common(id, db)
  validate_id_for_database(id, db)
  new_taxon_id(id, db)
}


#' @rdname taxon_id
#'
#' @param x An object with taxon IDs.
#' @param value The taxon IDs to set. Inputs will be coerced into a [taxon_id()] vector.
#'
#' @export
`taxon_id<-` <- function(x, value) {
  UseMethod('taxon_id<-')
}


setOldClass(c("taxa_taxon_id", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------

#' @rdname taxon_db
#' @export
`taxon_db<-.taxa_taxon_id` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_db())
  value <- vctrs::vec_recycle(value, length(x))

  vctrs::field(x, "db") <- value

  return(x)
}


#' @rdname taxon_db
#' @export
taxon_db.taxa_taxon_id <- function(db = character(), ...) {
  vctrs::field(db, "db")
}


#' @rdname taxon_id
#' @export
`taxon_id<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_id())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "id") <- value
  return(x)
}


#' @rdname taxon_id
#' @export
taxon_id.taxa_taxon <- function(x, ...) {
  vctrs::field(x, "id")
}



#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' Prepare taxon_id for printing
#'
#' Prepare taxon_id for printing. Makes color optional.
#'
#' @param color Use color?
#'
#' @return character
#'
#' @keywords internal
printed_taxon_id <- function(x, color = FALSE) {
  out <- vctrs::field(x, 'id')
  db <- vctrs::field(x, 'db')
  out <- font_na(out)
  out <- paste0(out, ifelse(is.na(db), '', font_secondary(paste0(' (', db, ')'))))
  if (! color) {
    out <- crayon::strip_style(out)
  }
  return(out)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
format.taxa_taxon_id <- function(x, ...) {
  printed_taxon_id(x, color = FALSE)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
obj_print_data.taxa_taxon_id <- function(x) {
  if (length(x) == 0) {
    return()
  }
  out <- printed_taxon_id(x, color = TRUE)
  print_with_color(out, quote = FALSE)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_id <- function(x) {
  "tax_id"
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_id <- function(x) {
  paste0("taxon_id")
}


#' @rdname taxa_printing_funcs
#' @importFrom pillar pillar_shaft
#' @export
#' @keywords internal
pillar_shaft.taxa_taxon_id <- function(x, ...) {
  out <- printed_taxon_id(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "left")
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @rdname taxa_coercion_funcs
#' @method vec_ptype2 taxa_taxon_id
#' @importFrom vctrs vec_ptype2
#' @export
#' @export vec_ptype2.taxa_taxon_id
#' @keywords internal
vec_ptype2.taxa_taxon_id <- function(x, y, ...) UseMethod("vec_ptype2.taxa_taxon_id", y)


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_id default
#' @export
vec_ptype2.taxa_taxon_id.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_id vctrs_unspecified
#' @export
vec_ptype2.taxa_taxon_id.vctrs_unspecified <- function(x, y, ...) x


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_id taxa_taxon_id
#' @export
vec_ptype2.taxa_taxon_id.taxa_taxon_id <- function(x, y, ...) new_taxon_id()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_id character
#' @export
vec_ptype2.taxa_taxon_id.character <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.character taxa_taxon_id
#' @importFrom vctrs vec_ptype2.character
#' @export
vec_ptype2.character.taxa_taxon_id <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_id factor
#' @export
vec_ptype2.taxa_taxon_id.factor <- function(x, y, ...) factor()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.factor taxa_taxon_id
#' @importFrom vctrs vec_ptype2.factor
#' @export
vec_ptype2.factor.taxa_taxon_id <- function(x, y, ...) factor()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @rdname taxa_casting_funcs
#' @method vec_cast taxa_taxon_id
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_id
#' @keywords internal
vec_cast.taxa_taxon_id <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon_id")


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_id default
#' @export
vec_cast.taxa_taxon_id.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_id taxa_taxon_id
#' @export
vec_cast.taxa_taxon_id.taxa_taxon_id <- function(x, to, x_arg, to_arg) x


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_id character
#' @export
vec_cast.taxa_taxon_id.character <- function(x, to, x_arg, to_arg) taxon_id(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.character taxa_taxon_id
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon_id <- function(x, to, x_arg, to_arg) vctrs::field(x, "id")


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_id factor
#' @export
vec_cast.taxa_taxon_id.factor <- function(x, to, x_arg, to_arg) taxon_id(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.factor taxa_taxon_id
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_taxon_id <- function(x, to, x_arg, to_arg) factor(vctrs::field(x, "id"))


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_id integer
#' @export
vec_cast.taxa_taxon_id.integer <- function(x, to, x_arg, to_arg) taxon_id(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.integer taxa_taxon_id
#' @importFrom vctrs vec_cast.integer
#' @export
vec_cast.integer.taxa_taxon_id <- function(x, to, x_arg, to_arg) integer(vctrs::field(x, "id"))


#' @rdname taxa_casting_funcs
#' @method vec_cast.data.frame taxa_taxon_id
#' @importFrom vctrs vec_cast.data.frame
#' @export
vec_cast.data.frame.taxa_taxon_id <- function(x, to, x_arg, to_arg) data.frame(stringsAsFactors = FALSE,
                                                                id = vctrs::field(x, "id"),
                                                                db = vctrs::field(x, "db"))



#--------------------------------------------------------------------------------
# S3 equality and comparison functions
#--------------------------------------------------------------------------------


#' @rdname taxa_comparison_funcs
#' @export
#' @keywords internal
vec_proxy_equal.taxa_taxon_id <- function(x, ...) {
  db <- as.character(taxon_db(x))
  db[is.na(db)] <- "NA" # avoids NA comparisons always being NA
  data.frame(id = as.character(x),
             db = db,
             stringsAsFactors = FALSE)
}



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


#' @rdname taxon_id
#' @export
is.na.taxa_taxon_id <- function(x) {
  is.na(vctrs::vec_cast(x, character()))
}


#' @export
`%in%.taxa_taxon_id` <- function(x, table) {
  UseMethod("%in%.taxa_taxon_id", table)
}


#' @export
`%in%.taxa_taxon_id.default` <- function(x, table) {
  as.character(x) %in% table
}


#' @export
`%in%.character.taxa_taxon_id` <- function(x, table) {
  x %in% as.character(table)
}


#' @export
`%in%.factor.taxa_taxon_id` <- function(x, table) {
  x %in% as.character(table)
}



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

#' @keywords internal
validate_id_for_database <- function(id, db) {
  is_invalid <- ! is_valid_database_id(id, db)
  if (sum(is_invalid, na.rm = TRUE) > 0) {
    stop(call. = FALSE, 'Taxon IDs must follow the database ID conventions if a database with a defined ID regex is specified. ',
         'The following IDs do not match the pattern for their database:\n',
         limited_print(paste0(id[is_invalid], ' (', db[is_invalid], ')'), type = 'silent', prefix = '  '))
  }
}


#' @keywords internal
is_valid_database_id <- function(id, db) {
  mapply(function(i, r) {
    grepl(i, pattern = r)
  }, i = id, r = db_ref$get(value = 'id_regex')[db])
}