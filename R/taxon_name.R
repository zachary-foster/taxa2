#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_name constructor
#'
#' Minimal taxon_name constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param name Zero or more taxonomic ids. Inputs will be transformed to a `character` vector.
#' @param db The name(s) of the database(s) associated with the IDs. If not `NA` (the
#'   default), the input must consist of names of databases in [database_ref]. The length must be
#'   0, 1, or equal to the number of IDs.
#'
#' @return An `S3` object of class `taxa_taxon_name`
#'
#' @keywords internal
new_taxon_name <- function(name = character(), db = taxon_db()) {
  vctrs::vec_assert(name, ptype = character())
  vctrs::vec_assert(db, ptype = taxon_db())

  vctrs::new_rcrd(list(name = name, db = db), class = "taxa_taxon_name")
}


#' Taxon name class
#'
#' \Sexpr[results=rd, stage=render]{taxa:::lifecycle("experimental")}
#' Used to store taxon IDs, either arbitrary or from a taxonomy database. This is typically used to
#' store taxon IDs in [taxon()] objects.
#'
#' @param ... Used to pass arguments to methods and allow methods to used additional arguments.
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon_name`
#' @family classes
#'
#' @examples
#'
#' # Making new objects
#' x <- taxon_name(c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'))
#' x <- taxon_name(c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
#'                 db = c('ncbi', 'ncbi', 'itis', 'itis'))
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
taxon_name <- function(...) {
  UseMethod("taxon_name")
}


#' @rdname taxon_name
#'
#' @param name Zero or more taxonomic ids. Inputs will be transformed to a `character` vector.
#' @param db The name(s) of the database(s) associated with the IDs. If not `NA` (the
#'   default), the input must consist of names of databases in [database_ref]. The length must be
#'   0, 1, or equal to the number of IDs.
#'
#' @export
taxon_name.default <- function(name = character(), db = NA, ...) {
  name <- vctrs::vec_cast(name, character())
  db <- vctrs::vec_cast(db, taxon_db())
  c(name, db) %<-% vctrs::vec_recycle_common(name, db)
  new_taxon_name(name, db)
}


#' @rdname taxon_name
#'
#' @param x An object with taxon names.
#' @param value The taxon names to set. Inputs will be coerced into a [taxon_name()] vector.
#'
#' @export
`taxon_name<-` <- function(x, value) {
  UseMethod('taxon_name<-')
}


setOldClass(c("taxa_taxon_name", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------

#' @rdname taxon_db
#' @export
`taxon_db<-.taxa_taxon_name` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_db())
  value <- vctrs::vec_recycle(value, length(x))

  vctrs::field(x, "db") <- value

  return(x)
}


#' @rdname taxon_db
#' @export
taxon_db.taxa_taxon_name <- function(x, ...) {
  vctrs::field(x, "db")
}



#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' Prepare taxon_name for printing
#'
#' Prepare taxon_name for printing. Makes color optional.
#'
#' @param color Use color?
#'
#' @return character
#'
#' @keywords internal
printed_taxon_name <- function(x, color = FALSE) {
  out <- vctrs::field(x, 'name')
  db <- vctrs::field(x, 'db')
  out <- ifelse(is.na(out), out, paste0('"', out, '"'))
  out <- font_na(out)
  out <- paste0(font_tax_name(out), ifelse(is.na(db), '', font_secondary(paste0(' (', db, ')'))))
  if (! color) {
    out <- crayon::strip_style(out)
  }
  return(out)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
format.taxa_taxon_name <- function(x, ...) {
  printed_taxon_name(x, color = FALSE)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
obj_print_data.taxa_taxon_name <- function(x) {
  if (length(x) == 0) {
    return()
  }
  out <- printed_taxon_name(x, color = TRUE)
  print_with_color(out, quote = FALSE)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_name <- function(x) {
  "tax_id"
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_name <- function(x) {
  paste0("taxon_name")
}


#' @rdname taxa_printing_funcs
#' @importFrom pillar pillar_shaft
#' @export
#' @keywords internal
pillar_shaft.taxa_taxon_name <- function(x, ...) {
  out <- printed_taxon_name(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "left")
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @rdname taxa_coercion_funcs
#' @method vec_type2 taxa_taxon_name
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon_name
#' @keywords internal
vec_type2.taxa_taxon_name <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon_name", y)


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon_name default
#' @export
#' @keywords internal
vec_type2.taxa_taxon_name.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon_name vctrs_unspecified
#' @export
#' @keywords internal
vec_type2.taxa_taxon_name.vctrs_unspecified <- function(x, y, ...) x


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon_name taxa_taxon_name
#' @export
#' @keywords internal
vec_type2.taxa_taxon_name.taxa_taxon_name <- function(x, y, ...) new_taxon_name()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon_name character
#' @export
#' @keywords internal
vec_type2.taxa_taxon_name.character <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.character taxa_taxon_name
#' @importFrom vctrs vec_type2.character
#' @export
#' @keywords internal
vec_type2.character.taxa_taxon_name <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon_name factor
#' @export
#' @keywords internal
vec_type2.taxa_taxon_name.factor <- function(x, y, ...) factor()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.factor taxa_taxon_name
#' @importFrom vctrs vec_type2.factor
#' @export
#' @keywords internal
vec_type2.factor.taxa_taxon_name <- function(x, y, ...) factor()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @rdname taxa_casting_funcs
#' @method vec_cast taxa_taxon_name
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_name
#' @keywords internal
vec_cast.taxa_taxon_name <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon_name")


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_name default
#' @export
#' @keywords internal
vec_cast.taxa_taxon_name.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_name taxa_taxon_name
#' @export
#' @keywords internal
vec_cast.taxa_taxon_name.taxa_taxon_name <- function(x, to, x_arg, to_arg) x


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_name character
#' @export
#' @keywords internal
vec_cast.taxa_taxon_name.character <- function(x, to, x_arg, to_arg) taxon_name(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.character taxa_taxon_name
#' @importFrom vctrs vec_cast.character
#' @export
#' @keywords internal
vec_cast.character.taxa_taxon_name <- function(x, to, x_arg, to_arg) vctrs::field(x, "name")


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_name factor
#' @export
#' @keywords internal
vec_cast.taxa_taxon_name.factor <- function(x, to, x_arg, to_arg) taxon_name(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.factor taxa_taxon_name
#' @importFrom vctrs vec_cast.factor
#' @export
#' @keywords internal
vec_cast.factor.taxa_taxon_name <- function(x, to, x_arg, to_arg) factor(vctrs::field(x, "name"))


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_name double
#' @export
#' @keywords internal
vec_cast.taxa_taxon_name.double <- function(x, to, x_arg, to_arg) taxon_name(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.double taxa_taxon_name
#' @importFrom vctrs vec_cast.double
#' @export
#' @keywords internal
vec_cast.double.taxa_taxon_name <- function(x, to, x_arg, to_arg) as.numeric(vctrs::field(x, "name"))


#' @rdname taxa_casting_funcs
#' @method vec_cast.data.frame taxa_taxon_name
#' @importFrom vctrs vec_cast.data.frame
#' @export
#' @keywords internal
vec_cast.data.frame.taxa_taxon_name <- function(x, to, x_arg, to_arg) data.frame(stringsAsFactors = FALSE,
                                                                name = vctrs::field(x, "name"),
                                                                db = vctrs::field(x, "db"))



#--------------------------------------------------------------------------------
# S3 equality and comparison functions
#--------------------------------------------------------------------------------


#' @rdname taxa_comparison_funcs
#' @export
#' @keywords internal
vec_proxy_equal.taxa_taxon_name <- function(x, ...) {
  db <- as.character(taxon_db(x))
  db[is.na(db)] <- "NA" # avoids NA comparisons always being NA
  data.frame(name = as.character(x),
             db = db,
             stringsAsFactors = FALSE)
}



#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' Check if is a taxon name
#'
#' Check if an object is the taxon name class
#'
#' @param x An object to test
#'
#' @export
is_taxon_name <- function(x) {
  inherits(x, "taxa_taxon_name")
}


#' @rdname taxon_name
#' @export
is.na.taxa_taxon_name <- function(x) {
  is.na(vctrs::vec_cast(x, character()))
}



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

