#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_rank constructor
#'
#' Minimal taxon_rank constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param rank Zero or more taxonomic ids. Inputs will be transformed to a `character` vector.
#' @param db The name(s) of the database(s) associated with the IDs. If not `NA` (the
#'   default), the input must consist of names of databases in [database_list]. The length must be
#'   0, 1, or equal to the number of IDs.
#'
#' @return An `S3` object of class `taxa_taxon_rank`
#'
#' @keywords internal
new_taxon_rank <- function(rank = character(), db = taxa_taxon_db()) {
  vctrs::vec_assert(rank, ptype = character())
  vctrs::vec_assert(db, ptype = taxon_db())

  vctrs::new_rcrd(list(rank = rank, db = db), class = "taxa_taxon_rank")
}


#' Taxon ID class
#'
#' Used to store taxon IDs, either arbitrary or from a taxonomy database. This is typically used to
#' store taxon IDs in [taxon()] objects.
#'
#' @export
#' @inheritParams new_taxon_rank
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon_rank`
#' @family classes
#'
#' @examples
#' (x <- taxon_rank(12345))
#' x$rank
#' x$database
#'
#' (x <- taxon_rank(
#'   12345,
#'   database_list$ncbi
#' ))
#' x$rank
#' x$database
#'
#' # a null taxon_name object
#' taxon_name(NULL)
#'
taxon_rank <- function(rank = character(), db = NA) {
  rank <- vctrs::vec_cast(rank, character())
  db <- vctrs::vec_cast(db, taxon_db())
  c(rank, db) %<-% vctrs::vec_recycle_common(rank, db)

  validate_id_for_database(rank, db)

  new_taxon_rank(rank, db)
}


#' @importFrom methods setOldClass
methods::setOldClass(c("taxa_taxon_rank", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------

#' @export
`taxon_db<-.taxa_taxon_rank` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_db())
  value <- vctrs::vec_recycle(value, length(x))

  vctrs::field(x, "db") <- value

  return(x)
}


#' @export
taxon_db.taxa_taxon_rank <- function(db = character()) {
  vctrs::field(db, "db")
}



#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' Prepare taxon_rank for printing
#'
#' Prepare taxon_rank for printing. Makes color optional.
#'
#' @param color Use color?
#'
#' @return character
#'
#' @keywords internal
printed_taxon_rank <- function(x, color = FALSE) {
  out <- vctrs::field(x, 'rank')
  db <- vctrs::field(x, 'db')
  out <- paste0(out, ifelse(is.na(db), '', font_secondary(paste0(' (', db, ')'))))
  if (! color) {
    out <- crayon::strip_style(out)
  }
  return(out)
}


#' @export
#' @keywords internal
format.taxa_taxon_rank <- function(x, ...) {
  printed_taxon_rank(x, color = FALSE)
}


#' @export
#' @keywords internal
obj_print_data.taxa_taxon_rank <- function(x) {
  if (length(x) == 0) {
    return()
  }
  out <- printed_taxon_rank(x, color = TRUE)
  print_with_color(out, quote = FALSE)
}


#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_rank <- function(x) {
  "tax_id"
}


#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_rank <- function(x) {
  paste0("taxon_rank")
}


#' @importFrom pillar pillar_shaft
#' @export
#' @keywords internal
pillar_shaft.taxa_taxon_rank <- function(x, ...) {
  out <- printed_taxon_rank(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "left")
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @method vec_type2 taxa_taxon_rank
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon_rank
#' @keywords internal
vec_type2.taxa_taxon_rank <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon_rank", y)


#' @method vec_type2.taxa_taxon_rank default
#' @export
vec_type2.taxa_taxon_rank.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.taxa_taxon_rank vctrs_unspecified
#' @export
vec_type2.taxa_taxon_rank.vctrs_unspecified <- function(x, y, ...) x


#' @method vec_type2.taxa_taxon_rank taxa_taxon_rank
#' @export
vec_type2.taxa_taxon_rank.taxa_taxon_rank <- function(x, y, ...) new_taxon_rank()


#' @method vec_type2.taxa_taxon_rank character
#' @export
vec_type2.taxa_taxon_rank.character <- function(x, y, ...) character()


#' @method vec_type2.character taxa_taxon_rank
#' @importFrom vctrs vec_type2.character
#' @export
vec_type2.character.taxa_taxon_rank <- function(x, y, ...) character()


#' @method vec_type2.taxa_taxon_rank factor
#' @export
vec_type2.taxa_taxon_rank.factor <- function(x, y, ...) factor()


#' @method vec_type2.factor taxa_taxon_rank
#' @importFrom vctrs vec_type2.factor
#' @export
vec_type2.factor.taxa_taxon_rank <- function(x, y, ...) factor()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @method vec_cast taxa_taxon_rank
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_rank
#' @keywords internal
vec_cast.taxa_taxon_rank <- function(x, to) UseMethod("vec_cast.taxa_taxon_rank")


#' @method vec_cast.taxa_taxon_rank default
#' @export
vec_cast.taxa_taxon_rank.default <- function(x, to) vctrs::vec_default_cast(x, to)


#' @method vec_cast.taxa_taxon_rank taxa_taxon_rank
#' @export
vec_cast.taxa_taxon_rank.taxa_taxon_rank <- function(x, to) x


#' @method vec_cast.taxa_taxon_rank character
#' @export
vec_cast.taxa_taxon_rank.character <- function(x, to) taxon_rank(x)


#' @method vec_cast.character taxa_taxon_rank
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon_rank <- function(x, to) vctrs::field(x, "rank")


#' @method vec_cast.taxa_taxon_rank factor
#' @export
vec_cast.taxa_taxon_rank.factor <- function(x, to) taxon_rank(x)


#' @method vec_cast.factor taxa_taxon_rank
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_taxon_rank <- function(x, to) factor(vctrs::field(x, "rank"))


#' @method vec_cast.taxa_taxon_rank double
#' @export
vec_cast.taxa_taxon_rank.double <- function(x, to) taxon_rank(x)


#' @method vec_cast.double taxa_taxon_rank
#' @importFrom vctrs vec_cast.double
#' @export
vec_cast.double.taxa_taxon_rank <- function(x, to) as.numeric(vctrs::field(x, "rank"))


#' @method vec_cast.data.frame taxa_taxon_rank
#' @importFrom vctrs vec_cast.data.frame
#' @export
vec_cast.data.frame.taxa_taxon_rank <- function(x, to) data.frame(stringsAsFactors = FALSE,
                                                                rank = vctrs::field(x, "rank"),
                                                                db = vctrs::field(x, "db"))



#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' Check if is a taxon rank
#'
#' Check if an object is the taxon rank class
#'
#' @param x An object to test
#'
#' @export
is_taxon_rank <- function(x) {
  inherits(x, "taxa_taxon_rank")
}



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

#' @keywords internal
validate_rank_for_database <- function(rank, db) {
  is_invalid <- ! is_valid_database_id(rank, db)
  if (sum(is_invalid) > 0) {
    stop(call. = FALSE, 'Taxon IDs must follow the database ID conventions if a database with a defined ID regex is specified. ',
         'The following IDs do not match the pattern for their database:\n',
         limited_print(paste0(rank[is_invalid], ' (', db[is_invalid], ')'), type = 'silent', prefix = '  '))
  }
}


#' @keywords internal
is_valid_database_rank <- function(rank, db) {
  mapply(function(i, r) {
    grepl(i, pattern = r)
  }, i = rank, r = db_regexs(db))
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
db_ranks <- function(db) {
  db_defs <- database_definitions$get()
  vapply(db, FUN.VALUE = character(1), function(d) {
    if (is.na(d)) {
      return(".*")
    } else {
      return(db_defs[[d]]$id_regex)
    }
  })
}
