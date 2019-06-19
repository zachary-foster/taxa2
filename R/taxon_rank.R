#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_rank constructor
#'
#' Minimal taxon_rank constructor for internal use. Only use when the input is
#' known to be valid since few validity checks are done.
#'
#' @param rank Zero or more taxonomic rank names. Inputs will be transformed to
#'   a `character` vector.
#' @param db The name(s) of the database(s) associated with the IDs. If not `NA`
#'   (the default), the input must consist of names of databases in
#'   [database_list]. The length must be 0, 1, or equal to the number of IDs.
#' @param levels A named numeric vector indicating the names and orders of
#'   possible taxonomic ranks. Higher numbers indicate for fine-scale groupings.
#'   Ranks of unknown order can be indicated with `NA` instead of a number.
#'
#' @return An `S3` object of class `taxa_taxon_rank`
#'
#' @keywords internal
new_taxon_rank <- function(rank = character(), db = taxon_db(), levels = taxon_rank_level()) {

  # Check that values are the correct type
  vctrs::vec_assert(rank, ptype = character())
  vctrs::vec_assert(db, ptype = taxon_db())
  vctrs::vec_assert(levels, ptype = taxon_rank_level())

  # Create new object
  vctrs::new_rcrd(list(rank = rank, db = db), levels = levels, class = "taxa_taxon_rank")
}


#' Taxon rank class
#'
#' Used to store taxon ranks, possibly assocaited with a taxonomy database. This is typically used to
#' store taxon ranks in [taxon()] objects.
#'
#' @export
#' @inheritParams new_taxon_rank
#' @param guess_order If `TRUE` and no rank order is given using numbers, try to guess order based on rank names.
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
taxon_rank <- function(rank = character(), db = NA, levels = NULL, guess_order = TRUE) {

  # Cast inputs to correct values
  rank <- vctrs::vec_cast(rank, character())
  db <- vctrs::vec_cast(db, taxon_db())

  # Recycle ranks and databases to common length
  c(rank, db) %<-% vctrs::vec_recycle_common(rank, db)

  # Create taxon levels object
  if (is.null(levels)) {
    levels <- unique(rank)
    levels <- levels[! is.na(levels)]
    levels <- taxon_rank_level(levels, guess_order = TRUE, impute_na = FALSE)
  } else {
    levels <- taxon_rank_level(levels, guess_order = guess_order, impute_na = TRUE)
  }

  # Check that all ranks are defined in levels
  validate_rank_levels(rank, levels)

  # Check that levels are acceptable for the database specified
  validate_rank_dbs(rank, db)

  # Create taxon_rank object
  new_taxon_rank(rank = rank, db = db, levels = levels)
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


#' @export
`levels<-.taxa_taxon_rank` <- function(x, value) {
  levels <- taxon_rank_level(value)
  validate_rank_levels(rank = vctrs::field(x, 'rank'),
                       levels = levels)
  attr(x, "levels") <- levels
  return(x)
}


#' @export
levels.taxa_taxon_rank <- function(x) {
  stats::setNames(vctrs::field(attr(x, 'levels'), 'order'),
                  vctrs::field(attr(x, 'levels'), 'level'))
}


#' @export
`[<-.taxa_taxon_rank` <- function(x, i, j, value) {
  if (is_taxon_rank(value)) {
    attr(x, 'levels') <- c(attr(x, 'levels'), attr(value, 'levels'))
  } else {
    value <- vctrs::vec_cast(value, taxon_rank())
    validate_rank_levels(as.character(value), attr(x, 'levels'))
  }
  NextMethod()
}


#' @export
`[[<-.taxa_taxon_rank` <- function(x, i, j, value) {
  if (is_taxon_rank(value)) {
    attr(x, 'levels') <- c(attr(x, 'levels'), attr(value, 'levels'))
  } else {
    value <- vctrs::vec_cast(value, taxon_rank())
    validate_rank_levels(as.character(value), attr(x, 'levels'))
  }
  NextMethod()
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
obj_print_footer.taxa_taxon_rank <- function(x) {
  levels <- attr(x, 'levels')
  if (length(levels) == 0) {
    return()
  }
  out <- printed_taxon_rank_level(attr(x, 'levels'), color = TRUE)
  cat(paste0(font_secondary('Levels: '), out, '\n'))
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
# S3 equality and comparison functions
#--------------------------------------------------------------------------------

#' @export
#' @keywords internal
vec_proxy_compare.taxa_taxon_rank <- function(x, ...) {
  levels(x)[as.character(x)]
}


#' @export
#' @keywords internal
vec_proxy_equal.taxa_taxon_rank <- function(x, ...) {
  db <- as.character(taxon_db(x))
  db[is.na(db)] <- "NA" # avoids NA comparisons always being NA
  data.frame(rank = as.character(x),
             db = db,
             stringsAsFactors = FALSE)
}


#' @export
#' @keywords internal
Ops.taxa_taxon_rank <- function(e1, e2) {

  # Make comparisons with character vectors use the levels of the taxon_rank
  if (.Generic %in% c('>', '>=', '<', '<=')) {
    if (is.character(e1) && is_taxon_rank(e2)) {
      validate_rank_levels(e1, attr(e2, 'levels'))
      e1 <- taxon_rank(e1, levels = attr(e2, 'levels'))
    }
    if (is.character(e2) && is_taxon_rank(e1)) {
      validate_rank_levels(e2, attr(e1, 'levels'))
      e2 <- taxon_rank(e2, levels = attr(e1, 'levels'))
    }
  }

  NextMethod()
}



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
validate_rank_levels <- function(rank, levels) {
  not_defined <- ! is.na(rank) & ! rank %in% as.character(levels)
  if (sum(not_defined) > 0) {
    stop(call. = FALSE,
         'The following rank names are not in `levels`:\n',
         limited_print(type = 'silent', prefix = '  ', unique(rank[not_defined])))
  }
}


#' @keywords internal
validate_rank_dbs <- function(rank, db) {
  db_levels <- database_definitions$get(value = "rank_levels")
  is_invalid <- vapply(seq_len(length(rank)), FUN.VALUE = logical(1), function(i) {
    ! is.null(db[[i]]) &&
      ! is.na(rank[i]) &&
      ! is.null(db_levels[[db[i]]]) &&
      ! tolower(rank[i]) %in% as.character(db_levels[[db[i]]])
  })
  if (sum(is_invalid) > 0) {
    stop(call. = FALSE,
         'Taxonomic levels must match those used by the database when both levels and database are defined. ',
         'The following levels are not used by their assocaited database:\n',
         limited_print(type = 'silent', prefix = '  ', unique(paste0(rank[is_invalid], ' (', db, ')'))),
         'Type `database_definitions$get(value = "rank_levels")` to see valid levels for each database.')
  }

}

