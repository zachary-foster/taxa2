#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_authority constructor
#'
#' Minimal taxon_authority constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param .names The names of the vector.
#' @param author Zero or more author names.
#' @param date Zero or more dates.
#' @param citation Zero or more literature citations.
#'
#' @return An `S3` object of class `taxa_taxon_authority`
#'
#' @keywords internal
new_taxon_authority <- function(.names = NULL, author = character(), date = character(), citation = character()) {
  # Set names to NA if not set
  if (is.null(names) || all(is.na(.names))) {
    .names_set <- FALSE
    .names <- vctrs::vec_recycle(NA_character_, length(author))
  } else {
    .names_set <- TRUE
    vctrs::vec_assert(.names, ptype = character())
  }
  vctrs::vec_assert(author, ptype = character())
  vctrs::vec_assert(date, ptype = character())
  vctrs::vec_assert(citation, ptype = character())

  vctrs::new_rcrd(list(.names = .names, author = author, date = date, citation = citation),
                  .names_set = .names_set,
                  class = "taxa_taxon_authority")
}


#' Taxon authority class
#'
#' \Sexpr[results=rd, stage=render]{taxa:::lifecycle("experimental")}
#' The function used to create `taxon_authority` objects
#'
#' @param author Zero or more author names.
#' @param date Zero or more dates.
#' @param citation Zero or more literature citations.
#' @param .names The names of the vector.
#'
#' @return An `S3` object of class `taxa_taxon_authority`
#' @family classes
#'
#' @examples
#'
#' # Making new objects
#' x <- taxon_authority(c('A', 'B', 'C'))
#' x <- taxon_authority(c('Cham. & Schldl.', 'L.'),
#'                      date = c('1827', '1753'))
#'
#' # Manipulating objects
#' as.character(x)
#' as.data.frame(x)
#' as_tibble(x)
#' x[2]
#' x[2] <- 'ABC'
#' names(x) <- c('a', 'b')
#' tax_author(x)[1] <- tolower(tax_author(x)[1])
#' tax_author(x)
#' tax_date(x) <- c('2000', '1234')
#' tax_date(x)
#' tax_cite(x)[2] <- c('Linnaeus, C. (1771). Mantissa plantarum altera generum.')
#' tax_cite(x)
#'
#' # Using as columns in tables
#' tibble::tibble(x = x, y = 1:2)
#' data.frame(x = x, y = 1:2)
#'
#' @export
#' @importFrom vctrs %<-%
taxon_authority <- function(author = character(), date = NA, .names = NA, citation = NA) {
  .names <- vctrs::vec_cast(.names, character())
  author <- vctrs::vec_cast(author, character())
  date <- vctrs::vec_cast(date, character())
  citation <- vctrs::vec_cast(citation, character())
  c(author, date, citation, .names) %<-% vctrs::vec_recycle_common(author, date, citation, .names)
  new_taxon_authority(.names = .names, author = author, date = date, citation = citation)
}

#' @importFrom methods setOldClass
#' @exportClass taxa_taxon_authority
setOldClass(c("taxa_taxon_authority", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------


#' @rdname tax_author
#' @export
`tax_author<-.taxa_taxon_authority` <- function(x, value) {
  value <- vctrs::vec_cast(value, character())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "author") <- value
  return(x)
}

#' @rdname tax_author
#' @export
tax_author.taxa_taxon_authority <- function(date = character()) {
  vctrs::field(date, "author")
}



#' @rdname taxon_db
#' @export
`tax_date<-.taxa_taxon_authority` <- function(x, value) {
  value <- vctrs::vec_cast(value, character())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "date") <- value
  return(x)
}

#' @rdname taxon_db
#' @export
tax_date.taxa_taxon_authority <- function(date = character()) {
  vctrs::field(date, "date")
}



#' @rdname taxon_db
#' @export
`tax_cite<-.taxa_taxon_authority` <- function(x, value) {
  value <- vctrs::vec_cast(value, character())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "citation") <- value
  return(x)
}

#' @rdname taxon_db
#' @export
tax_cite.taxa_taxon_authority <- function(cite = character()) {
  vctrs::field(cite, "citation")
}



#' @rdname taxon_authority
#' @export
names.taxa_taxon_authority <- function(x) {
  if (attributes(x)[['.names_set']]) {
    return(vctrs::field(x, ".names"))
  } else {
    return(NULL)
  }
}

#' @rdname taxon_authority
#' @export
`names<-.taxa_taxon_authority` <- function(x, value) {
  if (is.null(value)) {
    value = NA_character_
    attr(x, '.names_set') <- FALSE
  } else {
    attr(x, '.names_set') <- TRUE
  }
  value <- vctrs::vec_cast(value, character())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, ".names") <- value
  return(x)
}



#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' Prepare taxon_authority for printing
#'
#' Prepare taxon_authority for printing. Makes color optional.
#'
#' @param color Use color?
#'
#' @return character
#'
#' @keywords internal
printed_taxon_authority <- function(x, color = FALSE) {
  author <- vctrs::field(x, 'author')
  date <- vctrs::field(x, 'date')
  out <- font_na(author)
  out <- paste0(out, ifelse(is.na(date), '', paste0(' ', font_secondary(date))))
  if (! color) {
    out <- crayon::strip_style(out)
  }
  if (! is.null(names(x))) {
    names(out) <- names(x)
  }
  return(out)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
format.taxa_taxon_authority <- function(x, ...) {
  printed_taxon_authority(x, color = FALSE)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
obj_print_data.taxa_taxon_authority <- function(x) {
  if (length(x) == 0) {
    return()
  }
  out <- printed_taxon_authority(x, color = TRUE)
  print_with_color(out, quote = FALSE)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_authority <- function(x) {
  "tax_auth"
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_authority <- function(x) {
  paste0("taxon_authority")
}


#' @rdname taxa_printing_funcs
#' @importFrom pillar pillar_shaft
#' @export
#' @keywords internal
pillar_shaft.taxa_taxon_authority <- function(x, ...) {
  out <- printed_taxon_authority(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "left")
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @rdname taxa_coercion_funcs
#' @method vec_ptype2 taxa_taxon_authority
#' @importFrom vctrs vec_ptype2
#' @export
#' @export vec_ptype2.taxa_taxon_authority
#' @keywords internal
vec_ptype2.taxa_taxon_authority <- function(x, y, ...) UseMethod("vec_ptype2.taxa_taxon_authority", y)


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_authority default
#' @export
vec_ptype2.taxa_taxon_authority.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_authority vctrs_unspecified
#' @export
vec_ptype2.taxa_taxon_authority.vctrs_unspecified <- function(x, y, ...) x


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_authority taxa_taxon_authority
#' @export
vec_ptype2.taxa_taxon_authority.taxa_taxon_authority <- function(x, y, ...) new_taxon_authority()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_authority character
#' @export
vec_ptype2.taxa_taxon_authority.character <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.character taxa_taxon_authority
#' @importFrom vctrs vec_ptype2.character
#' @export
vec_ptype2.character.taxa_taxon_authority <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_authority factor
#' @export
vec_ptype2.taxa_taxon_authority.factor <- function(x, y, ...) factor()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.factor taxa_taxon_authority
#' @importFrom vctrs vec_ptype2.factor
#' @export
vec_ptype2.factor.taxa_taxon_authority <- function(x, y, ...) factor()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @rdname taxa_casting_funcs
#' @method vec_cast taxa_taxon_authority
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_authority
#' @keywords internal
vec_cast.taxa_taxon_authority <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon_authority")


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_authority default
#' @export
vec_cast.taxa_taxon_authority.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_authority taxa_taxon_authority
#' @export
vec_cast.taxa_taxon_authority.taxa_taxon_authority <- function(x, to, x_arg, to_arg) x


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_authority character
#' @export
vec_cast.taxa_taxon_authority.character <- function(x, to, x_arg, to_arg) {
  author <- vapply(x, FUN.VALUE = character(1), function(y) {
    if (grepl(y, pattern = '^.+,? *[0-9]{4} *$')) {
      out <- sub(y, pattern = '^(.+),? *([0-9]{4}) *$', replacement = '\\1')
      out <- sub(out, pattern = '[ ,]*$', replacement = '')
      return(out)
    } else {
      return(y)
    }
  })
  date <-  vapply(x, FUN.VALUE = character(1), function(y) {
    if (grepl(y, pattern = '^.+,? *[0-9]{4} *$')) {
      return(sub(y, pattern = '^(.+),? *([0-9]{4}) *$', replacement = '\\2'))
    } else {
      return(NA_character_)
    }
  })
  taxon_authority(author = unname(author), date = unname(date))
}


#' @rdname taxa_casting_funcs
#' @method vec_cast.character taxa_taxon_authority
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon_authority <- function(x, to, x_arg, to_arg) {
  ifelse(is.na(x), NA_character_, printed_taxon_authority(x, color = FALSE))
}


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_authority factor
#' @export
vec_cast.taxa_taxon_authority.factor <- function(x, to, x_arg, to_arg) taxon_authority(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.factor taxa_taxon_authority
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_taxon_authority <- function(x, to, x_arg, to_arg) factor(as.character(x))


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_authority integer
#' @export
vec_cast.taxa_taxon_authority.integer <- function(x, to, x_arg, to_arg) taxon_authority(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.data.frame taxa_taxon_authority
#' @importFrom vctrs vec_cast.data.frame
#' @export
vec_cast.data.frame.taxa_taxon_authority <- function(x, to, x_arg, to_arg) data.frame(stringsAsFactors = FALSE,
                                                                                      author = vctrs::field(x, "author"),
                                                                                      date = vctrs::field(x, "date"))



#--------------------------------------------------------------------------------
# S3 equality and comparison functions
#--------------------------------------------------------------------------------



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
is_taxon_authority <- function(x) {
  inherits(x, "taxa_taxon_authority")
}


#' @rdname taxon_authority
#' @export
is.na.taxa_taxon_authority <- function(x) {
  is.na(vctrs::field(x, "author")) & is.na(vctrs::field(x, "date"))
}


#' @export
`%in%.taxa_taxon_authority` <- function(x, table) {
  UseMethod("%in%.taxa_taxon_authority", table)
}


#' @export
`%in%.taxa_taxon_authority.default` <- function(x, table) {
  as.character(x) %in% table
}


#' @export
`%in%.character.taxa_taxon_authority` <- function(x, table) {
  x %in% as.character(table)
}


#' @export
`%in%.factor.taxa_taxon_authority` <- function(x, table) {
  x %in% as.character(table)
}

#' @export
as.data.frame.taxa_taxon_authority <- function(x, row.names = NULL, optional = FALSE, ...,
                                               stringsAsFactors = default.stringsAsFactors()) {
  data.frame(tax_author = tax_author(x),
             tax_date = tax_date(x),
             tax_cite = tax_cite(x),
             row.names = row.names, stringsAsFactors = stringsAsFactors, ...)

}

#' @importFrom tibble as_tibble
#' @export
as_tibble.taxa_taxon_authority <- function(x, ...) {
  tibble::as_tibble(as.data.frame(x, stringsAsFactors = FALSE), ...)
}


#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------
