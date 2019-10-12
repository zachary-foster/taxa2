#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon name constructor
#'
#' Minimal taxon name constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param .names The names of the vector.
#' @param name The names of taxa as a [character] vector.
#' @param rank The ranks of taxa as a [taxon_rank] vector.
#' @param id The ids of taxa as a [taxon_id] vector.
#' @param auth The authority of the taxon as a [tax_authority] vector.
#'
#' @return An `S3` object of class `taxa_taxon_name`
#'
#' @keywords internal
new_taxon_name <- function(.names = NULL, name = character(), rank = taxon_rank(), id = taxon_id(), auth = taxon_authority()) {

  # Set names to NA if not set
  if (is.null(names) || all(is.na(.names))) {
    .names_set <- FALSE
    .names <- vctrs::vec_recycle(NA_character_, length(name))
  } else {
    .names_set <- TRUE
    vctrs::vec_assert(.names, ptype = character())
  }

  # Check that values are the correct type
  vctrs::vec_assert(name, ptype = character())
  # vctrs::vec_assert(rank, ptype = taxon_rank())
  vctrs::vec_assert(id, ptype = taxon_id())
  vctrs::vec_assert(auth, ptype = taxon_authority())

  # Create new object
  vctrs::new_rcrd(list(.names = .names, name = name, rank = rank, id = id, auth = auth),
                  .names_set = .names_set,
                  class = "taxa_taxon_name")
}


#' Taxon name class
#'
#' \Sexpr[results=rd, stage=render]{taxa:::lifecycle("experimental")}
#' Used to store information about taxa, such as names, ranks, and IDs.
#' This should be used when storing information about taxa when things like synonyms or conflicting taxonomies are not needed.
#' To support the possiblity of synonyms or encode confliciting taxonomic opinions, use the [taxon()], [taxonomy()], [hierarchy()], or [taxmap()] classes.
#' For more information on what each class is designed for, see the [concepts] section of the help pages.
#'
#' @param name The names of taxa. Inputs with be coerced into a [taxon_name] vector if anything else
#'   is given.
#' @param rank The ranks of taxa. Inputs with be coerced into a [taxon_rank] vector if anything else
#'   is given.
#' @param id The ids of taxa. These should be unique identifier and are usually associated with a
#'   database. Inputs with be coerced into a [taxon_id] vector if anything else is given.
#' @param auth The authority of the taxon. Inputs with be coerced into a [character] vector if
#'   anything else is given.
#' @param .names The names of the vector.
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon_name`
#' @family classes
#'
#' @examples
#'
#' # Create taxon name vector
#' x <- taxon_name(c('A', 'B', 'C'))
#' x <- taxon_name(name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
#'                 rank = c('species', 'genus', 'phylum', 'family'),
#'                 id = taxon_id(c('9606', '1386', '4890', '4345'), db = 'ncbi'),
#'                 auth = c('Linnaeus, 1758', 'Cohn 1872', NA, 'Juss., 1789'))
#'
#' # Get parts of the taxon name vector
#' tax_name(x)
#' tax_rank(x)
#' tax_id(x)
#' tax_db(x)
#' tax_auth(x)
#' tax_author(x)
#' tax_date(x)
#' tax_cite(x)
#'
#' # Set parts of the taxon name vector
#' tax_name(x) <- tolower(tax_name(x))
#' tax_rank(x)[1] <- NA
#' tax_id(x) <- '9999'
#' tax_db(x) <- 'itis'
#' tax_auth(x) <- NA
#' tax_author(x)[2:3] <- c('Joe', 'Billy')
#' tax_date(x) <- c('1999', '2013', '1796', '1899')
#' tax_cite(x)[1] <- 'Linnaeus, C. (1771). Mantissa plantarum altera generum.'
#'
#' # Manipulate taxon name vectors
#' x[1:3]
#' x[tax_rank(x) > 'family']
#' c(x, x)
#' names(x) <- c('a', 'b', 'c', 'd')
#' x['b']
#'
#' # Use as columns in tables
#' tibble::tibble(x = x, y = 1:4)
#' data.frame(x = x, y = 1:4)
#'
#' @export
taxon_name <- function(name = character(0), rank = NA, id = NA, auth = NA, .names = NA) {
  # Cast inputs to correct values
  name <- vctrs::vec_cast(name, character())
  rank <- vctrs::vec_cast(rank, taxon_rank())
  id <- vctrs::vec_cast(id, taxon_id())
  auth <- vctrs::vec_cast(auth, taxon_authority())
  .names <- vctrs::vec_cast(.names, character())

  # Recycle ranks and databases to common length
  c(name, rank, id, auth, .names) %<-% vctrs::vec_recycle_common(name, rank, id, auth, .names)

  # Create taxon object
  new_taxon_name(.names = .names, name = name, rank = rank, id = id, auth = auth)
}


setOldClass(c("taxa_taxon_name", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------

#' @rdname tax_id
#' @export
tax_id.taxa_taxon_name <- function(x) {
  vctrs::field(x, "id")
}

#' @rdname tax_id
#' @export
`tax_id<-.taxa_taxon_name` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_id())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "id") <- value
  return(x)
}



#' @rdname tax_db
#' @export
tax_db.taxa_taxon_name <- function(x) {
  tax_db(tax_id(x))
}

#' @rdname tax_db
#' @export
`tax_db<-.taxa_taxon_name` <- function(x, value) {
  tax_db(tax_id(x)) <- value
  return(x)
}



#' @rdname tax_author
#' @export
tax_author.taxa_taxon_name <- function(x) {
  tax_author(tax_auth(x))
}

#' @rdname tax_author
#' @export
`tax_author<-.taxa_taxon_name` <- function(x, value) {
  tax_author(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_date
#' @export
tax_date.taxa_taxon_name <- function(x) {
  tax_date(tax_auth(x))
}

#' @rdname tax_date
#' @export
`tax_date<-.taxa_taxon_name` <- function(x, value) {
  tax_date(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_cite
#' @export
tax_cite.taxa_taxon_name <- function(x) {
  tax_cite(tax_auth(x))
}

#' @rdname tax_cite
#' @export
`tax_cite<-.taxa_taxon_name` <- function(x, value) {
  tax_cite(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_name
#' @export
tax_name.taxa_taxon_name <- function(x) {
  vctrs::field(x, "name")
}

#' @rdname tax_name
#' @export
`tax_name<-.taxa_taxon_name` <- function(x, value) {
  value <- vctrs::vec_cast(value, character())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "name") <- value
  return(x)
}



#' @rdname tax_auth
#' @export
tax_auth.taxa_taxon_name <- function(x) {
  vctrs::field(x, "auth")
}

#' @rdname tax_auth
#' @export
`tax_auth<-.taxa_taxon_name` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_authority())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "auth") <- value
  return(x)
}



#' @rdname tax_rank
#' @export
tax_rank.taxa_taxon_name <- function(x) {
  vctrs::field(x, "rank")
}

#' @rdname tax_rank
#' @export
`tax_rank<-.taxa_taxon_name` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_rank())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "rank") <- value
  return(x)
}



#' @rdname taxon_authority
#' @export
names.taxa_taxon_name <- function(x) {
  if (attributes(x)[['.names_set']]) {
    return(vctrs::field(x, ".names"))
  } else {
    return(NULL)
  }
}

#' @rdname taxon_authority
#' @export
`names<-.taxa_taxon_name` <- function(x, value) {
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

#' Prepare taxon for printing
#'
#' Prepare taxon for printing. Makes color optional.
#'
#' @param color Use color?
#'
#' @return character
#'
#' @keywords internal
printed_taxon_name <- function(x, color = FALSE) {
  id <- vctrs::field(x, 'id')
  rank <- vctrs::field(x, 'rank')
  auth <- vctrs::field(x, 'auth')
  out <- font_tax_name(x)
  out <- paste0(out, ifelse(is.na(auth), '', paste0(' ', font_secondary(auth))))
  out <- paste0(ifelse(is.na(id), '', paste0(font_secondary(id), font_punct('|'))),
                out)
  out <- paste0(out, ifelse(is.na(rank), '',
                            paste0(font_punct('|'),
                                   printed_taxon_rank(rank, color = TRUE))))
  if (! color) {
    out <- crayon::strip_style(out)
  }
  if (! is.null(names(x))) {
    names(out) <- names(x)
  }
  return(out)
}


#' @rdname taxa_printing_funcs
#' @rdname taxon
#' @export
#' @keywords internal
format.taxa_taxon_name <- function(x, ...) {
  printed_taxon_name(x, color = FALSE)
}


#' @rdname taxa_printing_funcs
#' @rdname taxon
#' @export
#' @keywords internal
obj_print_data.taxa_taxon_name <- function(x) {
  # Dont print anything if nothing to print
  if (length(x) == 0) {
    return()
  }

  # Only print other info for the taxa printed for speed
  original_length <- length(x)
  truncated <- original_length > options()$max.print
  if (truncated) {
    x <- head(x, options()$max.print)
  }

  # Print
  out <- printed_taxon_name(x, color = TRUE)
  print_with_color(out, original_length = original_length, quote = FALSE)
  invisible(x)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
obj_print_footer.taxa_taxon_name <- function(x) {
  # print taxon rank levels
  vctrs::obj_print_footer(vctrs::field(x, 'rank'))

  # Only print other info for the taxa printed for speed
  original_length <- length(x)
  truncated <- original_length > options()$max.print
  if (truncated) {
    x <- head(x, options()$max.print)
  }

  # print databases used in ids


}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_name <- function(x) {
  "taxon"
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_name <- function(x) {
  "taxon"
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
#' @method vec_ptype2 taxa_taxon_name
#' @importFrom vctrs vec_ptype2
#' @export
#' @export vec_ptype2.taxa_taxon_name
#' @keywords internal
vec_ptype2.taxa_taxon_name <- function(x, y, ...) UseMethod("vec_ptype2.taxa_taxon_name", y)


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_name default
#' @export
vec_ptype2.taxa_taxon_name.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_name vctrs_unspecified
#' @export
vec_ptype2.taxa_taxon_name.vctrs_unspecified <- function(x, y, ...) x


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_name taxa_taxon_name
#' @export
vec_ptype2.taxa_taxon_name.taxa_taxon_name <- function(x, y, ...) new_taxon_name()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_name character
#' @export
vec_ptype2.taxa_taxon_name.character <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.character taxa_taxon_name
#' @importFrom vctrs vec_ptype2.character
#' @export
vec_ptype2.character.taxa_taxon_name <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_name factor
#' @export
vec_ptype2.taxa_taxon_name.factor <- function(x, y, ...) factor()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.factor taxa_taxon_name
#' @importFrom vctrs vec_ptype2.factor
#' @export
vec_ptype2.factor.taxa_taxon_name <- function(x, y, ...) factor()



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
vec_cast.taxa_taxon_name.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_name taxa_taxon_name
#' @export
vec_cast.taxa_taxon_name.taxa_taxon_name <- function(x, to, x_arg, to_arg) x


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_name character
#' @export
vec_cast.taxa_taxon_name.character <- function(x, to, x_arg, to_arg) taxon_name(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.character taxa_taxon_name
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon_name <- function(x, to, x_arg, to_arg) as.character(vctrs::field(x, "name"))


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_name factor
#' @export
vec_cast.taxa_taxon_name.factor <- function(x, to, x_arg, to_arg) taxon_name(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.factor taxa_taxon_name
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_taxon_name <- function(x, to, x_arg, to_arg) as.factor(vctrs::field(x, "name"))



#--------------------------------------------------------------------------------
# S3 equality and comparison functions
#--------------------------------------------------------------------------------

#' @rdname taxa_comparison_funcs
#' @export
#' @keywords internal
vec_proxy_compare.taxa_taxon_name <- function(x, ...) {
  data.frame(stringsAsFactors = FALSE,
             rank = as.character(taxon_rank(x)),
             name = as.character(taxon_name(x)),
             id   = as.character(taxon_id(x)))
}


#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' @export
c.taxa_taxon_name <- function(...) {
  out <- vctrs::vec_c(...)
  if (is_taxon(out)) {
    vctrs::field(out, 'rank') <- do.call(c, lapply(list(...), function(x) vctrs::field(x, 'rank')))
  }
  return(out)
}


#' Check if is a taxon name
#'
#' Check if an object is the taxon name class
#'
#' @param x An object to test
#'
#' @export
is_taxon <- function(x) {
  inherits(x, "taxa_taxon_name")
}


#' @export
is.na.taxa_taxon_name <- function(x) {
  is.na(vctrs::vec_cast(x, character()))
}


#' @export
`%in%.taxa_taxon_name` <- function(x, table) {
  UseMethod("%in%.taxa_taxon_name", table)
}


#' @export
`%in%.taxa_taxon_name.default` <- function(x, table) {
  as.character(x) %in% table
}


#' @export
`%in%.character.taxa_taxon_name` <- function(x, table) {
  x %in% as.character(table)
}


#' @export
`%in%.factor.taxa_taxon_name` <- function(x, table) {
  x %in% as.character(table)
}


#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------








######################################################## Needs to be refactored


#' @rdname taxon_rank
#' @export
`levels<-.taxa_taxon` <- function(x, value) {
  levels(vctrs::field(x, 'rank')) <- value
  return(x)
}


#' @rdname taxon_rank
#' @export
levels.taxa_taxon <- function(x) {
  levels(vctrs::field(x, 'rank'))
}


#' @rdname taxon_rank
#' @export
`tax_rank<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_rank())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "rank") <- value
  return(x)
}


#' @rdname taxon_rank
#' @export
tax_rank.taxa_taxon <- function(x, ...) {
  vctrs::field(x, "rank")
}

