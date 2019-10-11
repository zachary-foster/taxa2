#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon name constructor
#'
#' Minimal taxon name constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param name The names of taxa as a [character] vector.
#' @param rank The ranks of taxa as a [taxon_rank] vector.
#' @param id The ids of taxa as a [taxon_id] vector.
#' @param auth The authority of the taxon as a [tax_authority] vector.
#'
#' @return An `S3` object of class `taxa_taxon_name`
#'
#' @keywords internal
new_taxon_name <- function(name = character(), rank = taxon_rank(), id = taxon_id(), auth = taxon_authority()) {

  # Check that values are the correct type
  vctrs::vec_assert(name, ptype = character())
  # vctrs::vec_assert(rank, ptype = taxon_rank())
  vctrs::vec_assert(id, ptype = taxon_id())
  vctrs::vec_assert(auth, ptype = taxon_authority())

  # Create new object
  vctrs::new_rcrd(list(name = name, rank = rank, id = id, auth = auth),
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
#' tax_auth(x)
#'
#' # Set parts of the taxon name vector
#' tax_name(x) <- tolower(tax_name(x))
#' tax_rank(x)[1] <- NA
#' tax_db(taxon_id(x)) <- 'itis'
#' tax_auth(x) <- NA
#'
#' # Manipulate taxon name vectors
#' x[1:3]
#' x[tax_rank(x) > 'family']
#' c(x, x)
#'
#' @export
taxon_name <- function(name = character(0), rank = NA, id = NA, auth = NA) {
  # Cast inputs to correct values
  name <- vctrs::vec_cast(name, character())
  rank <- vctrs::vec_cast(rank, taxon_rank())
  id <- vctrs::vec_cast(id, taxon_id())
  auth <- vctrs::vec_cast(auth, taxon_authority())

  # Recycle ranks and databases to common length
  c(name, rank, id, auth) %<-% vctrs::vec_recycle_common(name, rank, id, auth)

  # Create taxon object
  new_taxon_name(name = name, rank = rank, id = id, auth = auth)
}


setOldClass(c("taxa_taxon_name", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------


#' Set and get taxon authorities
#'
#' Set and get taxon authorities in objects that have them, such as [taxon()] objects.
#'
#' @param x An object with taxon authorities.
#'
#' @return A [character()] vector
#'
#' @export
tax_auth <- function(x) {
  UseMethod('tax_auth')
}


#' @rdname tax_auth
#' @export
tax_auth.taxa_taxon_name <- function(x) {
  vctrs::field(x, "auth")
}


#' @rdname tax_auth
#'
#' @param value The taxon authorities to set. Inputs will be coerced into a [taxon_auth()] vector.
#'
#' @export
`tax_auth<-` <- function(x, value) {
  UseMethod('tax_auth<-')
}


#' @rdname tax_auth
#' @export
`tax_auth<-.taxa_taxon_name` <- function(x, value) {
  value <- vctrs::vec_cast(value, character())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "auth") <- value
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

