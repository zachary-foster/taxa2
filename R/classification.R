#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal classfication constructor
#'
#' Minimal classfication constructor for internal use. Only use when the input is known to be valid
#' since few validity checks are done.
#'
#' @param taxonomy A [taxonomy()] object.
#' @param instances The indexes of each instance of a taxon in the taxonomy. Can be any length.
#'
#' @return An `S3` object of class `taxa_classification`
#'
#' @keywords internal
new_classification <- function(taxonomy = taxonomy(), instances = integer()) {
  # Check that values are the correct type
  vctrs::vec_assert(instances, ptype = integer())


  # Create new object
  vctrs::new_vctr(instances, taxonomy = taxonomy, class = "taxa_classification")
}


#' Taxon class
#'
#' \Sexpr[results=rd, stage=render]{taxa:::lifecycle("experimental")} Used to
#' store classifications in reference to a taxonomic tree. For more information
#' on what each class is designed for, see the [concepts] section of the help
#' pages.
#'
#' @param taxa A [taxon()] vector or something that can be converted to a
#'   [taxon()] vector.
#' @param supertaxa The indexes of `taxa` for each taxon's supertaxon.
#' @param instances The indexes of each instance of a taxon in the taxonomy. Can
#'   be any length, but must consist of valid indexes for taxa in the `taxonomy`
#'   object.
#' @param .names The names of the vector.
#'
#' @return An `S3` object of class `taxa_classification`
#' @family classes
#'
#' @examples
#'
#' # Create taxon vector
#' x <- classification(c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo',
#'                      'Panthera tigris', 'Ursidae', 'Ursus', 'Ursus arctos'),
#'                     supertaxa = c(NA, 1, 2, 3, 3, 1, 6, 7),
#'                     instances = c(3, 4, 4, 5, 5, 6, 8, 8, 2, 5, 6, 2))
#'
#' x <- classification(taxon(name = c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo',
#'                                    'Panthera tigris', 'Ursidae', 'Ursus', 'Ursus arctos'),
#'                           rank = c('order', 'family', 'genus', 'species',
#'                                    'species', 'family', 'genus', 'species'),
#'                           id = taxon_id(c('33554', '9681', '9688', '9689',
#'                                           '9694', '9632', '9639', '9644'),
#'                                         db = 'ncbi'),
#'                           auth = c('Bowdich, 1821', 'Fischer, 1817', 'Oken, 1816', 'L., 1758',
#'                                    'L., 1758', 'Fischer, 1817', 'L., 1758', 'L., 1758')),
#'                     supertaxa = c(NA, 1, 2, 3, 3, 1, 6, 7),
#'                     instances = c(3, 4, 4, 5, 5, 6, 8, 8, 2, 5, 6, 2))
#'
#' # Get parts of the classification vector
#' tax_name(x)
#' tax_rank(x)
#' tax_id(x)
#' tax_db(x)
#' tax_auth(x)
#' tax_author(x)
#' tax_date(x)
#' tax_cite(x)
#'
#' # Set parts of the classification vector
#' tax_name(x) <- tolower(tax_name(x))
#' tax_rank(x)[1] <- NA
#' tax_id(x) <- '9999'
#' tax_db(x) <- 'itis'
#' tax_auth(x) <- NA
#' tax_author(x)[2:3] <- c('Joe', 'Billy')
#' tax_date(x) <- c('1999', '2013', '1796', '1899')
#' tax_cite(x)[1] <- 'Linnaeus, C. (1771). Mantissa plantarum altera generum.'
#'
#' # Manipulate classification vectors
#' x[1:3]
#' x[tax_rank(x) > 'family']
#' c(x, x)
#' names(x) <- c('a', 'b', 'c', 'd')
#' x['b'] <- NA
#' is.na(x)
#' as.data.frame(x)
#' as_tibble(x)
#'
#' # Use as columns in tables
#' tibble::tibble(x = x, y = 1:4)
#' data.frame(x = x, y = 1:4)
#'
#' @export
classification <- function(taxa = taxon(), supertaxa = NA, instances = integer(), .names = NULL) {
  # Cast inputs to correct values
  .names <- vctrs::vec_cast(.names, character())
  instances <- vctrs::vec_cast(instances, integer())
  my_taxonomy <- taxonomy(taxa = taxa, supertaxa = supertaxa)

  # Create classification object
  out <- new_classification(taxonomy = my_taxonomy, instances = instances)
  names(out) <- .names
  return(out)
}


#' @importFrom methods setOldClass
#' @exportClass taxa_taxon
setOldClass(c("taxa_classification", "vctrs_vctr"))


#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------

#' @rdname tax_id
#' @export
tax_id.taxa_classification <- function(x) {
  tax_id(attr(x, 'taxonomy'))
}

#' @rdname tax_id
#' @export
`tax_id<-.taxa_classification` <- function(x, value) {
  tax_id(attr(x, 'taxonomy')) <- value
  return(x)
}



#' @rdname tax_db
#' @export
tax_db.taxa_classification <- function(x) {
  tax_db(tax_id(x))
}

#' @rdname tax_db
#' @export
`tax_db<-.taxa_classification` <- function(x, value) {
  tax_db(tax_id(x)) <- value
  return(x)
}



#' @rdname tax_author
#' @export
tax_author.taxa_classification <- function(x) {
  tax_author(tax_auth(x))
}

#' @rdname tax_author
#' @export
`tax_author<-.taxa_classification` <- function(x, value) {
  tax_author(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_date
#' @export
tax_date.taxa_classification <- function(x) {
  tax_date(tax_auth(x))
}

#' @rdname tax_date
#' @export
`tax_date<-.taxa_classification` <- function(x, value) {
  tax_date(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_cite
#' @export
tax_cite.taxa_classification <- function(x) {
  tax_cite(tax_auth(x))
}

#' @rdname tax_cite
#' @export
`tax_cite<-.taxa_classification` <- function(x, value) {
  tax_cite(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_name
#' @export
tax_name.taxa_classification <- function(x) {
  tax_name(attr(x, 'taxonomy'))
}

#' @rdname tax_name
#' @export
`tax_name<-.taxa_classification` <- function(x, value) {
  tax_name(attr(x, 'taxonomy')) <- value
  return(x)
}



#' @rdname tax_auth
#' @export
tax_auth.taxa_classification <- function(x) {
  tax_auth(attr(x, 'taxonomy'))
}

#' @rdname tax_auth
#' @export
`tax_auth<-.taxa_classification` <- function(x, value) {
  tax_auth(attr(x, 'taxonomy')) <- value
  return(x)
}



#' @rdname tax_rank
#' @export
tax_rank.taxa_classification <- function(x) {
  tax_rank(attr(x, 'taxonomy'))
}

#' @rdname tax_rank
#' @export
`tax_rank<-.taxa_classification` <- function(x, value) {
  tax_rank(attr(x, 'taxonomy')) <- value
  return(x)
}


#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------


#' Prepare classification for printing
#'
#' Prepare classification for printing. Makes color optional.
#'
#' @param color Use color?
#'
#' @return character
#'
#' @keywords internal
printed_classification <- function(x, color = FALSE) {
  # Dont print anything if nothing to print
  if (length(x) == 0) {
    return(character(0))
  }

  # Make print out
  out <- vapply(supertaxa(attr(x, 'taxonomy'), include = TRUE)[x], FUN.VALUE = character(1), function(i) {
    taxa <- as.character(attr(x, 'taxonomy'))[rev(i)]
    paste(taxa, collapse = font_punct('|'))
  })

  # Disable color if needed
  if (! color) {
    out <- crayon::strip_style(out)
  }

  # Add names
  if (! is.null(names(x))) {
    names(out) <- names(x)
  }

  # Return tree
  return(out)
}

#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
format.taxa_classification <- function(x, ...) {
  out <- printed_classification(x, color = FALSE)
}

#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
obj_print_data.taxa_classification <- function(x) {
  # Dont print anything if nothing to print
  if (length(x) == 0) {
    return()
  }
  out <- printed_classification(x, color = TRUE)
  print_with_color(out, quote = FALSE)
  invisible(x)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
obj_print_footer.taxa_classification <- function(x) {
  vctrs::obj_print_footer(attr(x, 'taxonomy'))
}



#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_abbr.taxa_classification <- function(x) {
  "classif"
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_full.taxa_classification <- function(x) {
  "classification"
}

#' @rdname taxa_printing_funcs
#' @importFrom pillar pillar_shaft
#' @export
#' @keywords internal
pillar_shaft.taxa_classification <- function(x, ...) {
  out <- printed_classification(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "left")
}


#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2 taxa_classification
#' @importFrom vctrs vec_ptype2
#' @export
#' @export vec_ptype2.taxa_classification
#' @keywords internal
vec_ptype2.taxa_classification <- function(x, y, ...) UseMethod("vec_ptype2.taxa_classification", y)


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_classification default
#' @export
vec_ptype2.taxa_classification.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_classification vctrs_unspecified
#' @export
vec_ptype2.taxa_classification.vctrs_unspecified <- function(x, y, ...) x


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_classification taxa_classification
#' @export
vec_ptype2.taxa_classification.taxa_classification <- function(x, y, ...) new_classification()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_classification character
#' @export
vec_ptype2.taxa_classification.character <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.character taxa_classification
#' @importFrom vctrs vec_ptype2.character
#' @export
vec_ptype2.character.taxa_classification <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_classification factor
#' @export
vec_ptype2.taxa_classification.factor <- function(x, y, ...) factor()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.factor taxa_classification
#' @importFrom vctrs vec_ptype2.factor
#' @export
vec_ptype2.factor.taxa_classification <- function(x, y, ...) factor()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @rdname taxa_casting_funcs
#' @method vec_cast taxa_classification
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_classification
#' @keywords internal
vec_cast.taxa_classification <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_classification")


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_classification default
#' @export
vec_cast.taxa_classification.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_classification taxa_classification
#' @export
vec_cast.taxa_classification.taxa_classification <- function(x, to, x_arg, to_arg) x


#' @rdname taxa_casting_funcs
#' @method vec_cast.character taxa_classification
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_classification <- function(x, to, x_arg, to_arg) as.character(format(x))


#' @rdname taxa_casting_funcs
#' @method vec_cast.factor taxa_classification
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_classification <- function(x, to, x_arg, to_arg) as.factor(format(x))



#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' Check if is a classification
#'
#' Check if an object is the classification class
#'
#' @param x An object to test
#'
#' @export
is_classification <- function(x) {
  inherits(x, "taxa_classification")
}


#' @export
is_root.taxa_classification <- function(x) {
  is_root(attr(x, 'taxonomy'))
}

#' @export
roots.taxa_classification <- function(x) {
  roots(attr(x, 'taxonomy'))
}

#' @export
subtaxa.taxa_classification <- function(x, max_depth = NULL, include = FALSE, value = NULL, ...) {
  subtaxa(attr(x, 'taxonomy'), max_depth = max_depth, include = include, value = value, ...)
}

#' @export
n_subtaxa.taxa_classification <- function(x) {
  subtaxa(attr(x, 'taxonomy'))
}

#' @export
supertaxa.taxa_classification <- function(x, max_depth = NULL, include = FALSE, value = NULL, ...) {
  supertaxa(attr(x, 'taxonomy'), max_depth = max_depth, include = include, value = value, ...)
}

#' @export
n_supertaxa.taxa_classification <- function(x) {
  n_supertaxa(attr(x, 'taxonomy'))
}

#' @method %in% taxa_classification
#' @export
`%in%.taxa_classification` <- function(x, table) {
  UseMethod("%in%.taxa_classification", table)
}

#' @export
`%in%.taxa_classification.default` <- function(x, table) {
  as_taxon(x) %in% table
}

#' @export
`%in%.character.taxa_classification` <- function(x, table) {
  x %in% as.character(as_taxon_name(table))
}

#' @export
`%in%.factor.taxa_classification` <- function(x, table) {
  x %in% as.character(as_taxon_name(table))
}

#' @export
as.data.frame.taxa_classification <- function(x, row.names = NULL, optional = FALSE, ...,
                                        stringsAsFactors = default.stringsAsFactors()) {
  out <- as.data.frame(as_taxon(x))
  cbind(supertaxon = vctrs::field(x, 'supertaxa')[out$taxon], out)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.taxa_classification <- function(x, ...) {
  tibble::as_tibble(as.data.frame(x, stringsAsFactors = FALSE), ...)
}

#' @export
stems.taxa_classification <- function(x, value = NULL, ...) {
  stems(attr(x, 'taxonomy'), value = value, ...)
}

#' @export
is_stem.taxa_classification <- function(x) {
  is_stem(attr(x, 'taxonomy'))
}

#' @export
leaves.taxa_classification <- function(x, value = NULL, ...) {
  leaves(attr(x, 'taxonomy'), value = value, ...)
}

#' @export
is_leaf.taxa_classification <- function(x) {
  is_leaf(attr(x, 'taxonomy'))
}

#' @export
n_leaves.taxa_classification <- function(x) {
  n_leaves(attr(x, 'taxonomy'))
}

#' @export
internodes.taxa_classification <- function(x) {
  internodes(attr(x, 'taxonomy'))
}

#' @export
is_internode.taxa_classification <- function(x) {
  is_internode(attr(x, 'taxonomy'))
}

#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------


#' @keywords internal
attr_as_classification <- function(x, value) {
  out <- lapply(supertaxa(attr(x, 'taxonomy'))[x], function(i) {
    value[i]
  })
  names(out) <- names(x)
  return(out)
}

