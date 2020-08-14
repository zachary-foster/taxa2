#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon constructor
#'
#' Minimal taxon constructor for internal use. Only use when the input is known to be valid
#' since few validity checks are done.
#'
#' @param .names The names of the vector.
#' @param taxa The taxa to store as list of [taxon_name] vectors. Each item in the list represents a
#'   singel taxon, potentially with multiple names.
#'
#' @return An `S3` object of class `taxa_taxon_name`
#'
#' @keywords internal
new_taxon <- function(.names = NULL, taxa = list()) {

  # Check that values are the correct type
  # for (item in taxa) {
  #   vctrs::vec_assert(item, ptype = taxon_name())
  # }

  # Create new object
  vctrs::new_list_of(taxa, taxon_name(), class = "taxa_taxon")
}


#' Taxon class
#'
#' \Sexpr[results=rd, stage=render]{taxa:::lifecycle("experimental")}
#' Used to store information about taxa, such as names, ranks, and IDs.
#' This class can store the informtaion of multiple [taxon_name()] objects for each taxon to account for synonyms or multiple authorities for the same taxon.
#' For more information on what each class is designed for, see the [concepts] section of the help pages.
#'
#' @param name The names of taxa. Inputs with be coerced into a list of [character] vectors if anything else
#'   is given.
#' @param rank The ranks of taxa. Inputs with be coerced into a list of [taxon_rank] vectors if anything else
#'   is given.
#' @param id The ids of taxa. These should be unique identifier and are usually associated with a
#'   database. Inputs with be coerced into a list of [taxon_id] vectors if anything else is given.
#' @param auth The authority of the taxon. Inputs with be coerced into a list of [taxon_authority] vectors if
#'   anything else is given.
#' @param .names The names of the vector.
#'
#' @return An `S3` object of class `taxa_taxon`
#' @family classes
#'
#' @examples
#'
#' # Create taxon vector
#' x <- taxon(c('A', 'B', 'C'))
#' x <- taxon(name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
#'                 rank = c('species', 'genus', 'phylum', 'family'),
#'                 id = taxon_id(c('9606', '1386', '4890', '4345'), db = 'ncbi'),
#'                 auth = c('Linnaeus, 1758', 'Cohn 1872', NA, 'Juss., 1789'))
#'
#' # Get parts of the taxon vector
#' tax_name(x)
#' tax_rank(x)
#' tax_id(x)
#' tax_db(x)
#' tax_auth(x)
#' tax_author(x)
#' tax_date(x)
#' tax_cite(x)
#'
#' # Set parts of the taxon vector
#' tax_name(x) <- tolower(tax_name(x))
#' tax_rank(x)[1] <- NA
#' tax_id(x) <- '9999'
#' tax_db(x) <- 'itis'
#' tax_auth(x) <- NA
#' tax_author(x)[2:3] <- c('Joe', 'Billy')
#' tax_date(x) <- c('1999', '2013', '1796', '1899')
#' tax_cite(x)[1] <- 'Linnaeus, C. (1771). Mantissa plantarum altera generum.'
#'
#' # Manipulate taxon vectors
#' x[1:3]
#' x[tax_rank(x) > 'family']
#' c(x, x)
#' names(x) <- c('a', 'b', 'c', 'd')
#' x['b'] <- NA
#' is.na(x)
#'
#' # Use as columns in tables
#' tibble::tibble(x = x, y = 1:4)
#' data.frame(x = x, y = 1:4)
#'
#' # Converting to tables
#' tibble::as_tibble(x)
#' as_data_frame(x)
#'
#' @export
taxon <- function(name = character(0), rank = NA, id = NA, auth = NA, .names = NA) {

  # Convert vector inputs to lists
  name <- as.list(name)
  rank <- as.list(rank)
  id <- as.list(id)
  auth <- as.list(auth)

  # Cast inputs to correct values
  .names <- vctrs::vec_cast(.names, character())

  # Recycle to common length
  recycled <- vctrs::vec_recycle_common(name, rank, id, auth, .names)
  name <- recycled[[1]]
  rank <- recycled[[2]]
  id <- recycled[[3]]
  auth <- recycled[[4]]
  .names <- recycled[[5]]

  # Create list of taxon_name objects
  taxa <- lapply(seq_len(length(name)), function(i) {
    taxon_name(name = name[[i]], rank = rank[[i]], id = id[[i]], auth = auth[[i]])
  })

  # Combine the taxon rank levels for all taxon_name objects
  #   NOTE: perhaps make this an internal function
  combined_levels <- do.call(c, lapply(taxa, function(x) {
    attr(tax_rank(x), 'levels')
  }))
  taxa <- lapply(taxa, function(x) {
    attr(tax_rank(x), 'levels') <- combined_levels
    x
  })

  # Create taxon object
  out <- new_taxon(taxa = taxa)
  if (!is.null(.names) && ! all(is.na(.names))) {
    names(out) <- .names
  }

  return(out)
}


#' @importFrom methods setOldClass
#' @exportClass taxa_taxon
setOldClass(c("taxa_taxon", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------

#' @rdname tax_id
#' @export
tax_id.taxa_taxon <- function(x) {
  get_taxon_field(x, "id")
}

#' @rdname tax_id
#' @export
`tax_id<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_id())
  value <- vctrs::vec_recycle(value, length(x))
  x <- set_first_of_each(x, `tax_id<-`, value)
  return(x)
}



#' @rdname tax_db
#' @export
tax_db.taxa_taxon <- function(x) {
  tax_db(tax_id(x))
}

#' @rdname tax_db
#' @export
`tax_db<-.taxa_taxon` <- function(x, value) {
  tax_db(tax_id(x)) <- value
  return(x)
}



#' @rdname tax_author
#' @export
tax_author.taxa_taxon <- function(x) {
  tax_author(tax_auth(x))
}

#' @rdname tax_author
#' @export
`tax_author<-.taxa_taxon` <- function(x, value) {
  tax_author(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_date
#' @export
tax_date.taxa_taxon <- function(x) {
  tax_date(tax_auth(x))
}

#' @rdname tax_date
#' @export
`tax_date<-.taxa_taxon` <- function(x, value) {
  tax_date(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_cite
#' @export
tax_cite.taxa_taxon <- function(x) {
  tax_cite(tax_auth(x))
}

#' @rdname tax_cite
#' @export
`tax_cite<-.taxa_taxon` <- function(x, value) {
  tax_cite(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_name
#' @export
tax_name.taxa_taxon <- function(x) {
  get_taxon_field(x, "name")
}

#' @rdname tax_name
#' @export
`tax_name<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, character())
  value <- vctrs::vec_recycle(value, length(x))
  x <- set_first_of_each(x, `tax_name<-`, value)
  return(x)
}



#' @rdname tax_auth
#' @export
tax_auth.taxa_taxon <- function(x) {
  get_taxon_field(x, "auth")
}

#' @rdname tax_auth
#' @export
`tax_auth<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_authority())
  value <- vctrs::vec_recycle(value, length(x))
  x <- set_first_of_each(x, `tax_auth<-`, value)
  return(x)
}



#' @rdname tax_rank
#' @export
tax_rank.taxa_taxon <- function(x) {
  get_taxon_field(x, "rank")
}

#' @rdname tax_rank
#' @export
`tax_rank<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_rank())
  value <- vctrs::vec_recycle(value, length(x))
  x <- set_first_of_each(x, `tax_rank<-`, value)
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
printed_taxon <- function(x, color = FALSE) {
  id <- tax_id(x)
  rank <- tax_rank(x)
  auth <- tax_auth(x)
  out <- font_tax_name(tax_name(x))
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
#' @export
#' @keywords internal
format.taxa_taxon <- function(x, ...) {
  printed_taxon(x, color = FALSE)
}


#' @rdname taxa_printing_funcs
#' @importFrom vctrs obj_print_data
#' @export
#' @keywords internal
obj_print_data.taxa_taxon <- function(x) {
  # Dont print anything if nothing to print
  if (length(x) == 0) {
    return()
  }

  # Only print other info for the taxa printed for speed
  original_length <- length(x)
  truncated <- original_length > options()$max.print
  if (truncated) {
    x <- utils::head(x, options()$max.print)
  }

  # Print
  out <- printed_taxon(x, color = TRUE)
  print_with_color(out, original_length = original_length, quote = FALSE)
  invisible(x)
}


#' @rdname taxa_printing_funcs
#' @importFrom vctrs obj_print_footer
#' @export
#' @keywords internal
obj_print_footer.taxa_taxon <- function(x) {
  # print taxon rank levels
  vctrs::obj_print_footer(tax_rank(x)) # Might need to print combined level to be safe (CHANGE)

  # Only print other info for the taxa printed for speed
  original_length <- length(x)
  truncated <- original_length > options()$max.print
  if (truncated) {
    x <- utils::head(x, options()$max.print)
  }

  # print databases used in ids


}


#' @rdname taxa_printing_funcs
#' @importFrom vctrs vec_ptype_abbr
#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon <- function(x) {
  "taxon"
}


#' @rdname taxa_printing_funcs
#' @importFrom vctrs vec_ptype_full
#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon <- function(x) {
  "taxon"
}


#' @rdname taxa_printing_funcs
#' @importFrom pillar pillar_shaft
#' @export
#' @keywords internal
pillar_shaft.taxa_taxon <- function(x, ...) {
  out <- printed_taxon(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "left")
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @rdname taxa_coercion_funcs
#' @method vec_ptype2 taxa_taxon
#' @importFrom vctrs vec_ptype2
#' @export
#' @keywords internal
vec_ptype2.taxa_taxon <- function(x, y, ...) UseMethod("vec_ptype2.taxa_taxon", y)


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon default
#' @export
vec_ptype2.taxa_taxon.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon vctrs_unspecified
#' @export
vec_ptype2.taxa_taxon.vctrs_unspecified <- function(x, y, ...) x


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon taxa_taxon
#' @export
vec_ptype2.taxa_taxon.taxa_taxon <- function(x, y, ...) taxon()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon character
#' @export
vec_ptype2.taxa_taxon.character <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.character taxa_taxon
#' @importFrom vctrs vec_ptype2.character
#' @export
vec_ptype2.character.taxa_taxon <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon taxa_taxon_name
#' @export
vec_ptype2.taxa_taxon.taxa_taxon_name <- function(x, y, ...) taxon()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon_name taxa_taxon
#' @export
vec_ptype2.taxa_taxon_name.taxa_taxon <- function(x, y, ...) taxon()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.taxa_taxon factor
#' @export
vec_ptype2.taxa_taxon.factor <- function(x, y, ...) factor()


#' @rdname taxa_coercion_funcs
#' @method vec_ptype2.factor taxa_taxon
#' @importFrom vctrs vec_ptype2.factor
#' @export
vec_ptype2.factor.taxa_taxon <- function(x, y, ...) factor()




#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------


#' Check if is a taxon
#'
#' Check if an object is the taxon class
#'
#' @param x An object to test
#'
#' @export
is_taxon <- function(x) {
  inherits(x, "taxa_taxon")
}


#' @export
is.na.taxa_taxon <- function(x) {
  is.na(tax_name(x))
}


#' @method %in% taxa_taxon
#' @export
`%in%.taxa_taxon` <- function(x, table) {
  UseMethod("%in%.taxa_taxon", table)
}


#' @export
`%in%.taxa_taxon.default` <- function(x, table) {
  vapply(x, FUN.VALUE = logical(1), function(y) {
    any(as.character(y) %in% table)
  })
}


#' @export
`%in%.character.taxa_taxon` <- function(x, table) {
  x %in% as.character(as_taxon_name(table))
}


#' @export
`%in%.factor.taxa_taxon` <- function(x, table) {
  x %in% as.character(as_taxon_name(table))
}


#' @export
as_data_frame.taxa_taxon <- function(x, row.names = NULL, optional = FALSE, ...,
                                     stringsAsFactors = default.stringsAsFactors()) {
  tax_name_df <- lapply(x, as_data_frame, row.names = row.names, optional = optional,
                        stringsAsFactors = stringsAsFactors, ...)
  tax_name_i <- rep(seq_len(length(tax_name_df)), vapply(tax_name_df, nrow, numeric(1)))
  tax_name_df <- do.call(rbind, tax_name_df)
  cbind(taxon = tax_name_i, tax_name_df)
}


#' @importFrom tibble as_tibble
#' @export
as_tibble.taxa_taxon <- function(x, ...) {
  tibble::as_tibble(as_data_frame(x, stringsAsFactors = FALSE), ...)
}

#' @export
as.character.taxa_taxon <- function(x, ...) {
  tax_name(x)
}

#' @export
as_taxon <- function(x, ...) {
  UseMethod('as_taxon')
}

#' @export
as_taxon.taxa_taxon <- function(x, ...) {
  x
}

#' @export
as_taxon.taxa_taxonomy <- function(x, ...) {
  vctrs::field(x, 'taxa')
}

#' @export
as_taxon.taxa_classification <- function(x, ...) {
  as_taxon(attr(x, 'taxonomy'))[x]
}

#' @export
c.taxa_taxon <- function(...) {
  out <- vctrs::vec_c(...)
  if (is_taxon(out)) {
    attr(tax_rank(out), 'levels') <- do.call(c, lapply(list(...), function(x) attr(tax_rank(x), 'levels')))
  }
  return(out)
}


#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

#' Get a value from taxon_name in taxon
#'
#' Get a value from taxon_name in taxon
#'
#' @param simplify If TRUE, the return a vector composed of the first item in each taxon. Otherwise,
#'   a list will all data is returned.
#'
#' @keywords internal
get_taxon_field <- function(x, i, simplify = TRUE) {
  out <- get_field_in_list(x, i)
  if (simplify) {
    out <- get_first_of_each(out)
  }
  return(out)
}

#' @keywords internal
get_first_of_each <- function(a_list) {
  do.call(c, lapply(a_list, function(x) {
    x[1]
  }))
}

#' @keywords internal
get_field_in_list <- function(list_vctr, id) {
  lapply(list_vctr, vctrs::field, i = id)
}


#' @keywords internal
set_first_of_each <- function(a_list, setter, value) {
  new_taxon(taxa = lapply(seq_len(length(a_list)), function(i) {
    a_list[[i]][1] <- setter(a_list[[i]][1], value[[i]])
    a_list[[i]]
  }))
}

