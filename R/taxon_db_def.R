
#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_db_def constructor
#'
#' Minimal taxon_db_def constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param name Name of the database in lower case. Inputs will be transformed to a `character` vector.
#' @param url URL of the database website. Inputs will be transformed to a `character` vector.
#' @param desc Description of the database. Inputs will be transformed to a `character` vector.
#' @param id_regex A regular expression for taxon IDs of the database. Inputs will be transformed to a `character` vector.
#' @param ranks Valid taxonomic ranks for the database. Should be a list of `factor`s.
#'
#' @return An `S3` object of class `taxa_taxon_db_def`
#'
#' @keywords internal
new_taxon_db_def <- function(name = character(), url = character(), desc = character(),
                             id_regex = character(), ranks = list()) {
  vctrs::vec_assert(name, ptype = character())
  vctrs::vec_assert(url, ptype = character())
  vctrs::vec_assert(desc, ptype = character())
  vctrs::vec_assert(id_regex, ptype = character())
  vctrs::vec_assert(ranks, ptype = list())

  vctrs::new_rcrd(list(name = name, url = url, desc = desc, id_regex = id_regex, ranks = ranks),
                  class = "taxa_taxon_db_def")
}


#' Taxon database definition class
#'
#' Used to store information on taxonomic databases that is used to validate information in other classes.
#'
#' @inheritParams new_taxon_db_def
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon_db_def`
#' @family classes
#' @keywords internal
taxon_db_def <- function(name = character(), url = NA_character_, desc = NA_character_,
                         id_regex = NA_character_, ranks = rep(list(NULL), length(name))) {
  name <- vctrs::vec_cast(name, character())
  url <- vctrs::vec_cast(url, character())
  desc <- vctrs::vec_cast(desc, character())
  id_regex <- vctrs::vec_cast(id_regex, character())
  ranks <- vctrs::vec_cast(ranks, list())

  # Check that ranks are right format
  valid_ranks <- vapply(ranks, FUN.VALUE = logical(1), function(x) {
    is.factor(x) || is.null(x)
  })
  if (any(! valid_ranks)) {
    stop(call. = FALSE,
         'Invalid ranks. Must be a list of factors. The following indexes are not factors:\n',
         limited_print(type = 'silent', prefix = '  ', which(! valid_ranks)))
  }

  c(name, url, desc, id_regex, ranks) %<-% vctrs::vec_recycle_common(name, url, desc, id_regex, ranks)

  new_taxon_db_def(name, url, desc, id_regex, ranks)
}


#' @importFrom methods setOldClass
methods::setOldClass(c("taxa_taxon_db_def", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' @export
#' @keywords internal
obj_print_data.taxa_taxon_db_def <- function(x) {
  spacer <- '   '
  screen_width <- getOption("width") * 0.8
  max_value_nchar <- round(screen_width * 0.7)

  parts <- lapply(seq_len(length(x)), function(i) {

    # Format key/value pairs for printing
    keys <- vctrs::fields(x)
    keys <- keys[keys != 'name']
    keys_printed <- paste0(keys, ': ')
    keys_printed <- stringr::str_pad(keys_printed, max(nchar(keys_printed)), side = 'right')
    values <- vapply(keys, FUN.VALUE = character(1), function(key) {
      value <- vctrs::field(x, key)[[i]]
      if (length(value) > 1) {
        out <- paste0(value, collapse = ', ')
      } else {
        out <- format(value)
      }
      if (nchar(out) > max_value_nchar) {
        out <- paste0(substr(out, start = 1, stop = max_value_nchar), '\u2026')
      }
      return(out)
    })
    pair <- paste0(spacer, keys_printed, values)
    pair <- stringr::str_pad(pair, max(nchar(pair)), side = 'right')

    # Add name header
    header <- stringr::str_pad(paste0(' ', vctrs::field(x, 'name')[i], ' '),
                               width = max(nchar(pair)) - nchar(spacer),
                               side = 'both',
                               pad = '_')
    header <- paste0(spacer, header)
    pair <- c(header, pair)

    return(pair)
  })

  part_len <- vapply(parts, FUN.VALUE = numeric(1), function(p) {
    max(nchar(p))
  })

  row_num <- floor(cumsum(part_len) / screen_width) + 1

  rows <- lapply(split(parts, row_num), function(row_parts) {
    paste0(do.call(paste, row_parts), collapse = '\n')
  })

  cat(paste0(rows, collapse = '\n\n'))
}


#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_db_def <- function(x) {
  paste0("taxon_db_def")
}



#--------------------------------------------------------------------------------
# Default database definitions
#--------------------------------------------------------------------------------

#' Database list
#'
#' The list of known databases. Not currently used much, but will be when we add
#' more check for taxon IDs and taxon ranks from particular databases.
#'
#' @keywords internal
database_list <- c(

  taxon_db_def(
    name =     "ncbi",
    url =      "http://www.ncbi.nlm.nih.gov/taxonomy",
    desc =     "NCBI Taxonomy Database",
    id_regex = ".*",
    ranks  =   list(NULL)
  ),

  taxon_db_def(
    name =     "gbif",
    url =      "http://www.gbif.org/developer/species",
    desc =     "GBIF Taxonomic Backbone",
    id_regex = ".*",
    ranks  =   list(NULL)
  ),

  taxon_db_def(
    name =     "bold",
    url =      "http://www.boldsystems.org",
    desc =     "Barcode of Life",
    id_regex = ".*",
    ranks  =   list(NULL)
  ),

  taxon_db_def(
    name =     "col",
    url =      "http://www.catalogueoflife.org",
    desc =     "Catalogue of Life",
    id_regex = ".*",
    ranks  =   list(NULL)
  ),

  taxon_db_def(
    name =     "eol",
    url =      "http://eol.org",
    desc =     "Encyclopedia of Life",
    id_regex = ".*",
    ranks  =   list(NULL)
  ),

  taxon_db_def(
    name =     "nbn",
    url =      "https://nbn.org.uk",
    desc =     "UK National Biodiversity Network",
    id_regex = ".*",
    ranks  =   list(NULL)
  ),

  taxon_db_def(
    name =     "tps",
    url =      "http://www.tropicos.org/",
    desc =     "Tropicos",
    id_regex = ".*",
    ranks  =   list(NULL)
  ),

  taxon_db_def(
    name =     "itis",
    url =      "http://www.itis.gov",
    desc =     "Integrated Taxonomic Information System",
    id_regex = "[0-9]+",
    ranks  =   list(NULL)
  )
)



#--------------------------------------------------------------------------------
# getters/setters
#--------------------------------------------------------------------------------

#' Defines valid taxonomic databases
#'
#' @param name (character) name of the database
#' @param url (character) url for the database
#' @param desc (character) description of the database
#' @param id_regex (character) id regex
#'
#' @section Attribution:
#'
#' This code is copied from the code handling options in [knitr].
#'
#' @keywords internal
default_database_definitions <- function(defaults = list()) {
  definitions <- defaults
  initial_value <- defaults

  merge_list = function(x, y) {
    x[names(y)] = y
    x
  }

  get <- function(name = NULL, value = NULL) {
    db_names <- vctrs::field(definitions, 'name')
    if (is.null(name)) {
      if (is.null(value)) {
        return(definitions)
      } else {
        return(stats::setNames(vctrs::field(definitions, value),
                               db_names))
      }
    } else {
      db <- definitions[db_names == name]
      if (is.null(value)) {
        if (name %in% db_names) {
          return(db)
        } else {
          stop(call. = FALSE, 'Unknown database: "', name, '"')
        }
      } else {
        return(vctrs::field(db, value))
      }
    }
  }

  set <- function(name, url = NA, desc = NA, id_regex = NA, ranks = NULL) {
    addition <- taxon_db_def(
      name = name,
      url = url,
      desc = desc,
      id_regex = id_regex,
      ranks = list(ranks)
    )
    db_names <- vctrs::field(definitions, 'name')
    new_defs <- definitions
    if (name %in% db_names) {
      new_defs[name == db_names] <- addition
    } else {
      new_defs <- c(new_defs, addition)
    }
    definitions <<- new_defs
  }

  reset <- function() {
    definitions <<- initial_value
  }

  list(get = get, set = set, reset = reset)
}


#' Valid taxonomy databases
#'
#' This defines the valid taxonomic databases that can be used in `taxa_database` objects as a list of
#'
#' @param name (character) name of the database
#' @param url (character) url for the database
#' @param desc (character) description of the database
#' @param id_regex (character) id regex
#' @param ranks Possible taxonomic ranks
#'
#' @section Attribution:
#'
#' This code is based on the code handling options in [knitr].
#'
#' @examples
#'
#' # List all database definitions
#' database_definitions$get()
#'
#' # Get a specific database definition
#' database_definitions$get('ncbi')
#'
#' # Add or overwrite a database definition
#' database_definitions$set(
#'   name = "my_new_database",
#'   url = "http://www.my_tax_database.com",
#'   desc = "I just made this up",
#'   id_regex = ".*"
#' )
#'
#' # Reset definitions to default values
#' database_definitions$reset()
#'
#' @export
database_definitions <- default_database_definitions(database_list)



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------


#' Check regex validity
#'
#' Check if a regular expression is valid
#'
#' @param text The putative regex to check.
#'
#' @keywords internal
is_valid_regex <- function(text)
{
  out <- suppressWarnings(try(grepl(pattern = text, "x"), silent = TRUE))
  return(! inherits(out, "try-error"))
}





