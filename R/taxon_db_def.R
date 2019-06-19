
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
#' @param rank_levels Valid taxonomic ranks for the database. Should be a list of `numeric` vectors named by taxonomic ranks.
#'
#' @return An `S3` object of class `taxa_taxon_db_def`
#'
#' @keywords internal
new_taxon_db_def <- function(name = character(), url = character(), desc = character(),
                             id_regex = character(), rank_levels = list()) {
  vctrs::vec_assert(name, ptype = character())
  vctrs::vec_assert(url, ptype = character())
  vctrs::vec_assert(desc, ptype = character())
  vctrs::vec_assert(id_regex, ptype = character())
  vctrs::vec_assert(rank_levels, ptype = list())

  vctrs::new_rcrd(list(name = name, url = url, desc = desc, id_regex = id_regex, rank_levels = rank_levels),
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
                         id_regex = NA_character_, rank_levels = rep(list(NULL), length(name))) {

  # Cast inputs to correct type
  name <- vctrs::vec_cast(name, character())
  url <- vctrs::vec_cast(url, character())
  desc <- vctrs::vec_cast(desc, character())
  id_regex <- vctrs::vec_cast(id_regex, character())
  rank_levels <- vctrs::vec_cast(rank_levels, list())

  # Check that rank_levels are right format
  # valid_ranks <- vapply(rank_levels, FUN.VALUE = logical(1), function(x) {
  #   is_taxon_rank_level(x) || is.null(x)
  # })
  # if (any(! valid_ranks)) {
  #   stop(call. = FALSE,
  #        'Invalid rank_levels. Must be a list of factors. The following indexes are not factors:\n',
  #        limited_print(type = 'silent', prefix = '  ', which(! valid_ranks)))
  # }

  # Convert rank_levels into taxon_rank_level class
  rank_levels <- lapply(rank_levels, function(lev) {
   if (is.null(lev)) {
     return(lev)
   } else {
     return(taxon_rank_level(lev))
   }
  })

  # Recycle inputs to common length
  c(name, url, desc, id_regex, rank_levels) %<-% vctrs::vec_recycle_common(name, url, desc, id_regex, rank_levels)

  # Create new object
  new_taxon_db_def(name, url, desc, id_regex, rank_levels)
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
  screen_width <- round(getOption("width") * 0.9)
  max_value_nchar <- round(screen_width * 0.8)

  parts <- lapply(seq_len(length(x)), function(i) {

    # Format key/value pairs for printing
    keys <- vctrs::fields(x)
    keys <- keys[keys != 'name']
    keys_printed <- paste0(keys, ': ')
    keys_printed <- stringr::str_pad(keys_printed, max(nchar(keys_printed)), side = 'right')
    values <- vapply(keys, FUN.VALUE = character(1), function(key) {
      value <- vctrs::field(x, key)[[i]]
      out <- toString(value)
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

  # row_num <- floor(cumsum(part_len) / screen_width) + 1
  current_pos <- 0
  current_row <- 1
  row_num <- vapply(part_len, FUN.VALUE = numeric(1), function(len) {
    current_pos <<- current_pos + len
    if (current_pos >= screen_width) {
      current_row <<- current_row + 1
      current_pos <<- len
    }
    current_row
  })


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
# S3 casting functions
#--------------------------------------------------------------------------------

#' @method vec_cast taxa_taxon_db_def
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_db_def
#' @keywords internal
vec_cast.taxa_taxon_db_def <- function(x, to) UseMethod("vec_cast.taxa_taxon_db_def")


#' @method vec_cast.taxa_taxon_db_def default
#' @export
vec_cast.taxa_taxon_db_def.default <- function(x, to) vctrs::vec_default_cast(x, to)


#' @method vec_cast.taxa_taxon_db_def taxa_taxon_db_def
#' @export
vec_cast.taxa_taxon_db_def.taxa_taxon_db_def <- function(x, to) x




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





