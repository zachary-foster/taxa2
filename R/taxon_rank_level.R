
#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_rank_level constructor
#'
#' Minimal taxon_rank_level constructor for internal use. Only use when the
#' input is known to be valid since few validity checks are done.
#'
#' @param level Zero or more taxonomic rank names. If a named numeric is
#'   appplied, the names are used for levels and the numberic values are used
#'   for the order. Inputs will be transformed to a `character` vector.
#' @param order Intergers that determine the relative order of taxonomic levels.
#'   Inputs will be transformed to a `interger` vector. `NA`s can be used to
#'   indicate that the order is not known.
#'
#' @return An `S3` object of class `taxa_taxon_rank_level`
#'
#' @keywords internal
new_taxon_rank_level <- function(level = character(), order = numeric()) {
  vctrs::vec_assert(level, ptype = character())
  vctrs::vec_assert(order, ptype = numeric())
  vctrs::new_rcrd(list(level = level, order = order), class = "taxa_taxon_rank_level")
}


#' Taxon ID class
#'
#' Used to store taxon IDs, either arbitrary or from a taxonomy database. This is typically used to
#' store taxon IDs in [taxon()] objects.
#'
#' @inheritParams new_taxon_rank_level
#' @param guess_order If `TRUE` and no order is given, try to guess order based on rank names.
#' @param impute_na If `TRUE`, fill in NAs based on nearby values (assumed in ascending order).
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon_rank_level`
#' @family classes
#'
#' @export
taxon_rank_level <- function(level = character(), order = NULL, guess_order = TRUE, impute_na = FALSE) {

  # Accept named numeric as input if supplied
  if (is.null(order) && (is.numeric(level) || all(is.na(level))) && ! is.null(names(level))) {
    order <- unname(level)
    level <- names(level)
  }

  # Use order of known ranks if no order is defined for any taxon
  order_inferred <- is.null(order)
  if (is.null(order)) {
    if (guess_order) {
      order <- unname(known_taxon_rank_levels[tolower(level)])
      known_orders <- order[!is.na(order)]
      if (impute_na && length(known_orders) > 0 && any(diff(known_orders) < 0)) {
      stop(call. = FALSE,
           'If both `guess_order` and `impute_na` are TRUE, then known ranks must be in ascending order. ',
           'If you really want to do this, try specifying the level order manually.')
      }
    } else {
      order <- NA
    }
  }

  # Coerce inputs to right data types
  level <- vctrs::vec_cast(level, character())
  order <- vctrs::vec_cast(order, numeric())

  # Recycle to common length
  c(level, order) %<-% vctrs::vec_recycle_common(level, order)

  # Fill in NAs based on nearby values (assumed in ascending order)
  if (impute_na && order_inferred) {
    mean_inc_between_ranks <- round(mean(diff(unique(known_taxon_rank_levels)), na.rm = TRUE))
    order <- impute_order_na(order, inc = mean_inc_between_ranks)
  }

  # Reorder inputs in specified order
  c(level, order) %<-% check_taxon_rank_order(level, order, warn = FALSE)

  # Check that levels are unique
  not_unique <- which(duplicated(level))
  if (length(not_unique) > 0) {
    stop(call. = FALSE,
         'Taxon rank levels must be unique. The following levels are not unique:\n',
         limited_print(prefix = '  ', type = 'silent', unique(level[not_unique])))
  }

  new_taxon_rank_level(level, order)
}


#' @importFrom methods setOldClass
methods::setOldClass(c("taxa_taxon_rank_level", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------




#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------


#' @export
#' @keywords internal
format.taxa_taxon_rank_level <- function(x, ...) {
  vctrs::field(x, "level")
}


#' Prepare taxon_rank_level for printing
#'
#' Prepare taxon_rank_level for printing. Makes color optional.
#'
#' @param color Use color?
#'
#' @return character
#'
#' @keywords internal
printed_taxon_rank_level <- function(x, color = FALSE) {
  if (length(x) == 0) {
    return()
  }

  lev <- vctrs::field(x, "level")
  ord <- vctrs::field(x, "order")
  unordered <- lev[is.na(ord)]
  lev <- lev[!is.na(ord)]
  ord <- ord[!is.na(ord)]

  if (length(lev) > 1) {
    out <- paste(lev,
                        c(ifelse(ord[1:(length(lev) - 1)] == ord[2:length(lev)],
                                 font_punct("="),
                                 font_punct("<")), ""),
                        collapse = " ")
    out <- trimws(out)
  } else {
    out <- lev
  }
  out <- paste(c(out, unordered), collapse = font_punct(' ? '))

  if (! color) {
    out <- crayon::strip_style(out)
  }
  return(out)
}


#' @export
#' @keywords internal
obj_print_data.taxa_taxon_rank_level <- function(x) {
  level_text <- paste0(printed_taxon_rank_level(x, color = TRUE), '\n')
  cat(level_text)
}


#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_rank_level <- function(x) {
  "tax_rank_lev"
}


#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_rank_level <- function(x) {
  "taxon_rank_level"
}


#' @export
#' @keywords internal
toString.taxa_taxon_rank_level <- function(x) {
  printed_taxon_rank_level(x, color = FALSE)
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @method vec_type2 taxa_taxon_rank_level
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon_rank_level
#' @keywords internal
vec_type2.taxa_taxon_rank_level <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon_rank_level", y)


#' @method vec_type2.taxa_taxon_rank_level default
#' @export
vec_type2.taxa_taxon_rank_level.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.taxa_taxon_rank_level vctrs_unspecified
#' @export
vec_type2.taxa_taxon_rank_level.vctrs_unspecified <- function(x, y, ...) x


#' @method vec_type2.taxa_taxon_rank_level taxa_taxon_rank_level
#' @export
vec_type2.taxa_taxon_rank_level.taxa_taxon_rank_level <- function(x, y, ...) new_taxon_rank_level()


#' @method vec_type2.taxa_taxon_rank_level character
#' @export
vec_type2.taxa_taxon_rank_level.character <- function(x, y, ...) character()


#' @method vec_type2.character taxa_taxon_rank_level
#' @importFrom vctrs vec_type2.character
#' @export
vec_type2.character.taxa_taxon_rank_level <- function(x, y, ...) character()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @method vec_cast taxa_taxon_rank_level
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_rank_level
#' @keywords internal
vec_cast.taxa_taxon_rank_level <- function(x, to) UseMethod("vec_cast.taxa_taxon_rank_level")


#' @method vec_cast.taxa_taxon_rank_level default
#' @export
vec_cast.taxa_taxon_rank_level.default <- function(x, to) vctrs::vec_default_cast(x, to)


#' @method vec_cast.taxa_taxon_rank_level taxa_taxon_rank_level
#' @export
vec_cast.taxa_taxon_rank_level.taxa_taxon_rank_level <- function(x, to) x


#' @method vec_cast.taxa_taxon_rank_level character
#' @export
vec_cast.taxa_taxon_rank_level.character <- function(x, to) taxon_rank_level(x)


#' @method vec_cast.character taxa_taxon_rank_level
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon_rank_level <- function(x, to) vctrs::field(x, "level")



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
is_taxon_rank_level <- function(x) {
  inherits(x, "taxa_taxon_rank_level")
}


#' @export
c.taxa_taxon_rank_level <- function(...) {
  input <- list(...)
  combined <- setNames(unlist(lapply(input, function(x) vctrs::field(x, 'order'))),
                       unlist(lapply(input, function(x) vctrs::field(x, 'level'))))
  combined <- sort(combined, na.last = TRUE)

  # Resolve multiple ranks with same name
  combined <- vapply(split(combined, names(combined)), FUN.VALUE = numeric(1), function(x) {
    if (length(x) == 1) {
      return(x)
    }
    rank_name <- unique(names(x))
    if (all(diff(which(names(combined) == rank_name)) == 1)) {
      return(mean(x))
    } else {
      return(NA)
    }
  })
  combined <- sort(combined, na.last = TRUE)

  # Make new taxon_rank_level object
  taxon_rank_level(combined)
}



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------


#' Check that order is ascending
#'
#' Check that order is ascending and reorder the orders and thier levels if needed.
#'
#' @param level Zero or more taxonomic rank names.
#' @param order Intergers that determine the relative order of taxonomic levels.
#' @param warn If `TRUE`, issue a warning when not in ascending order.
#'
#' @keywords internal
check_taxon_rank_order <- function(level, order, warn = FALSE) {
  sorted <- sort(order, na.last = TRUE)
  if (! all((!is.na(sorted == order) & sorted == order) | (is.na(sorted) & is.na(order)))) {
    if (warn) {
      warning(call. = FALSE,
              'Levels and order not supplied in ascending order. Reordering.')
    }
    level <- level[order(order)]
    order <- order[order(order)]
  }
  return(list(level = level, order = order))
}


#' Fill in NA values in sequence
#'
#' Fill in the NA values in a ascending sequence based on nearby non-NA values.
#' Used to guess the order values for unknown ranks based on the values of known ranks.
#'
#' @param order An ascending sequences, possibly with NAs
#' @param inc The increment size to use for values in NA blocks at the start and end of the sequence.
#'
#' @keywords internal
impute_order_na <- function(order, inc = 1) {

  # If no NAs, return input
  if (all(! is.na(order))) {
    return(order)
  }

  # If all NA, return ascending sequence
  if (all(is.na(order))) {
    return(seq_len(length(order)) * inc)
  }

  # Check that sequences is increasing
  if (! all(diff(order[! is.na(order)]) >= 0)) {
    stop('Rank orders must be increasing in impute unknown rank values.')
  }

  # Find contiguous blocks of NA
  na_blocks <- data.frame(unclass(rle(is.na(order))))
  na_blocks$end <- cumsum(na_blocks$lengths)
  na_blocks$start <- na_blocks$end - na_blocks$lengths + 1
  na_blocks <- na_blocks[na_blocks$values, ]

  # Replace blocks of NA with imputed values
  for (i in seq_len(nrow(na_blocks))) {
    start <- na_blocks$start[i]
    end <- na_blocks$end[i]
    if (start == 1L) {
      fill <- order[end + 1] - rev(seq_len(end - start + 1)) * inc
    } else if (end == length(order)) {
      fill <- order[start - 1] + seq_len(end - start + 1) * inc
    } else {
      fill <- seq(from = order[start - 1], to = order[end + 1], length.out = end - start + 3)
      fill <- fill[2:(length(fill) - 1)]
    }
    order[start:end] <- fill
  }

  return(order)
}
