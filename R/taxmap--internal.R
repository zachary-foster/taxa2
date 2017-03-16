#' Format taxon subset value
#'
#' Format an input to a \code{subset} option on functions like
#' \code{\link{supertaxa}}. Converts logical and \code{taxon_ids} into indexes
#' of \code{taxon_data}.
#'
#' @param obj (\code{taxmap}) The \code{taxmap} object containing taxon
#'   information to be queried.
#' @param index If a \code{character}, then it should be values of
#'   \code{taxon_ids}. If a \code{numeric}, then it should be row indexes of
#'   \code{taxon_data}. If a \code{logical}, then it should correspond to rows
#'   of \code{taxon_data}.
#'
#' @return \code{numeric}
#'
#' @keywords internal
format_taxon_subset <- function(taxon_ids, index) {
  if (is.null(index)) {
    output <- stats::setNames(seq_along(taxon_ids), taxon_ids)
  } else {
    if (is.null(names(index))) {
      names(index) <- index
    }
    if (is.numeric(index)) {
      output <- index
      my_names <- names(index)
    } else if (is.character(index)) {
      output <- match(index, taxon_ids)
      my_names <- names(index)
    } else if (is.logical(index)) {
      output <- which(index)
      my_names <- names(index)[output]
    } else {
      stop("Invalid subset value.")
    }
    names(output) <- my_names
  }
  return(output)
}


#' Find taxon id info in data set
#'
#' Look for taxon ids in an object.
#' For tables, this would be a "taxon_id" column.
#' For lists/vectors, it would be names.
#'
#' @param x Something to look for taxon ids in.
#'
#' @return Taxon ids if found, otherwise throw an error.
#'
#' @examples
#' taxa:::extract_taxon_ids(ex_taxmap$data$info)
#'
#' @keywords internal
extract_taxon_ids <- function(x) {
  if (is.data.frame(x)) {
    if ("taxon_id" %in% colnames(x)) {
      return(x$taxon_id)
    } else {
      stop('There is no "taxon_id" column in the specified table.')
    }
  } else if (is.list(x) || is.vector(x)) {
    if (is.null(names(x))) {
      stop('The specified object is a list/vector, but not named by taxon ID.')
    } else {
      return(names(x))
    }
  } else {
    stop("Invalid object type supplied.")
  }
}


#' Convert `data` input for Taxamp
#'
#' Make sure `data` is in the right format and complain if it is not.
#' Then, add a `taxon_id` column to data with the same length as the input
#'
#' @param data The `data` variable passed to the `Taxmap` constructor
#' @param input_ids
#'
#' @return A `data` varaible with the right format
#'
#' @keywords internal
validate_taxmap_data <- function(data, input_ids) {

  process_one <- function(x, name) {
    if (is.data.frame(x)) {
      # Convert all data.frames to tibbles
      if  (! tibble::is_tibble(x)) {
        x <- tibble::as_tibble(x)
      }

      # Add the `taxon_id` column if it is not already there
      if ("taxon_id" %in% colnames(x)) {
        message(paste0('Using existing "taxon_id" column for table "',
                       name, '"'))
      } else if ("taxon_index" %in% colnames(x) && is.integer(x$taxon_index)) {
        x$taxon_id <- input_ids[x$taxon_index]
      } else if (nrow(x) == length(input_ids)) {
        x$taxon_id <- input_ids
      } else {
        message(paste('The table "', name,
                      '" does not have a "taxon_index" column or a number of ',
                      'rows equal to the number of inputs, so no "taxon_id"',
                      ' can be assigned.'))
      }
    } else if (is.null(names(x)) && length(x) == length(input_ids)) {
      names(x) <- input_ids
    }
    return(x)
  }

  # Get names of data inputs for messages
  data_names <- names(data)
  if (is.null(data_names)) {
    data_names <- rep(NA, length(data))
  }
  data_names <- ifelse(is.na(data_names) | data_names == "",
                       paste0("input_", seq_along(data)),
                       data_names)

  # Process each input
  mapply(process_one, data, data_names, SIMPLIFY = FALSE)
}


#' Validate `funcs` input for Taxamp
#'
#' Make sure `funcs` is in the right format and complain if it is not.
#' NOTE: This currently does nothing.
#'
#' @param funcs The `funcs` variable passed to the `Taxmap` constructor
#'
#' @return A `funcs` varaible with the right format
#'
#' @keywords internal
validate_taxmap_funcs <- function(funcs) {
  funcs
}


#' Print a table
#'
#' Used to print each item in the `taxmap` print method.
#'
#' @param data The item to be printed
#' @param max_rows (\code{numeric} of length 1) The maximum number of rows in
#'   tables to print.
#' @param max_items (\code{numeric} of length 1) The maximum number of list
#'   items to print.
#' @param max_width (\code{numeric} of length 1) The maximum number of
#'   characters to print.
#' @param prefix (\code{numeric} of length 1) What to print in front of each
#'   line.
#'
#' @examples
#' taxa:::print_item(ex_taxmap$data$info)
#' taxa:::print_item(1:100)
#'
#' @keywords internal
print_item <- function(data, name = NULL, max_rows = 3, max_items = 3,
                       max_width = getOption("width") - 10, prefix = "") {
  prefixed_print <- function(x, prefix, ...) {
    output <- paste0(prefix, utils::capture.output(print(x, ...)))
    cat(paste0(paste0(output, collapse = "\n"), "\n"))
  }
  arrange_obs

  if (is.data.frame(data)) {
    loadNamespace("dplyr") # used for tibble print methods
    if (length(name) > 0 && ! is.na(name)) {
      cat(paste0(prefix, name, ":\n"))
    }
    prefixed_print(data, prefix = paste0(prefix, "  "), n = max_rows,
                   width = max_width)
  } else if (is.list(data)) {
    if (length(data) < 1) {
      prefixed_print(list(), prefix = prefix)
    } else {
      cat(paste0(prefix, name, ": a list with ", length(data),
                 ifelse(length(data) == 1, " item", " items"), "\n"))
    }
  } else if (is.vector(data)) {
    cat(paste0(prefix, name, ": "))
    limited_print(data, max_chars = max_width)
  } else {
    prefixed_print(data, prefix = prefix)
  }
  invisible(data)
}


#' @keywords internal
get_data_taxon_ids <- function(x) {
  if (is.data.frame(x)) {
    data_taxon_ids <- x$taxon_id
  } else {
    data_taxon_ids <- names(x)
  }
  return(data_taxon_ids)
}
