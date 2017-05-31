#' Format taxon subset value
#'
#' Format an input to a `subset` option on functions like [supertaxa()].
#' Converts logical and `taxon_ids` into indexes of `taxon_data`.
#'
#' @param taxon_ids All of the taxon ids from a `taxmap` object.
#' @param index If a `character`, then it should be values of `taxon_ids`. If a
#'   `numeric`, then it should be row indexes of `taxon_data`. If a `logical`,
#'   then it should correspond to rows of `taxon_data`.
#'
#' @return (`integer`) row indexes of the edge list in a `taxmap` object nammed
#'   by corresponding taxon ids
#'
#' @keywords internal
format_taxon_subset <- function(taxon_ids, index) {
  if (is.null(index)) {
    output <- stats::setNames(seq_along(taxon_ids), taxon_ids)
  } else {
    if (is.numeric(index)) {
      output <- index
    } else if (is.character(index)) {
      output <- match(index, taxon_ids)
    } else if (is.logical(index)) {
      output <- which(index)
    } else {
      stop("Invalid subset value.")
    }
    names(output) <- taxon_ids[output]
  }
  return(output)
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
#' @param max_rows (`numeric` of length 1) The maximum number of rows in
#'   tables to print.
#' @param max_items (`numeric` of length 1) The maximum number of list
#'   items to print.
#' @param max_width (`numeric` of length 1) The maximum number of
#'   characters to print.
#' @param prefix (`numeric` of length 1) What to print in front of each
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



#' used to parse inputs to `taxonless` and `reassign_obs`
#'
#' @keywords internal
parse_possibly_named_logical <- function(input, data, default) {
  if (is.null(names(input))) {
    if (length(input) == 1) {
      output <- stats::setNames(rep(input, length(data)),
                                names(data))
    } else if (length(input) == length(data)) {
      output <- stats::setNames(input, names(data))
    } else {
      stop(paste("Invalid input for logical vector selecting which data",
                  "sets to affect. Valid inputs include:\n",
                  "1) a single unnamed logical (e.g. TRUE)\n",
                  "2) one or more named logicals with names matching",
                  "data sets in obj$data (e.g. c(data_1 = TRUE, data_2",
                  "= FALSE)\n  3) an unamed logical vector of the same",
                  "length as obj$data."))
    }
  } else {
    if (length(not_data_names <-
               names(input)[! names(input) %in% names(data)]) > 0) {
      stop(paste0("Invalid input for logical vector selecting which data",
                  " sets to affect. The following names are not in",
                  " data: ",
                  paste0(not_data_names, collapse = ", ")))
    }
    output <- stats::setNames(rep(default, length(data)),
                              names(data))
    output[names(input)] <- input
  }
  return(output)
}

