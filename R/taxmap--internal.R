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
#' @return (`integer`) row indexes of the edge list in a `taxmap` object named
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
#' @return A `data` variable with the right format
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
#' @return A `funcs` variable with the right format
#'
#' @keywords internal
validate_taxmap_funcs <- function(funcs) {
  funcs
}


#' used to parse inputs to `drop_obs` and `reassign_obs`
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

#' Get list of usable functions
#'
#' Returns the names of all functions that can be called from any environment
#'
#' @return vector
#'
#' @keywords internal
all_functions <- function() {
  objects <- unlist(lapply(seq_len(length(search())), function(i) ls(pos = i)))
  is_func <- vapply(objects, function(obj) is.function(get(obj)), logical(1))
  return(objects[is_func])
}


#' Check dataset format
#'
#' Check that the datasets in a [taxmap()] object are in the correct format.
#' * Checks that column names are not the names of functions
#'
#' @param obj A [taxmap()] object
#'
#' @return NULL
#'
#' @keywords internal
check_taxmap_data <- function(obj) {
  #  Check that column names are not the names of functions
  data_names <- all_names(obj, funcs = FALSE, builtin_funcs = FALSE)
  suspect_names <- data_names[data_names %in% all_functions()]
  if (length(suspect_names) > 0) {
    warning(paste0("Naming table columns/vectors/lists the same name as ",
                   "functions can sometimes interfere with non-standard ",
                   "evaluation. The following data shares names with ",
                   "functions:\n", limited_print(names(suspect_names),
                                                 type = "silent")),
            call. = FALSE)
  }

  return(invisible(NULL))
}
