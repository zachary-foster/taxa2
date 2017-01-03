#' Taxmap class
#'
#' @export
#' @param ... Any number of object of class \code{\link{hierarchy}} or character vectors.
#' @param data A list of tables with data associated with the taxa.
#' @return An \code{R6Class} object of class \code{Taxmap}
#'
#' @details on initialize, function sorts the taxon list, see
#' \code{\link{ranks_ref}} for the reference rank names and orders
taxmap <- function(...) {
  Taxmap$new(...)
}

Taxmap <- R6::R6Class(
  "Taxmap",
  inherit = Taxonomy,
  public = list(
    data = list(),
    funcs = list(),

    initialize = function(..., data = list(), funcs = list()) {

      # Call `taxonomy` constructor
      super$initialize(...)

      # Make sure `data` is in the right format and add to object
      self$data <- validate_taxmap_data(data, self)

      # Make sure `funcs` is in the right format and add to object
      self$funcs <- validate_taxmap_funcs(funcs, self)
    },

    print = function(indent = "", max_rows = 3, max_items = 3, max_width = getOption("width") - 10) {

      # Call `taxonomy` print method
      taxonomy_output <- paste0(paste0(capture.output(super$print(indent = indent)), collapse = "\n"), "\n")
      cat(gsub(taxonomy_output, pattern = "Taxonomy", replacement = "Taxmap"))

      # Print a subset of each item in data, up to a maximum number, then just print item names
      cat(paste0("  ", length(self$data), " data sets:\n"))
      if (length(self$data) > 0) {
        for (i in 1:min(c(max_items, length(self$data)))) {
          print_item(self$data[[i]], name = names(self$data[i]), max_rows = max_rows, max_width = max_width, prefix = "    ")
        }
        if (length(self$data) > max_items) {
          cat(paste0("    And ", length(self$data) - max_items, " more data sets:"))
          limited_print(names(self$data)[(max_items + 1):length(self$data)])
        }
      }

      # Print the names of functions
      cat(paste0("  ", length(self$funcs), " functions:\n"))
      limited_print(names(self$funcs))

      invisible(self)
    },

    obs = function(data, subset = NULL, recursive = TRUE, simplify = FALSE) {
      # Parse arguments
      subset <- format_taxon_subset(self$edge_list$from, subset)
      if (length(data) == 1 && (data %in% names(self$data) || is.integer(data))) { # data is name/index of dataset in object
        obs_taxon_ids <- extract_taxon_ids(self$data[[data]])
      } else {
        obs_taxon_ids <- extract_taxon_ids(data)
      }

      # Get observations of taxa
      my_subtaxa <- self$subtaxa(subset = subset, recursive = recursive, include_input = TRUE, index = TRUE)
      unique_subtaxa <- unique(unlist(my_subtaxa))
      obs_taxon_index <- match(obs_taxon_ids, self$edge_list$from)
      obs_key <- split(seq_along(obs_taxon_ids), obs_taxon_index)
      output <- stats::setNames(lapply(my_subtaxa, function(x) unname(unlist(obs_key[as.character(x)]))),
                                names(subset))
      is_null <- vapply(output, is.null, logical(1))
      output[is_null] <- lapply(1:sum(is_null), function(x) numeric(0))

      # Convert to return type
      output <- lapply(output, private$get_return_type, return_type = return_type)

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    }
  ),

  private = list(

  )
)



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
#' @keywords internal
extract_taxon_ids <- function(x) {
  if (is.data.frame(x)) {
    if ("taxon_id" %in% colnames(x)) {
      return(x$taxon_ids)
    } else {
      stop('There is no "taxon_id" column in the specified table.')
    }
  } else if (is.list(x) || is.vector(x)) {
    if (is.null(names(x))) {
      stop('The specified object is a list/vector, but is not named by taxon ID.')
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
#'
#' @return A `data` varaible with the right format
#'
#' @keywords internal
validate_taxmap_data <- function(data, self) {

  process_one <- function(x, name) {
    if (is.data.frame(x)) {
      # Convert all data.frames to tibbles
      if  (! tibble::is_tibble(x)) {
        x <- tibble::as_tibble(x)
      }

      # Add the `taxon_id` column if it is not already there
      if ("taxon_id" %in% colnames(x)) {
        message(paste0('Using existing "taxon_id" column for table "', name, '"'))
      } else if ("taxon_index" %in% colnames(x) && is.integer(x$taxon_index)) {
        x$taxon_id <- self$input_ids[x$taxon_index]
      } else if (nrow(x) == length(self$input_ids)) {
        x$taxon_id <- self$input_ids
      } else {
        message(paste('The table "', name, '" does not have a "taxon_index" column or a number of rows equal to the number of inputs, so no "taxon_id" can be assigned.'))
      }
    } else if (is.null(names(x)) && length(x) == length(self$input_ids)) {
      names(x) <- self$input_ids
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
#'
#' @param funcs The `funcs` variable passed to the `Taxmap` constructor
#'
#' @return A `funcs` varaible with the right format
#'
#' @keywords internal
validate_taxmap_funcs <- function(funcs, self) {
  funcs
}


#' Print a table
#'
#' Used to print each table in the `taxmap` print method.
#'
#' @param data The table to be printed
#' @param max_rows (\code{numeric} of length 1) The maximum number of rows in tables to print.
#' @param max_items (\code{numeric} of length 1) The maximum number of list items to print.
#' @param max_width (\code{numeric} of length 1) The maximum number of characters to print.
#' @param prefix (\code{numeric} of length 1) What to print in front of each line.
#'
#' @keywords internal
print_item <- function(data, name = NULL, max_rows = 3, max_items = 3, max_width = getOption("width") - 10, prefix = "") {
  prefixed_print <- function(x, prefix, ...) {
    output <- paste0(prefix, capture.output(print(x, ...)))
    cat(paste0(paste0(output, collapse = "\n"), "\n"))
  }


  if (is.data.frame(data)) {
    loadNamespace("dplyr") # used for tibble print methods
    if (length(name) > 0 && ! is.na(name)) {
      cat(paste0(prefix, name, ":\n"))
    }
    prefixed_print(data, prefix = paste0(prefix, "  "), n = max_rows, width = max_width)
  } else if (is.list(data)) {
    if (length(data) < 1) {
      prefixed_print(list(), prefix = prefix)
    } else {
      cat(paste0(prefix, name, ":\n"))
      prefixed_print(data[1:min(c(max_items, length(data)))], prefix =  paste0(prefix, "  "))
      if (length(data) > max_items) {
        cat(paste0(prefix, "  And ", length(data) - max_items, " more items\n"))
      }
    }
  } else if (is.vector(data)) {
    cat(paste0(prefix, name, ": "))
    limited_print(data, max_chars = max_width)
  } else {
    prefixed_print(data, prefix = prefix)
  }
  invisible(data)
}


#' @export
obs <- function(obj, ...) {
  UseMethod("obs")
}

#' @export
obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
obs.Taxmap <- function(obj, ...) {
  obj$obs(...)
}

#' Get data indexes associated with taxa
#'
#' Given a \code{\link{taxmap}} object, return the indexes
#' associated with each taxon in a given table included in that \code{\link{taxmap}} object.
#'
#' @param obj (\code{taxmap})
#' The \code{taxmap} object containing taxon information to be queried.
#' @param data Either the name of something in \code{obj$data} that has taxon information or a
#' an external object with taxon information.
#' For tables, there must be a column named "taxon_id" and lists/vectors must be named by taxon ID.
#' @param subset (\code{character}) \code{taxon_ids} or indexes for which
#'   observation indexes will be returned. Default: All taxa in \code{obj} will be used.
#' @param recursive (\code{logical})
#' If \code{FALSE}, only return the observation assigned to the specified input taxa, not subtaxa.
#' If \code{TRUE}, return all the observations of every subtaxa, etc.
#' @param simplify (\code{logical}) If \code{TRUE}, then combine all the results into a single
#'   vector of unique observation indexes.
#'
#' @return If \code{simplify = FALSE}, then a list of vectors of observation indexes are returned
#'   corresponding to the \code{target} argument. If \code{simplify = TRUE}, then the observation indexes
#'   for all \code{target} taxa are returned in a single vector.
#'
#' @family taxmap taxonomy functions
#'
#' @name obs
NULL

