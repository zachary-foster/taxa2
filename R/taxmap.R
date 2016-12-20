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
    }
  ),

  private = list(

  )
)


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
