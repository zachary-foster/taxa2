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

    # Returns the names of things to be accessible using non-standard evaluation
    all_names = function(tables = TRUE, funcs = TRUE, others = TRUE, warn = FALSE) {
      output <- c()

      # Get column names in each table, removing 'taxon_id'
      is_table <- vapply(self$data, is.data.frame, logical(1))
      if (tables) {
        table_col_names <- unlist(lapply(self$data[is_table], colnames))
        names(table_col_names) <- paste0("data$", rep(names(self$data[is_table]),
                                      vapply(self$data[is_table], ncol, integer(1))))
        table_col_names <- table_col_names[table_col_names != "taxon_id"]
        output <- c(output, table_col_names)
      }

      # Get other object names in data
      if (others) {
        other_names <- names(self$data[!is_table])
        names(other_names) <- rep("data", length(other_names))
        output <- c(output, other_names)
      }


      # Get function names
      if (funcs) {
        func_names <- names(self$funcs)
        names(func_names) <- rep("funcs", length(func_names))
        output <- c(output, func_names)
      }

      # Check for duplicates
      if (warn) {
        duplicated_names <- unique(output[duplicated(output)])
        if (length(duplicated_names) > 0) {
          warning(paste0("The following names are used more than once: ",
                         paste0(duplicated_names, collapse = ", ")))
        }
      }


      # Add the name to the name of the name and return
      names(output) <- paste0(names(output), "$", output)
      return(output)
    },

    # Looks for names of data in a expression for use with non-standard evaulation
    names_used = function(...) {
      decompose <- function(x) {
        if (class(x) %in% c("call", "(")) {
          return(lapply(1:length(x), function(i) decompose(x[[i]])))
        } else {
          return(as.character(x))
        }
      }

      expressions <- lapply(lazyeval::lazy_dots(...), function(x) x$expr)
      if (length(expressions) == 0) {
        return(character(0))
      } else {
        names_used <- unlist(lapply(1:length(expressions), function(i) decompose(expressions[[i]])))
        my_names <- self$all_names()
        return(my_names[my_names %in% names_used])
      }
    },

    # Get a list of all data in an expression used with non-standard evaluation
    data_used = function(...) {

      # Get the data referred to by name
      my_names_used <- self$names_used(...)
      output <- lapply(names(my_names_used),
                       function(x) eval(parse(text = paste0("self$", x))))
      names(output) <- my_names_used

      # Run any functions and return their results instead
      is_func <- vapply(output, is.function, logical(1))
      output[is_func] <- lapply(output[is_func], function(f) f(self))

      return(output)
    },

    obs = function(data, subset = NULL, recursive = TRUE, simplify = FALSE) {
      # Parse arguments
      subset <- format_taxon_subset(names(self$taxa), subset)
      if (length(data) == 1 && (data %in% names(self$data) || is.integer(data))) { # data is name/index of dataset in object
        obs_taxon_ids <- extract_taxon_ids(self$data[[data]])
      } else {
        obs_taxon_ids <- extract_taxon_ids(data)
      }

      # Get observations of taxa
      my_subtaxa <- self$subtaxa(subset = unname(subset), recursive = recursive, include_input = TRUE, return_type = "index") #'unname' is neede for some reason.. something to look into
      unique_subtaxa <- unique(unlist(my_subtaxa))
      obs_taxon_index <- match(obs_taxon_ids, self$edge_list$to)
      obs_key <- split(seq_along(obs_taxon_ids), obs_taxon_index)
      output <- stats::setNames(lapply(my_subtaxa, function(x) unname(unlist(obs_key[as.character(x)]))),
                                names(subset))
      is_null <- vapply(output, is.null, logical(1))
      output[is_null] <- lapply(1:sum(is_null), function(x) numeric(0))

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
      return(x$taxon_id)
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
#' @param subset (\code{character}) Taxon IDs or indexes for which
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


#' @export
all_names <- function(obj, ...) {
  UseMethod("all_names")
}

#' @export
all_names.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
all_names.Taxmap <- function(obj, ...) {
  obj$all_names(...)
}

#' Return names of data in taxmap
#'
#' Return all the valid names that can be used with non-standard evalulation in manipulation functions like \code{filter_taxa}.
#'
#' @param obj (\code{taxmap})
#' The \code{taxmap} object containing taxon information to be queried.
#' @param tables If \code{TRUE}, include the names of columns of tables in \code{obj$data}
#' @param funcs If \code{TRUE}, include the names of user-definable functionsin \code{obj$funcs}.
#' @param others If \code{TRUE}, include the names of data in \code{obj$data} besides tables.
#' @param warn If \code{TRUE}, warn if there are duplicate names.
#'
#' @return \code{character}
#'
#' @name all_names
NULL


#' Get names of taxon_data in an unevaluated expression
#'
#' Get names of taxon_data in an unevaluated expression
#'
#' @param obj a \code{taxmap} object
#' @param ... unevaluated expression
#'
#' @return \code{character}
#'
#' @keywords internal
#' @name names_used
NULL