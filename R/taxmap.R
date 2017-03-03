#' Taxmap class
#'
#' @export
#' @param ... Any number of object of class \code{\link{hierarchy}} or character vectors.
#' @param data A list of tables with data associated with the taxa.
#' @return An \code{R6Class} object of class \code{\link{taxmap}}
#'
#' @details on initialize, function sorts the taxon list based on rank (if rank information is available), see
#' \code{\link{ranks_ref}} for the reference rank names and orders
#'
#' @template taxmapegs
taxmap <- function(..., data = NULL) {
  Taxmap$new(..., data = data)
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

      # Add taxon id and taxon name
      if (others) {
        output <- c(output, "taxon_names", "taxon_ids")
      }

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
      names(output) <- paste0(names(output), ifelse(names(output) == "", "", "$"), output)
      return(output)
    },

    # Looks for names of data in a expression for use with non-standard evaulation
    names_used = function(...) {
      decompose <- function(x) {
        if (class(x) %in% c("call", "(", "{")) {
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

    # Get data by name
    get_data = function(name) {
      my_names <- self$all_names()
      if (any(unknown <- !name %in% my_names)) {
        stop(paste0("Cannot find the following data: ", paste0(name[unknown], collapse = ", "), "\n ",
                    "Valid choices include: ",  paste0(my_names, collapse = ", "), "\n "))
      }
      name <- my_names[match(name, my_names)]
      output <- lapply(names(name),
                       function(x) eval(parse(text = paste0("self$", x))))
      names(output) <- name

      # Run any functions and return their results instead
      is_func <- vapply(output, is.function, logical(1))
      output[is_func] <- lapply(output[is_func], function(f) {
        if (length(formals(f)) == 0) {
          return(f())
        } else {
          return(f(self))
        }
      })

      return(output)
    },

    # Get a list of all data in an expression used with non-standard evaluation
    data_used = function(...) {
      my_names_used <- self$names_used(...)
      self$get_data(my_names_used)
    },

    # Return all data
    all_data = function(...) {
      self$get_data(self$all_names(...))
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
      if (recursive) {
        my_subtaxa <- self$subtaxa(subset = unname(subset), recursive = TRUE, include_input = TRUE, return_type = "index") #'unname' is neede for some reason.. something to look into
      } else {
        my_subtaxa <- subset
      }
      obs_taxon_index <- match(obs_taxon_ids, self$taxon_ids())
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
    },

    filter_taxa = function(..., subtaxa = FALSE, supertaxa = FALSE, taxonless = FALSE,
                           reassign_obs = TRUE, reassign_taxa = TRUE, invert = FALSE) {

      parse_possibly_named_logical <- function(input, default) { # used to parse inputs to `taxonless` and `reassign_obs`
        if (is.null(names(input))) {
          if (length(input) == 1) {
            output <- stats::setNames(rep(input, length(self$data)), names(self$data))
          } else if (length(input) == length(self$data)) {
            output <- stats::setNames(input, names(self$data))
          } else {
            error("Invalid input for logical vector selecting which data sets to affect. Valid inputs include:\n  1) a single unnamed logical (e.g. TRUE)\n  2) one or more named logicals with names matching data sets in obj$data (e.g. c(data_1 = TRUE, data_2 = FALSE)\n  3) an unamed logical vector of the same length as obj$data.")
          }
        } else {
          if (length(not_data_names <- names(input)[! names(input) %in% names(self$data)]) > 0) {
            stop(paste0("Invalid input for logical vector selecting which data sets to affect. The following names are not in self$data: ",
                        paste0(not_data_names, collapse = ", ")))
          }
          output <- stats::setNames(rep(default, length(self$data)), names(self$data))
          output[names(input)] <- input
        }
        return(output)
      }

      # non-standard argument evaluation
      selection <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), data = self$data_used(...))

      # convert taxon_ids to logical
      is_char <- vapply(selection, is.character, logical(1))
      selection[is_char] <- lapply(selection[is_char], function(x) self$taxon_ids() %in% x)

      # convert indexes to logical
      is_index <- vapply(selection, is.numeric, logical(1))
      selection[is_index] <- lapply(selection[is_index], function(x) 1:nrow(self$edge_list) %in% x)

      # combine filters
      selection <- Reduce(`&`, selection)

      # Get taxa of subset
      taxa_subset <- unique(c(which(selection),
                              if (subtaxa) {
                                self$subtaxa(subset = selection, recursive = TRUE, return_type = "index",
                                             include_input = FALSE, simplify = TRUE)
                              },
                              if (supertaxa) {
                                self$supertaxa(subset = selection, recursive = TRUE, return_type = "index",
                                               na = FALSE, simplify = TRUE, include_input = FALSE)
                              }))

      # Invert selection
      if (invert) {
        taxa_subset <- (1:nrow(self$edge_list))[-taxa_subset]
      }

      # Reassign taxonless observations
      reassign_obs <- parse_possibly_named_logical(reassign_obs, default = formals(self$filter_taxa)$reassign_obs)
      process_one <- function(data_index) {

        reassign_one <- function(parents) {
          included_parents <- parents[parents %in% taxa_subset]
          return(self$taxon_ids()[included_parents[1]])
        }

        # Get the taxon ids of the current object
        if (is.null((data_taxon_ids <- get_data_taxon_ids(self$data[[data_index]])))) {
          return(NULL) # if there is no taxon id info, dont change anything
        }

        # Generate replacement taxon ids
        to_reassign <- ! data_taxon_ids %in% self$taxon_ids()[taxa_subset]
        supertaxa_key <- self$supertaxa(subset = unique(data_taxon_ids[to_reassign]),
                                        recursive = TRUE, simplify = FALSE, include_input = FALSE,
                                        return_type = "index", na = FALSE)
        reassign_key <- vapply(supertaxa_key, reassign_one, character(1))
        new_data_taxon_ids <- reassign_key[data_taxon_ids[to_reassign]]

        # Replace taxon ids
        if (is.data.frame(self$data[[data_index]])) {
          self$data[[data_index]][to_reassign, "taxon_id"] <- new_data_taxon_ids
        } else {
          names(self$data[[data_index]])[to_reassign] <- new_data_taxon_ids
        }
      }

      unused_output <- lapply(seq_along(self$data)[reassign_obs], process_one)

      # Reassign subtaxa
      if (reassign_taxa) {
        reassign_one <- function(parents) {
          included_parents <- parents[parents %in% taxa_subset]
          return(self$taxon_ids()[included_parents[1]])
        }

        to_reassign <- ! self$edge_list$from %in% self$taxon_ids()[taxa_subset]
        supertaxa_key <- self$supertaxa(subset = unique(self$taxon_ids()[to_reassign]),
                                        recursive = TRUE, simplify = FALSE, include_input = FALSE,
                                        return_type = "index", na = FALSE)
        reassign_key <- vapply(supertaxa_key, reassign_one, character(1))
        self$edge_list[to_reassign, "from"] <- reassign_key[self$taxon_ids()[to_reassign]]
      }


      # Remove taxonless observations
      taxonless <- parse_possibly_named_logical(taxonless, default = formals(self$filter_taxa)$taxonless)
      process_one <- function(my_index) {

        # Get the taxon ids of the current object
        if (is.null((data_taxon_ids <- get_data_taxon_ids(self$data[[my_index]])))) {
          return(NULL) # if there is no taxon id info, dont change anything
        }

        obs_subset <- data_taxon_ids %in% self$taxon_ids()[taxa_subset]
        if (taxonless[my_index]) {
          if (is.data.frame(self$data[[my_index]])) {
            self$data[[my_index]][! obs_subset, "taxon_id"] <- as.character(NA)
          } else {
            names(self$data[[my_index]])[! obs_subset] <- as.character(NA)
          }
        } else {
          if (is.data.frame(self$data[[my_index]])) {
            self$data[[my_index]] <- self$data[[my_index]][obs_subset, , drop = FALSE]
          } else {
            self$data[[my_index]] <- self$data[[my_index]][obs_subset]
          }
        }

      }
      unused_output <- lapply(seq_along(self$data), process_one)



      # Remove filtered taxa
      self$taxa <- self$taxa[self$taxon_ids()[taxa_subset]]
      self$edge_list <- self$edge_list[taxa_subset, , drop = FALSE]
      self$edge_list[! self$edge_list$from %in% self$taxon_ids(), "from"] <- as.character(NA)

      return(self)
    },






    filter_obs = function(target, ..., unobserved = TRUE) {
      # non-standard argument evaluation
      selection <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), data = self$data_used(...))

      # convert taxon_ids to indexes
      is_char <- vapply(selection, is.character, logical(1))
      if (sum(is_char) > 0) {
        stop("observation filtering with taxon IDs is not currently supported. If you want to filter observation by taxon IDs, use something like: `obj$data$my_target$taxon_ids %in% my_subset`")
      }

      # convert logical to indexes
      is_logical <- vapply(selection, is.logical, logical(1))
      selection[is_logical] <- lapply(selection[is_logical], which)

      # combine filters
      intersect_with_dups <-function(a, b) {
        #taken from http://r.789695.n4.nabble.com/intersect-without-discarding-duplicates-td2225377.html
        rep(sort(intersect(a, b)), pmin(table(a[a %in% b]), table(b[b %in% a])))
      }
      selection <- Reduce(intersect_with_dups, selection)

      # Remove observations
      data_taxon_ids <- get_data_taxon_ids(self$data[[target]]) # used in removing unobserved taxa but must be calculated before filtering observations
      self$data[[target]] <- self$data[[target]][selection, , drop = FALSE]

      # Remove unobserved taxa
      if (! unobserved & ! is.null(data_taxon_ids)) {
        unobserved_taxa <- self$supertaxa(unique(data_taxon_ids[-selection]), na = FALSE,
                                          recursive = TRUE, simplify = TRUE, include_input = TRUE, return_type = "index")
        taxa_to_remove <- 1:nrow(self$edge_list) %in% unobserved_taxa & vapply(self$obs(target), length, numeric(1)) == 0
        self$taxa <- self$taxa[self$taxon_ids()[! taxa_to_remove]]
        self$edge_list <- self$edge_list[! taxa_to_remove, , drop = FALSE]
        self$edge_list[! self$edge_list$from %in% self$taxon_ids(), "from"] <- as.character(NA)
        # Todo: deal with other objects in data that had their taxa removed. There should be a `remove_taxa` function that this and `filter_taxa` could use.
      }

      return(self)
    },

    select_obs = function(target, ...) {
      self$data[[target]] <- dplyr::bind_cols(self$data[[target]][ , c("taxon_id"), drop = FALSE],
                                              dplyr::select(self$data[[target]], ...))
      return(self)
    },


    mutate_obs = function(target, ...) {
      data_used <- self$data_used(...)
      unevaluated <- lazyeval::lazy_dots(...)
      for (index in seq_along(unevaluated)) {
        new_col <- lazyeval::lazy_eval(unevaluated[index], data = data_used)
        data_used <- c(data_used, new_col) # Allow this col to be used in evaluating the next cols
        self$data[[target]][[names(new_col)]] <- new_col[[1]]
      }
      return(self)
    },


    transmute_obs = function(target, ...) {
      result <- list()
      data_used <- self$data_used(...)
      unevaluated <- lazyeval::lazy_dots(...)
      for (index in seq_along(unevaluated)) {
        new_col <- lazyeval::lazy_eval(unevaluated[index], data = data_used)
        data_used <- c(data_used, new_col) # Allow this col to be used in evaluating the next cols
        result[[names(new_col)]] <- new_col[[1]]
      }
      self$data[[target]] <- tibble::as_tibble(result)
      return(self)
    },


    arrange_obs = function(target, ...) {
      data_used <- self$data_used(...)
      data_used <- data_used[! names(data_used) %in% names(self$data[[target]])] # These are not needed since they are already in the table being sorted
      if (length(data_used) == 0) {
        self$data[[target]] <- dplyr::arrange(self$data[[target]], ...)
      } else {
        target_with_extra_cols <- dplyr::bind_cols(data_used, self$data[[target]])
        self$data[[target]] <- dplyr::arrange(target_with_extra_cols, ...)[, -seq_along(data_used)]
      }

      return(self)
    },

    arrange_taxa = function(...) {
      data_used <- self$data_used(...)
      data_used <- data_used[! names(data_used) %in% names(self$edge_list)] # These are not needed since they are already in the table being sorted
      if (length(data_used) == 0) {
        self$edge_list <- dplyr::arrange(self$edge_list, ...)
      } else {
        target_with_extra_cols <- dplyr::bind_cols(data_used, self$edge_list)
        self$edge_list <- dplyr::arrange(target_with_extra_cols, ...)[, -seq_along(data_used)]
      }

      return(self)
    },


    sample_n_obs = function(target, size, replace = FALSE, taxon_weight = NULL, obs_weight = NULL,
                            use_supertaxa = TRUE, collapse_func = mean, ...) {
      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(taxon_weight, obs_weight)))
      taxon_weight <- lazyeval::lazy_eval(lazyeval::lazy(taxon_weight), data = data_used)
      obs_weight <- lazyeval::lazy_eval(lazyeval::lazy(obs_weight), data = data_used)

      # Calculate taxon component of taxon weights
      if (is.null(taxon_weight)) {
        obs_taxon_weight <- rep(1, nrow(self$data[[target]]))
      } else {
        obs_index <- match(get_data_taxon_ids(self$data[[target]]), self$taxon_ids())
        my_supertaxa <- self$supertaxa(recursive = use_supertaxa, simplify = FALSE,
                                       include_input = TRUE, return_type = "index", na = FALSE)
        taxon_weight_product <- vapply(my_supertaxa, function(x) collapse_func(taxon_weight[x]), numeric(1))
        obs_taxon_weight <- taxon_weight_product[obs_index]
      }
      obs_taxon_weight <- obs_taxon_weight / sum(obs_taxon_weight)

      # Calculate observation component of observation weights
      if (is.null(obs_weight)) {
        obs_weight <- rep(1, nrow(self$data[[target]]))
      }
      obs_weight <- obs_weight / sum(obs_weight)

      # Combine observation and taxon weight components
      combine_func <- prod
      weight <- mapply(obs_taxon_weight, obs_weight, FUN = function(x, y) combine_func(c(x,y)))
      weight <- weight / sum(weight)

      # Sample observations
      sampled_rows <- sample.int(nrow(self$data[[target]]), size = size, replace = replace, prob = weight)
      self$filter_obs(target, sampled_rows, ...)
    },

    sample_frac_obs = function(target, size, replace = FALSE, taxon_weight = NULL, obs_weight = NULL,
                                use_supertaxa = TRUE, collapse_func = mean, ...) {
      self$sample_n_obs(target = target, size = size * nrow(self$data[[target]]), replace = replace,
                   taxon_weight = taxon_weight, obs_weight = obs_weight,
                   use_supertaxa = use_supertaxa, collapse_func = collapse_func, ...)
    },

    sample_n_taxa = function(size, taxon_weight = NULL, obs_weight = NULL, obs_target = NULL,
                              use_subtaxa = TRUE, collapse_func = mean, ...) {
      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(taxon_weight, obs_weight)))
      taxon_weight <- lazyeval::lazy_eval(lazyeval::lazy(taxon_weight), data = data_used)
      obs_weight <- lazyeval::lazy_eval(lazyeval::lazy(obs_weight), data = data_used)

      # Calculate observation component of taxon weights
      if (is.null(obs_weight)) {
        taxon_obs_weight <- rep(1, nrow(self$edge_list))
      } else {
        if (is.null(obs_target)) {
          stop("If the option `obs_weight` is used, then `obs_target` must also be defined.")
        }
        my_obs <- self$obs(obs_target, recursive = use_subtaxa, simplify = FALSE)
        taxon_obs_weight <- vapply(my_obs, function(x) collapse_func(obs_weight[x]), numeric(1))
      }
      taxon_obs_weight <- taxon_obs_weight / sum(taxon_obs_weight)

      # Calculate taxon component of taxon weights
      if (is.null(taxon_weight)) {
        taxon_weight <- rep(1, nrow(self$edge_list))
      }
      taxon_weight <- taxon_weight / sum(taxon_weight)

      # Combine observation and taxon weight components
      combine_func <- prod
      weight <- mapply(taxon_weight, taxon_obs_weight, FUN = function(x, y) combine_func(c(x,y)))
      weight <- weight / sum(weight)

      # Sample observations
      sampled_rows <- sample.int(nrow(self$edge_list), size = size, replace = FALSE, prob = weight)
      self$filter_taxa(sampled_rows, ...)
    },

    sample_frac_taxa = function(size = 1, taxon_weight = NULL, obs_weight = NULL, obs_target = NULL,
                                 use_subtaxa = TRUE, collapse_func = mean, ...) {
      self$sample_n_taxa(size = size * nrow(self$edge_list),
                    taxon_weight = taxon_weight, obs_weight = obs_weight, obs_target = obs_target,
                    use_subtaxa = use_subtaxa, collapse_func = collapse_func, ...)
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
#' NOTE: This currently does nothing.
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
#' Used to print each item in the `taxmap` print method.
#'
#' @param data The item to be printed
#' @param max_rows (\code{numeric} of length 1) The maximum number of rows in tables to print.
#' @param max_items (\code{numeric} of length 1) The maximum number of list items to print.
#' @param max_width (\code{numeric} of length 1) The maximum number of characters to print.
#' @param prefix (\code{numeric} of length 1) What to print in front of each line.
#'
#' @examples
#' taxa:::print_item(ex_taxmap$data$info)
#' taxa:::print_item(1:100)
#'
#' @keywords internal
print_item <- function(data, name = NULL, max_rows = 3, max_items = 3, max_width = getOption("width") - 10, prefix = "") {
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
    prefixed_print(data, prefix = paste0(prefix, "  "), n = max_rows, width = max_width)
  } else if (is.list(data)) {
    if (length(data) < 1) {
      prefixed_print(list(), prefix = prefix)
    } else {
      cat(paste0(prefix, name, ": a list with ", length(data), ifelse(length(data) == 1, " item", " items"), "\n"))
    }
  } else if (is.vector(data)) {
    cat(paste0(prefix, name, ": "))
    limited_print(data, max_chars = max_width)
  } else {
    prefixed_print(data, prefix = prefix)
  }
  invisible(data)
}



#' Get data indexes associated with taxa
#'
#' Given a \code{\link{taxmap}} object, return the indexes
#' associated with each taxon in a given table included in that \code{\link{taxmap}} object.
#' \preformatted{
#' obj$obs(data, subset = NULL, recursive = TRUE, simplify = FALSE)
#' obs(obj, data, subset = NULL, recursive = TRUE, simplify = FALSE)}
#'
#' @param obj (\code{\link{taxmap}})
#' The \code{\link{taxmap}} object containing taxon information to be queried.
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
#'
#' @examples
#' # Get indexes of rows corresponding to each taxon
#' ex_taxmap$obs("info")
#'
#' # Get only a subset of taxon indexes
#' ex_taxmap$obs("info", subset = 1:2)
#'
#' # Get only a subset of taxon IDs
#' ex_taxmap$obs("info", subset = c("1", "2"))
#'
#' # Only return indexes of rows assinged to each taxon specifically (i.e. not subtaxa)
#' ex_taxmap$obs("info", recursive = FALSE)
#'
#' # Lump all row indexes in a single vector
#' ex_taxmap$obs("info", simplify = TRUE)
#'
NULL

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



#' Return names of data in a \code{\link{taxmap}}
#'
#' Return all the valid names that can be used with non-standard evalulation in manipulation functions like \code{filter_taxa}.
#' \preformatted{
#' obj$all_names(tables = TRUE, funcs = TRUE, others = TRUE, warn = FALSE)
#' all_names(obj, tables = TRUE, funcs = TRUE, others = TRUE, warn = FALSE)}
#'
#' @param obj (\code{\link{taxmap}})
#' The \code{\link{taxmap}} object containing taxon information to be queried.
#' @param tables If \code{TRUE}, include the names of columns of tables in \code{obj$data}
#' @param funcs If \code{TRUE}, include the names of user-definable functionsin \code{obj$funcs}.
#' @param others If \code{TRUE}, include the names of data in \code{obj$data} besides tables.
#' @param warn If \code{TRUE}, warn if there are duplicate names.
#'
#' @return \code{character}
#'
#' @examples
#' # Get the names of all data accesible by non-standard evaluation
#' ex_taxmap$all_names()
#'
#' # Dont include the names of functions
#' ex_taxmap$all_names(funcs = FALSE)
#'
#' @family accessors
#'
#' @name all_names
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



#' Get names of data used in expressions
#'
#' Get names of available data used in expressions.
#' Expressions are not evaluated and do not need to make sense.
#' \preformatted{
#' obj$names_used(...)
#' names_used(obj,...)}
#'
#' @param obj a \code{\link{taxmap}} object
#' @param ... One or more expressions
#'
#' @return Named \code{character}
#'
#' @examples
#' ex_taxmap$names_used(n_legs + dangerous == invalid_expression)
#'
#' @family accessors
#'
#' @name names_used
NULL

#' @export
names_used <- function(obj, ...) {
  UseMethod("names_used")
}

#' @export
names_used.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
names_used.Taxmap <- function(obj, ...) {
  obj$names_used(...)
}


#' Get data in a taxmap object by name
#'
#' Given a vector of names, return a list of data contained in a \code{\link{taxmap}} object.
#' \preformatted{
#' obj$get_data(name)
#' get_data(obj, name)}
#'
#' @param obj A \code{\link{taxmap}}  object
#' @param name (\code{character}) Names of data to return.
#'
#' @return \code{list}
#'
#' @examples
#' ex_taxmap$get_data("reaction")
#'
#' @family accessors
#'
#' @name get_data
NULL

#' @export
get_data <- function(obj, ...) {
  UseMethod("get_data")
}

#' @export
get_data.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
get_data.Taxmap <- function(obj, ...) {
  obj$get_data(...)
}


#' Get values of data used in expressions
#'
#' Get values of data in a \code{\link{taxmap}} used in expressions.
#' Expressions are not evaluated and do not need to make sense.
#' \preformatted{
#' obj$data_used(...)
#' data_used(obj, ...)}
#'
#' @param obj a \code{\link{taxmap}} object
#' @param ... One or more expressions
#'
#' @return \code{list}
#'
#' @examples
#' ex_taxmap$data_used(n_legs + dangerous == invalid_expression)
#'
#' @family accessors
#'
#' @name data_used
NULL

#' @export
data_used <- function(obj, ...) {
  UseMethod("data_used")
}

#' @export
data_used.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
data_used.Taxmap <- function(obj, ...) {
  obj$data_used(...)
}


#' Get values of all data
#'
#' Get values of all data in a \code{\link{taxmap}} object
#' \preformatted{
#' obj$data_used(...)
#' data_used(obj, ...)}
#'
#' @param obj a \code{\link{taxmap}} object
#' @param ... Passed to \code{\link{all_names}}
#'
#' @return Named \code{list}
#'
#' @examples
#' ex_taxmap$all_data()
#'
#' @family accessors
#'
#' @name all_data
NULL

#' @export
all_data <- function(obj, ...) {
  UseMethod("all_data")
}

#' @export
all_data.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
all_data.Taxmap <- function(obj, ...) {
  obj$all_data(...)
}


#' Filter taxa with a list of conditions
#'
#' Filter taxa in a \code{\link{taxmap}} object with a series of conditions. Any variable name that
#' appears in \code{obj$all_names()} can be used as if it was a vector on its own. See
#' \code{\link[dplyr]{filter}} for the inspiration for this function and more information.
#' Calling the function using the \code{obj$filter_taxa(...)} style edits "obj" in place, unlike most R functions.
#' However, calling the function using the \code{filter_taxa(obj, ...)} mitates R's traditional copy-on-modify semantics,
#' so "obj" would not be changed; instead a changed version would be returned, like most R functions.
#' \preformatted{
#' obj$filter_taxa(..., subtaxa = FALSE, supertaxa = FALSE, taxonless = FALSE,
#'                 reassign_obs = TRUE, reassign_taxa = TRUE, invert = FALSE)
#' filter_taxa(obj, ...)}
#'
#' @param obj An object of class \code{\link{taxmap}}
#' @param ... One or more filtering conditions. Each filtering condition can be one of three things: \describe{
#'   \item{\code{character}}{One or more taxon IDs contained in \code{obj$edge_list$to}} \item{\code{integer}}{One or more row indexes
#'   of \code{obj$edge_list}} \item{\code{logical}}{A \code{TRUE}/\code{FALSE} vector of length equal
#'   to the number of rows in \code{obj$edge_list}} } Any variable name that
#' appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#' @param subtaxa (\code{logical} of length 1) If \code{TRUE}, include subtaxa of taxa passing the
#'   filter.
#' @param supertaxa (\code{logical} of length 1) If \code{TRUE}, include supertaxa of taxa passing
#'   the filter.
#' @param taxonless (\code{logical}) If \code{TRUE}, include observations even if the
#'   taxon they are assigned to is filtered out. Observations assigned to removed taxa will be
#'   assigned to \code{NA}. This option can be either simply \code{TRUE}/\code{FALSE},
#'   meaning that all data sets will be treated the same, or a logical vector can be supplied with names
#'   corresponding one or more data sets in \code{obj$data}. For example, \code{c(abundance = TRUE, stats = FALSE)}
#'   would inlcude observations whose taxon was filtered out in \code{obj$data$abundance}, but not in \code{obj$data$stats}.
#'   See the \code{reassign_obs} option below for further complications.
#' @param reassign_obs (\code{logical} of length 1) If \code{TRUE}, observations assigned to removed
#'   taxa will be reassigned to the closest supertaxon that passed the filter. If there are no
#'   supertaxa of such an observation that passed the filter, they will be filtered out if
#'   \code{taxonless} is \code{TRUE}. This option can be either simply \code{TRUE}/\code{FALSE},
#'   meaning that all data sets will be treated the same, or a logical vector can be supplied with names
#'   corresponding one or more data sets in \code{obj$data}. For example, \code{c(abundance = TRUE, stats = FALSE)} would reassign observations
#'   in \code{obj$data$abundance}, but not in \code{obj$data$stats}.
#' @param reassign_taxa (\code{logical} of length 1) If \code{TRUE}, subtaxa of removed taxa will be
#'   reassigned to the closest supertaxon that passed the filter. This is useful for removing intermediate levels of a taxonomy.
#' @param invert (\code{logical} of length 1) If \code{TRUE}, do NOT include the selection.
#'   This is different than just replacing a \code{==} with a \code{!=} because this option negates
#'   the selection after taking into account the \code{subtaxa} and \code{supertaxa} options.
#'   This is useful for removing a taxon and all its subtaxa for example.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @examples
#' # Filter by index
#' filter_taxa(ex_taxmap, 1:3)
#'
#' # Filter by taxon ID
#' filter_taxa(ex_taxmap, c("1", "2", "3"))
#'
#' # Fiter by TRUE/FALSE
#' filter_taxa(ex_taxmap, taxon_names == "Plantae", subtaxa = TRUE)
#'
#' # Filter by an observation characteristic
#' dangerous_taxa <- sapply(ex_taxmap$obs("info"),
#'                          function(i) any(ex_taxmap$data$info$dangerous[i]))
#' filter_taxa(ex_taxmap, dangerous_taxa)
#'
#' # Include supertaxa
#' filter_taxa(ex_taxmap, 12, supertaxa = TRUE)
#'
#' # Include subtaxa
#' filter_taxa(ex_taxmap, 1, subtaxa = TRUE)
#'
#' # Remove rows in data corresponding to removed taxa
#' filter_taxa(ex_taxmap, 2, taxonless = c(info = FALSE))
#'
#' # Remove a taxon and it subtaxa
#' filter_taxa(ex_taxmap, 1, subtaxa = TRUE, invert = TRUE)
#'
#' @family dplyr-like functions
#'
#' @name filter_taxa
NULL

#' @export
filter_taxa <- function(obj, ...) {
  UseMethod("filter_taxa")
}

#' @export
filter_taxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
filter_taxa.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$filter_taxa(...)
}




#' Filter observations with a list of conditions
#'
#' Filter data in a \code{\link{taxmap}} (in \code{obj$data}) object with a series of conditions.
#' Any variable name that appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#' See \code{\link[dplyr]{filter}} for the inspiration for this function and more information.
#' Calling the function using the \code{obj$filter_obs(...)} style edits "obj" in place, unlike most R functions.
#' However, calling the function using the \code{filter_obs(obj, ...)} mitates R's traditional copy-on-modify semantics,
#' so "obj" would not be changed; instead a changed version would be returned, like most R functions.
#' \preformatted{
#' obj$filter_obs(target, ..., unobserved = TRUE)
#' filter_obs(obj, target, ...)}
#'
#' @param obj An object of type \code{\link{taxmap}}
#' @param target The name of the list/vector/table in \code{obj$data} to filter
#' @param ... One or more filtering conditions. Each filtering condition can be one of three things: \describe{
#'   \item{\code{character}}{One or more taxon IDs contained in \code{obj$edge_list$to}} \item{\code{integer}}{One or more row indexes
#'   of \code{obj$edge_list}} \item{\code{logical}}{A \code{TRUE}/\code{FALSE} vector of length equal
#'   to the number of rows in \code{obj$edge_list}} } Any variable name that
#' appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#' @param unobserved (\code{logical} of length 1) If \code{TRUE}, preserve taxa even if all of their
#'   observations are filtered out. If \code{FALSE}, remove taxa for which all observations were filtered out.
#'   Note that only taxa that are unobserved due to this filtering will be removed; there might be
#'   other taxa without observations to begin with that will not be removed.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @examples
#' # Filter by row index
#' filter_obs(ex_taxmap, "info", 1:2)
#'
#' # Filter by TRUE/FALSE
#' filter_obs(ex_taxmap, "info", dangerous == FALSE)
#'
#' # Remove taxa whose obserservation were filtered out
#' filter_obs(ex_taxmap, "info", dangerous == FALSE, unobserved = FALSE)
#'
#' @family dplyr-like functions
#'
#' @name filter_obs
NULL

#' @export
filter_obs <- function(obj, ...) {
  UseMethod("filter_obs")
}

#' @export
filter_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
filter_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$filter_obs(...)
}


get_data_taxon_ids <- function(x) {
  if (is.data.frame(x)) {
    data_taxon_ids <- x$taxon_id
  } else {
    data_taxon_ids <- names(x)
  }
  return(data_taxon_ids)
}



#' Subset columns in a \code{\link{taxmap}} object
#'
#' Subsets columns in a \code{\link{taxmap}} object. Takes and returns a
#' \code{\link{taxmap}} object. Any variable name that appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#' See \code{\link[dplyr]{select}} for the inspiration for this function and more information.
#' Calling the function using the \code{obj$select_obs(...)} style edits "obj" in place, unlike most R functions.
#' However, calling the function using the \code{select_obs(obj, ...)} mitates R's traditional copy-on-modify semantics,
#' so "obj" would not be changed; instead a changed version would be returned, like most R functions.
#' \preformatted{
#' obj$select_obs(target, ..., unobserved = TRUE)
#' select_obs(obj, target, ...)}
#'
#' @param obj An object of type \code{\link{taxmap}}
#' @param target The name of the list/vector/table in \code{obj$data} to filter
#' @param ... One or more column names to return in the new object. Each can be one of two things:
#'   \describe{ \item{expression with unquoted column name}{The name of a column in
#'   \code{obj$data[[target]]} typed as if it was a varaible on its own.} \item{\code{numeric}}{Indexes of
#'   columns in \code{obj$data[[target]]}} } To match column names with a character vector, use
#'   \code{matches("my_col_name")}. To match a logical vector, convert it to a column index using
#'   \code{\link[base]{which}}.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @family dplyr-like functions
#'
#' @examples
#' # Selecting a column by name
#' select_obs(ex_taxmap, "info", dangerous)
#'
#' # Selecting a column by index
#' select_obs(ex_taxmap, "info", 3)
#'
#' # Selecting a column by regular expressions
#' select_obs(ex_taxmap, "info", matches("^n"))
#'
#' @name select_obs
NULL

#' @export
select_obs <- function(obj, ...) {
  UseMethod("select_obs")
}

#' @export
select_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
select_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$select_obs(...)
}



#' Add columns to \code{\link{taxmap}} objects
#'
#' Add columns to tables in \code{obj$data} in \code{\link{taxmap}} objects. Any variable name that appears in
#' \code{obj$all_names()} can be used as if it was a vector on its own.
#' See \code{\link[dplyr]{mutate}} for the inspiration for this function and more information.
#' Calling the function using the \code{obj$mutate_obs(...)} style edits "obj" in place, unlike most R functions.
#' However, calling the function using the \code{mutate_obs(obj, ...)} mitates R's traditional copy-on-modify semantics,
#' so "obj" would not be changed; instead a changed version would be returned, like most R functions.
#' \preformatted{
#' obj$mutate_obs(target, ...)
#' mutate_obs(obj, target, ...)}
#'
#' @param obj An object of type \code{\link{taxmap}}
#' @param target The name of the table in \code{obj$data} to filter
#' @param ... One or more named columns to add. Newly created columns can be
#'   referenced in the same function call.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @examples
#' mutate_obs(ex_taxmap, "info",
#'            new_col = "Im new",
#'            newer_col = paste0(new_col, "er!"))
#'
#' @family dplyr-like functions
#' @name mutate_obs
NULL

#' @export
mutate_obs <- function(obj, ...) {
  UseMethod("mutate_obs")
}

#' @export
mutate_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
mutate_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$mutate_obs(...)
}


#' Replace columns in \code{\link{taxmap}} objects
#'
#' Replace columns of tables in \code{obj$data} in \code{\link{taxmap}} objects.
#' Any variable name that appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#' See \code{\link[dplyr]{transmute}} for the inspiration for this function and more information.
#' \preformatted{
#' obj$transmute_obs(target, ...)
#' transmute_obs(obj, target, ...)}
#'
#' @param obj An object of type \code{\link{taxmap}}
#' @param target The name of the table in \code{obj$data} to filter
#' @param ... One or more named columns to add. Newly created columns can be
#'   referenced in the same function call.
#'
#' @return An object of type \code{\link{taxmap}}
#' @examples
#' transmute_obs(ex_taxmap, "info", new_col = paste0(name, "!!!"))
#'
#' @family dplyr-like functions
#'
#' @name transmute_obs
NULL


#' @export
transmute_obs <- function(obj, ...) {
  UseMethod("transmute_obs")
}

#' @export
transmute_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
transmute_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$transmute_obs(...)
}



#' Sort columns of \code{\link{taxmap}} objects
#'
#' Sort columns of tables in \code{obj$data} in \code{\link{taxmap}} objects.
#' Any variable name that appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#' See \code{\link[dplyr]{arrange}} for the inspiration for this function and more information.
#' \preformatted{
#' obj$arrange_obs(target, ...)
#' arrange_obs(obj, target, ...)}
#'
#' @param obj An object of type \code{\link{taxmap}}
#' @param target The name of the table in \code{obj$data} to filter
#' @param ... One or more column names to sort on.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @examples
#' # Sort in ascending order
#' arrange_obs(ex_taxmap, "info", n_legs)
#'
#' # Sort in decending order
#' arrange_obs(ex_taxmap, "info", desc(n_legs))
#'
#' @family dplyr-like functions
#'
#' @name arrange_obs
NULL

#' @export
arrange_obs <- function(obj, ...) {
  UseMethod("arrange_obs")
}

#' @export
arrange_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
arrange_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$arrange_obs(...)
}




#' Sort the edge list of \code{\link{taxmap}} objects
#'
#' Sort the edge list in \code{\link{taxmap}} objects.
#' Any variable name that appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#' See \code{\link[dplyr]{arrange}} for the inspiration for this function and more information.
#' \preformatted{
#' obj$arrange_taxa(...)
#' arrange_taxa(obj, ...)}
#'
#' @param obj \code{\link{taxmap}}
#' @param ... One or more column names to sort on.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @examples
#' arrange_taxa(ex_taxmap, desc(ex_taxmap$taxon_names()))
#'
#' @family dplyr-like functions
#'
#' @name arrange_taxa
NULL


#' @export
arrange_taxa <- function(obj, ...) {
  UseMethod("arrange_taxa")
}

#' @export
arrange_taxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
arrange_taxa.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$arrange_taxa(...)
}



#' Sample n observations from \code{\link{taxmap}}
#'
#' Randomly sample some number of observations from a \code{\link{taxmap}} object. Weights can be
#' specified for observations or the taxa they are taxmap by.
#' Any variable name that appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#' See \link[dplyr]{sample_n} for the inspiration for this function.
#' \preformatted{
#' obj$sample_n_obs(target, size, replace = FALSE, taxon_weight = NULL, obs_weight = NULL,
#' use_supertaxa = TRUE, collapse_func = mean, ...)
#' sample_n_obs(obj, target, size, replace = FALSE, taxon_weight = NULL, obs_weight = NULL,
#' use_supertaxa = TRUE, collapse_func = mean, ...)}
#'
#' @param obj (\code{\link{taxmap}}) The object to sample from.
#' @param target The name of the table in \code{obj$data} to filter
#' @param size (\code{numeric} of length 1) The number of observations to sample.
#' @param replace (\code{logical} of length 1) If \code{TRUE}, sample with replacement.
#' @param taxon_weight (\code{numeric}) Non-negative sampling weights of each taxon. If
#'   \code{use_supertaxa} is \code{TRUE}, the weights for each taxon in an observation's classification are
#'   supplied to \code{collapse_func} to get the observation weight. If
#'   \code{obs_weight} is also specified, the two weights are multiplied (after \code{taxon_weight}
#'   for each observation is calculated).
#' @param obs_weight (\code{numeric}) Sampling weights of each observation.  If
#'   \code{taxon_weight} is also specified, the two weights are multiplied (after
#'   \code{taxon_weight} for each observation is calculated).
#' @param use_supertaxa (\code{logical} of length 1) Affects how the \code{taxon_weight} is used. If
#'   \code{TRUE}, the weights for each taxon in an observation's classification are multiplied to get the
#'   observation weight. Otherwise, just the taxonomic level the observation is assign to it considered.
#' @param collapse_func (\code{function} of length 1) If \code{taxon_weight} option is used and
#'   \code{supertaxa} is \code{TRUE}, the weights for each taxon in an observation's classification are
#'   supplied to \code{collapse_func} to get the observation weight. This function should take  numeric
#'   vector and return a single number.
#' @param ... Additional options are passed to \code{\link{filter_obs}}.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @examples
#' # Sample 2 rows without replacement
#' sample_n_obs(ex_taxmap, "info", 2)
#'
#' # Sample with replacement
#' sample_n_obs(ex_taxmap, "info", 10, replace = TRUE)
#'
#' # Sample some rows for often then others
#' sample_n_obs(ex_taxmap, "info", 3, obs_weight = n_legs)
#'
#' @family dplyr-like functions
#' @name sample_n_obs
NULL

#' @export
sample_n_obs <- function(obj, ...) {
  UseMethod("sample_n_obs")
}

#' @export
sample_n_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
sample_n_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$sample_n_obs(...)
}


#' Sample a proportion of observations from \code{\link{taxmap}}
#'
#' Randomly sample some propoortion of observations from a \code{\link{taxmap}} object. Weights can be
#' specified for observations or their taxa.
#' See \link[dplyr]{sample_frac} for the inspiration for this function.
#' \preformatted{
#' obj$sample_frac_obs(target, size, replace = FALSE, taxon_weight = NULL, obs_weight = NULL,
#' use_supertaxa = TRUE, collapse_func = mean, ...)
#' sample_frac_obs(obj, target, size, replace = FALSE, taxon_weight = NULL, obs_weight = NULL,
#' use_supertaxa = TRUE, collapse_func = mean, ...)}
#'
#' @param obj (\code{\link{taxmap}}) The object to sample from.
#' @param target The name of the table in \code{obj$data} to filter
#' @param size (\code{numeric} of length 1) The proportion of observations to sample.
#' @param replace (\code{logical} of length 1) If \code{TRUE}, sample with replacement.
#' @param taxon_weight (\code{numeric}) Non-negative sampling weights of each taxon. If
#'   \code{use_supertaxa} is \code{TRUE}, the weights for each taxon in an observation's classification are
#'   supplied to \code{collapse_func} to get the observation weight. If
#'   \code{obs_weight} is also specified, the two weights are multiplied (after \code{taxon_weight}
#'   for each observation is calculated).
#' @param obs_weight (\code{numeric}) Sampling weights of each observation.  If
#'   \code{taxon_weight} is also specified, the two weights are multiplied (after
#'   \code{taxon_weight} for each observation is calculated).
#' @param use_supertaxa (\code{logical} of length 1) Affects how the \code{taxon_weight} is used. If
#'   \code{TRUE}, the weights for each taxon in an observation's classification are multiplied to get the
#'   observation weight. Otherwise, just the taxonomic level the observation is assign to it considered.
#' @param collapse_func (\code{function} of length 1) If \code{taxon_weight} option is used and
#'   \code{supertaxa} is \code{TRUE}, the weights for each taxon in an observation's classification are
#'   supplied to \code{collapse_func} to get the observation weight. This function should take  numeric
#'   vector and return a single number.
#' @param ... Additional options are passed to \code{\link{filter_obs}}.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @examples
#' sample_frac_obs(ex_taxmap, "info", 0.5)
#'
#' @family dplyr-like functions
#' @name sample_frac_obs
NULL

#' @export
sample_frac_obs <- function(obj, ...) {
  UseMethod("sample_frac_obs")
}

#' @export
sample_frac_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
sample_frac_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$sample_frac_obs(...)
}


#' Sample n taxa from \code{\link{taxmap}}
#'
#' Randomly sample some number of taxa from a \code{\link{taxmap}} object. Weights can be
#' specified for taxa or the observations assigned to them.
#' See \link[dplyr]{sample_n} for the inspiration for this function.
#' \preformatted{
#' obj$sample_n_taxa(size, taxon_weight = NULL, obs_weight = NULL, obs_target = NULL,
#' use_subtaxa = TRUE, collapse_func = mean, ...)
#' sample_n_taxa(obj, size, taxon_weight = NULL, obs_weight = NULL, obs_target = NULL,
#' use_subtaxa = TRUE, collapse_func = mean, ...)}
#'
#' @param obj (\code{\link{taxmap}}) The object to sample from.
#' @param size (\code{numeric} of length 1) The number of taxa to sample.
#' @param taxon_weight (\code{numeric}) Non-negative sampling weights of each taxon.
#'   If \code{obs_weight} is also specified, the two weights are multiplied (after
#'   \code{obs_weight} for each taxon is calculated).
#' @param obs_weight (\code{numeric}) Sampling weights of each observation. The weights for each observation
#'   assigned to a given taxon are supplied to \code{collapse_func} to get the taxon weight. If
#'   \code{use_subtaxa} is \code{TRUE} then the observations assigned to every subtaxa are also used. Any variable name that
#' appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#'  If \code{taxon_weight} is also specified, the two weights are multiplied (after
#'   \code{obs_weight} for each observation is calculated). \code{obs_target} must be used with this option.
#' @param obs_target (\code{character} of length 1) The name of the data set in \code{obj$data} that values in
#' \code{obs_weight} corresponds to. Must be used when \code{obs_weight} is used.
#' @param use_subtaxa (\code{logical} of length 1) Affects how the \code{obs_weight} option is
#'   used. If \code{TRUE}, the weights for each taxon in an observation's classification are multiplied to
#'   get the observation weight. Otherwise, just the taxonomic level the observation is assign to it considered.
#' @param collapse_func (\code{function} of length 1) If \code{taxon_weight} is used and
#'   \code{supertaxa} is \code{TRUE}, the weights for each taxon in an observation's classification are
#'   supplied to \code{collapse_func} to get the observation weight. This function should take  numeric
#'   vector and return a single number.
#' @param ... Additional options are passed to \code{\link{filter_taxa}}.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#' @examples
#' # Randomly sample three taxa
#' sample_n_taxa(ex_taxmap, 3)
#'
#' # Include supertaxa
#' sample_n_taxa(ex_taxmap, 3, supertaxa = TRUE)
#'
#' # Include subtaxa
#' sample_n_taxa(ex_taxmap, 1, subtaxa = TRUE)
#'
#' # Sample some taxa more often then others
#' sample_n_taxa(ex_taxmap, 3, supertaxa = TRUE, obs_weight = n_legs, obs_target = "info")
#'
#' @family dplyr-like functions
#'
#' @name sample_n_taxa
NULL

#' @export
sample_n_taxa <- function(obj, ...) {
  UseMethod("sample_n_taxa")
}

#' @export
sample_n_taxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
sample_n_taxa.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$sample_n_taxa(...)
}



#' Sample a proportion of taxa from \code{\link{taxmap}}
#'
#' Randomly sample some proportion of taxa from a \code{\link{taxmap}} object. Weights can be
#' specified for taxa or the observations assigned to them. See \link[dplyr]{sample_frac} for the
#' inspiration for this function.
#' \preformatted{
#' obj$sample_frac_taxa(size, taxon_weight = NULL, obs_weight = NULL, obs_target = NULL,
#' use_subtaxa = TRUE, collapse_func = mean, ...)
#' sample_frac_taxa(obj, size, taxon_weight = NULL, obs_weight = NULL, obs_target = NULL,
#' use_subtaxa = TRUE, collapse_func = mean, ...)}
#'
#' @param obj (\code{\link{taxmap}}) The object to sample from.
#' @param size (\code{numeric} of length 1) The proportion of taxa to sample.
#' @param taxon_weight (\code{numeric}) Non-negative sampling weights of each taxon.
#'   If \code{obs_weight} is also specified, the two weights are multiplied (after
#'   \code{obs_weight} for each taxon is calculated).
#' @param obs_weight (\code{numeric}) Sampling weights of each observation. The weights for each observation
#'   assigned to a given taxon are supplied to \code{collapse_func} to get the taxon weight. If
#'   \code{use_subtaxa} is \code{TRUE} then the observations assigned to every subtaxa are also used. Any variable name that
#' appears in \code{obj$all_names()} can be used as if it was a vector on its own.
#' If \code{taxon_weight} is also specified, the two weights are multiplied (after
#'   \code{obs_weight} for each observation is calculated). \code{obs_target} must be used with this option.
#' @param obs_target (\code{character} of length 1) The name of the data set in \code{obj$data} that values in
#' \code{obs_weight} corresponds to. Must be used when \code{obs_weight} is used.
#' @param use_subtaxa (\code{logical} of length 1) Affects how the \code{obs_weight} option is
#'   used. If \code{TRUE}, the weights for each taxon in an observation's classification are multiplied to
#'   get the observation weight. Otherwise, just the taxonomic level the observation is assign to it considered.
#' @param collapse_func (\code{function} of length 1) If \code{taxon_weight} is used and
#'   \code{supertaxa} is \code{TRUE}, the weights for each taxon in an observation's classification are
#'   supplied to \code{collapse_func} to get the observation weight. This function should take  numeric
#'   vector and return a single number.
#' @param ... Additional options are passed to \code{\link{filter_taxa}}.
#'
#' @return An object of type \code{\link{taxmap}}
#'
#'
#' @examples
#' sample_frac_taxa(ex_taxmap, 0.5, supertaxa = TRUE)
#'
#' @family dplyr-like functions
#'
#' @name sample_frac_taxa
NULL

#' @export
sample_frac_taxa <- function(obj, ...) {
  UseMethod("sample_frac_taxa")
}

#' @export
sample_frac_taxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
sample_frac_taxa.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE) # Makes this style of executing the function imitate traditional copy-on-change
  obj$sample_frac_taxa(...)
}
