#' Taxmap class
#'
#' @export
#' @param ... Any number of object of class [hierarchy()] or character vectors.
#' @param data A list of tables with data associated with the taxa.
#' @return An `R6Class` object of class [taxmap()]
#'
#' @details on initialize, function sorts the taxon list based on rank (if rank
#'   information is available), see [ranks_ref] for the reference rank names and
#'   orders
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
      self$data <- validate_taxmap_data(data, self$input_ids)

      # Make sure `funcs` is in the right format and add to object
      self$funcs <- validate_taxmap_funcs(funcs)
    },

    print = function(indent = "", max_rows = 3, max_items = 3,
                     max_width = getOption("width") - 10) {

      # Call `taxonomy` print method
      taxonomy_output <- paste0(
        paste0(capture.output(super$print(indent = indent)), collapse = "\n"),
        "\n"
      )
      cat(gsub(taxonomy_output, pattern = "Taxonomy", replacement = "Taxmap"))

      # Print a subset of each item, up to a max number, then just print names
      cat(paste0("  ", length(self$data), " data sets:\n"))
      if (length(self$data) > 0) {
        for (i in 1:min(c(max_items, length(self$data)))) {
          print_item(self$data[[i]],
                     name = names(self$data[i]), max_rows = max_rows,
                     max_width = max_width, prefix = "    ")
        }
        if (length(self$data) > max_items) {
          cat(paste0("    And ", length(self$data) - max_items,
                     " more data sets:"))
          limited_print(names(self$data)[(max_items + 1):length(self$data)])
        }
      }

      # Print the names of functions
      cat(paste0("  ", length(self$funcs), " functions:\n"))
      limited_print(names(self$funcs))

      invisible(self)
    },

    # Returns the names of things to be accessible using non-standard evaluation
    all_names = function(tables = TRUE, funcs = TRUE, others = TRUE,
                         builtin_funcs = TRUE, warn = FALSE) {
      output <- c()

      # Add functions included in the package
      if (builtin_funcs) {
        output <- c(output, private$nse_accessible_funcs)
      }

      # Get column names in each table, removing 'taxon_id'
      is_table <- vapply(self$data, is.data.frame, logical(1))
      if (tables && length(self$data[is_table]) > 0) {
        table_col_names <- unlist(lapply(self$data[is_table], colnames))
        names(table_col_names) <- paste0("data$",
                                         rep(names(self$data[is_table]),
                                             vapply(self$data[is_table],
                                                    ncol, integer(1))))
        table_col_names <- table_col_names[table_col_names != "taxon_id"]
        output <- c(output, table_col_names)
      }

      # Get other object names in data
      is_other <- !is_table
      if (others && length(self$data[is_other]) > 0) {
        other_names <- names(self$data[is_other])
        names(other_names) <- rep("data", length(other_names))
        output <- c(output, other_names)
      }


      # Get function names
      if (funcs && length(self$funcs) > 0) {
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
      names(output) <- paste0(names(output),
                              ifelse(names(output) == "", "", "$"), output)
      return(output)
    },


    obs = function(data, value = NULL, subset = NULL, recursive = TRUE, simplify = FALSE) {
      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(subset)))
      subset <- lazyeval::lazy_eval(lazyeval::lazy(subset), data = data_used)
      subset <- private$parse_nse_taxon_subset(subset)
      obs_taxon_ids <- private$get_data_taxon_ids(data, require = TRUE)

      # Get observations of taxa
      if (is.logical(recursive) && recursive == FALSE) {
        recursive = 0
      }
      if (recursive || is.numeric(recursive)) {
        my_subtaxa <- self$subtaxa(subset = unname(subset),
                                   recursive = recursive,
                                   include_input = TRUE,
                                   value = "taxon_indexes")
        #unname is neede for some reason.. something to look into...
      } else {
        my_subtaxa <- subset
      }
      obs_taxon_index <- match(obs_taxon_ids, self$taxon_ids())
      obs_key <- split(seq_along(obs_taxon_ids), obs_taxon_index)
      output <- stats::setNames(
        lapply(my_subtaxa,function(x) unname(unlist(obs_key[as.character(x)]))),
        names(subset)
      )
      is_null <- vapply(output, is.null, logical(1))
      output[is_null] <- lapply(1:sum(is_null), function(x) numeric(0))

      # Look up values
      if (!is.null(value)) {
        possible_values <- self$get_data(value)[[1]]
        if (is.null(names(possible_values))) {
          output <- lapply(output, function(i) possible_values[i])
        } else {
          output <- lapply(output, function(i) possible_values[private$get_data_taxon_ids(data)[i]])
        }

      }

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    },

    obs_apply = function(data, func, simplify = FALSE, value = NULL,
                         subset = NULL, recursive = TRUE, ...) {
      my_obs <- self$obs(data, simplify = FALSE, value = value, subset = subset,
                         recursive = recursive)
      output <- lapply(my_obs, func, ...)
      if (simplify) {
        output <- unlist(output)
      }
      return(output)
    },


    filter_obs = function(target, ..., unobserved = TRUE) {
      # Check that the target data exists
      private$check_dataset_name(target)

      # non-standard argument evaluation
      selection <- lazyeval::lazy_eval(lazyeval::lazy_dots(...),
                                       data = self$data_used(...))

      # If no selection is supplied, match all rows
      if (length(selection) == 0) {
        selection <- list(seq_len(nrow(self$data[[target]])))
      }

      # convert taxon_ids to indexes
      is_char <- vapply(selection, is.character, logical(1))
      if (sum(is_char) > 0) {
        stop(paste("observation filtering with taxon IDs is not currently",
                   "supported. If you want to filter observation by taxon IDs,",
                   "use something like: `obj$data$my_target$taxon_ids %in%",
                   "my_subset`"))
      }

      # convert logical to indexes
      is_logical <- vapply(selection, is.logical, logical(1))
      selection[is_logical] <- lapply(selection[is_logical], which)

      # combine filters
      intersect_with_dups <-function(a, b) {
        rep(sort(intersect(a, b)), pmin(table(a[a %in% b]), table(b[b %in% a])))
      }
      selection <- Reduce(intersect_with_dups, selection)

      # Remove observations
      data_taxon_ids <- private$get_data_taxon_ids(target)
      private$remove_obs(dataset = target, indexes = selection)

      # Remove unobserved taxa
      if (! unobserved & ! is.null(data_taxon_ids)) {
        unobserved_taxa <- self$supertaxa(unique(data_taxon_ids[-selection]),
                                          na = FALSE, recursive = TRUE,
                                          simplify = TRUE, include_input = TRUE,
                                          value = "taxon_indexes")
        taxa_to_remove <- 1:nrow(self$edge_list) %in%
          unobserved_taxa & vapply(self$obs(target), length, numeric(1)) == 0
        self$taxa <- self$taxa[self$taxon_ids()[! taxa_to_remove]]
        self$edge_list <- self$edge_list[! taxa_to_remove, , drop = FALSE]
        self$edge_list[! self$edge_list$from %in% self$taxon_ids(), "from"] <-
          as.character(NA)
      }

      return(self)
    },


    select_obs = function(target, ...) {
      # Check that the target data exists
      private$check_dataset_name(target)

      # Check that the target is a table
      if (! is.data.frame(self$data[[target]])) {
        stop(paste0('The dataset "', target, '" is not a table, so columns cannot be selected.'))
      }

      self$data[[target]] <-
        dplyr::bind_cols(self$data[[target]][ , c("taxon_id"), drop = FALSE],
                         dplyr::select(self$data[[target]], ...))
      return(self)
    },


    mutate_obs = function(target, ...) {
      # Check that the target data exists
      private$check_dataset_name(target)

      # Check that the target is a table
      if (! is.data.frame(self$data[[target]])) {
        stop(paste0('The dataset "', target, '" is not a table, so columns cannot be selected.'))
      }

      data_used <- self$data_used(...)
      unevaluated <- lazyeval::lazy_dots(...)
      for (index in seq_along(unevaluated)) {
        new_col <- lazyeval::lazy_eval(unevaluated[index], data = data_used)
        # Allow this col to be used in evaluating the next cols
        data_used <- c(data_used, new_col)
        self$data[[target]][[names(new_col)]] <- new_col[[1]]
      }
      return(self)
    },


    transmute_obs = function(target, ...) {
      # Check that the target data exists
      private$check_dataset_name(target)

      # Check that the target is a table
      if (! is.data.frame(self$data[[target]])) {
        stop(paste0('The dataset "', target, '" is not a table, so columns cannot be selected.'))
      }

      if ("taxon_id" %in% colnames(self$data[[target]])) {
        result <- list(taxon_id = self$data[[target]]$taxon_id)
      } else {
        result <- list()
      }
      data_used <- self$data_used(...)
      unevaluated <- lazyeval::lazy_dots(...)
      for (index in seq_along(unevaluated)) {
        new_col <- lazyeval::lazy_eval(unevaluated[index], data = data_used)
        # Allow this col to be used in evaluating the next cols
        data_used <- c(data_used, new_col)
        result[[names(new_col)]] <- new_col[[1]]
      }
      self$data[[target]] <- tibble::as_tibble(result)
      return(self)
    },


    arrange_obs = function(target, ...) {
      # Check that the target data exists
      private$check_dataset_name(target)

      # Sort observations
      data_used <- self$data_used(...)
      data_used <- data_used[! names(data_used) %in% names(self$data[[target]])]
      if (is.data.frame(self$data[[target]])) { # if it is a table
        if (length(data_used) == 0) {
          self$data[[target]] <- dplyr::arrange(self$data[[target]], ...)
        } else {
          target_with_extra_cols <-
            dplyr::bind_cols(data_used, self$data[[target]])
          self$data[[target]] <-
            dplyr::arrange(target_with_extra_cols, ...)[, -seq_along(data_used)]
        }
      } else { # if it is a list or vector
        dummy_table <- data.frame(index = seq_along(self$data[[target]]))
        if (length(data_used)!= 0) {
          dummy_table <- dplyr::bind_cols(data_used, dummy_table)
        }
        dummy_table <- dplyr::arrange(dummy_table, ...)
        self$data[[target]] <- self$data[[target]][dummy_table$index]
      }
      return(self)
    },


    sample_n_obs = function(target, size, replace = FALSE, taxon_weight = NULL,
                            obs_weight = NULL, use_supertaxa = TRUE,
                            collapse_func = mean, ...) {
      # Check that the target data exists
      private$check_dataset_name(target)

      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(taxon_weight, obs_weight)))
      taxon_weight <- lazyeval::lazy_eval(lazyeval::lazy(taxon_weight),
                                          data = data_used)
      obs_weight <- lazyeval::lazy_eval(lazyeval::lazy(obs_weight),
                                        data = data_used)

      # Get length of target
      if (is.data.frame(self$data[[target]])) {
        target_length <- nrow(self$data[[target]])
      } else {
        target_length <- length(self$data[[target]])
      }

      # Calculate taxon component of taxon weights
      if (is.null(taxon_weight)) {
        obs_taxon_weight <- rep(1, target_length)
      } else {
        obs_index <- match(private$get_data_taxon_ids(target),
                           self$taxon_ids())
        my_supertaxa <- self$supertaxa(recursive = use_supertaxa,
                                       simplify = FALSE, include_input = TRUE,
                                       na = FALSE,
                                       value = "taxon_indexes")
        taxon_weight_product <- vapply(
          my_supertaxa,
          function(x) collapse_func(taxon_weight[x]),
          numeric(1)
        )
        obs_taxon_weight <- taxon_weight_product[obs_index]
      }
      obs_taxon_weight <- obs_taxon_weight / sum(obs_taxon_weight)

      # Calculate observation component of observation weights
      if (is.null(obs_weight)) {
        obs_weight <- rep(1, target_length)
      }
      obs_weight <- obs_weight / sum(obs_weight)

      # Combine observation and taxon weight components
      combine_func <- prod
      weight <- mapply(obs_taxon_weight, obs_weight,
                       FUN = function(x, y) combine_func(c(x,y)))
      weight <- weight / sum(weight)

      # Sample observations
      sampled_rows <- sample.int(target_length, size = size,
                                 replace = replace, prob = weight)
      self$filter_obs(target, sampled_rows, ...)
    },

    sample_frac_obs = function(target, size, replace = FALSE,
                               taxon_weight = NULL, obs_weight = NULL,
                               use_supertaxa = TRUE,
                               collapse_func = mean, ...) {
      # Get length of target
      if (is.data.frame(self$data[[target]])) {
        target_length <- nrow(self$data[[target]])
      } else {
        target_length <- length(self$data[[target]])
      }


      self$sample_n_obs(target = target,
                        size = size * target_length,
                        replace = replace,
                        taxon_weight = taxon_weight, obs_weight = obs_weight,
                        use_supertaxa = use_supertaxa,
                        collapse_func = collapse_func, ...)
    },


    n_obs = function(target) {
      vapply(self$obs(target, recursive = TRUE, simplify = FALSE),
             length, numeric(1))
    },

    n_obs_1 = function(target) {
      vapply(self$obs(target, recursive = FALSE, simplify = FALSE),
             length, numeric(1))
    }

  ),

  private = list(
    nse_accessible_funcs = c("taxon_names", "taxon_ids", "taxon_indexes",
                             "n_supertaxa", "n_subtaxa", "n_subtaxa_1",
                             "taxon_ranks", "is_root", "is_stem", "is_branch",
                             "is_leaf"),

    check_dataset_name = function(target) {
      if (! target %in% names(self$data)) {
        stop(paste0("The target `", target, "` is not the name of a data set.",
                    " Valid targets include: ",
                    paste0(names(self$data), collapse = ", ")))
      }
    },

    # Remove observations from a particular dataset or just remove the taxon ids
    remove_obs = function(dataset, indexes, unname_only = FALSE) {
      if (unname_only) {
        if (is.data.frame(self$data[[dataset]])) {
          self$data[[dataset]][! indexes, "taxon_id"] <- as.character(NA)
        } else {
          names(self$data[[dataset]])[! indexes] <- as.character(NA)
        }
      } else {
        if (is.data.frame(self$data[[dataset]])) {
          self$data[[dataset]] <-
            self$data[[dataset]][indexes, , drop = FALSE]
        } else {
          self$data[[dataset]] <- self$data[[dataset]][indexes]
        }
      }
    },

    # Find taxon ids for datasets by dataset name
    #
    # require: if TRUE, require that taxon ids be present, or make an error
    get_data_taxon_ids = function(dataset_name, require = FALSE) {
      # Get the dataset
      if (length(dataset_name) == 1 && # data is name/index of dataset in object
          (dataset_name %in% names(self$data) || is.integer(dataset_name))) {
        dataset <- self$data[[dataset_name]]
      } else { # it is an external data set, not in the object
        dataset <- dataset_name
        dataset_name <- deparse(substitute(dataset_name))
      }

      # Extract taxon ids if they exist
      if (is.data.frame(dataset)) {
        if ("taxon_id" %in% colnames(dataset)) {
          return(dataset$taxon_id)
        } else if (require) {
          stop(paste0('There is no "taxon_id" column in the data set "',
                      dataset_name, '", so taxon ids cannot be extracted.'))
        }
      } else if (is.list(dataset) || is.vector(dataset)) {
        if (! is.null(names(dataset))) {
          return(names(dataset))
        } else if (require) {
          stop(paste0('The data set "', dataset_name,
                      '" is a list/vector, but not named, ',
                      'so taxon ids cannot be extracted.'))
        }
      } else if (require) {
        stop(paste0('I dont know how to extract taxon ids from "', dataset_name,
                    '" of type "', class(dataset)[1], '".'))
      }

      # Return NULL if taxon ids cannot be found
      return(NULL)
    }

  )
)

