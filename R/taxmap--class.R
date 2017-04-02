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
      if (tables) {
        table_col_names <- unlist(lapply(self$data[is_table], colnames))
        names(table_col_names) <- paste0("data$",
                                         rep(names(self$data[is_table]),
                                             vapply(self$data[is_table],
                                                    ncol, integer(1))))
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
      names(output) <- paste0(names(output),
                              ifelse(names(output) == "", "", "$"), output)
      return(output)
    },

    # Looks for names of data in a expression for use with NSE
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
        names_used <- unlist(lapply(1:length(expressions),
                                    function(i) decompose(expressions[[i]])))
        my_names <- self$all_names()
        return(my_names[my_names %in% names_used])
      }
    },

    # Get data by name
    get_data = function(name) {
      my_names <- self$all_names()
      if (any(unknown <- !name %in% my_names)) {
        stop(paste0("Cannot find the following data: ",
                    paste0(name[unknown], collapse = ", "), "\n ",
                    "Valid choices include: ",
                    paste0(my_names, collapse = ", "), "\n "))
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
      if (length(data) == 1 && # data is name/index of dataset in object
          (data %in% names(self$data) || is.integer(data))) {
        obs_taxon_ids <- extract_taxon_ids(self$data[[data]])
      } else {
        obs_taxon_ids <- extract_taxon_ids(data)
      }

      # Get observations of taxa
      if (recursive) {
        my_subtaxa <- self$subtaxa(subset = unname(subset), recursive = TRUE,
                                   include_input = TRUE, return_type = "index")
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

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    },

    filter_taxa = function(..., subtaxa = FALSE, supertaxa = FALSE,
                           taxonless = FALSE, reassign_obs = TRUE,
                           reassign_taxa = TRUE, invert = FALSE) {

      # non-standard argument evaluation
      selection <- lazyeval::lazy_eval(lazyeval::lazy_dots(...),
                                       data = self$data_used(...))

      # convert taxon_ids to logical
      is_char <- vapply(selection, is.character, logical(1))
      selection[is_char] <- lapply(selection[is_char],
                                   function(x) self$taxon_ids() %in% x)

      # convert indexes to logical
      is_index <- vapply(selection, is.numeric, logical(1))
      selection[is_index] <- lapply(selection[is_index],
                                    function(x) 1:nrow(self$edge_list) %in% x)

      # combine filters
      selection <- Reduce(`&`, selection)

      # default to all taxa if no selection is provided
      if (is.null(selection)) {
        selection <- rep(TRUE, length(self$taxon_ids()))
      }

      # Get taxa of subset
      taxa_subset <- unique(c(which(selection),
                              if (subtaxa) {
                                self$subtaxa(subset = selection,
                                             recursive = TRUE,
                                             return_type = "index",
                                             include_input = FALSE,
                                             simplify = TRUE)
                              },
                              if (supertaxa) {
                                self$supertaxa(subset = selection,
                                               recursive = TRUE,
                                               return_type = "index",
                                               na = FALSE, simplify = TRUE,
                                               include_input = FALSE)
                              }))

      # Invert selection
      if (invert) {
        taxa_subset <- (1:nrow(self$edge_list))[-taxa_subset]
      }

      # Reassign taxonless observations
      reassign_obs <- parse_possibly_named_logical(
        reassign_obs,
        self$data,
        default = formals(self$filter_taxa)$reassign_obs
      )
      process_one <- function(data_index) {

        reassign_one <- function(parents) {
          included_parents <- parents[parents %in% taxa_subset]
          return(self$taxon_ids()[included_parents[1]])
        }

        # Get the taxon ids of the current object
        if (is.null((data_taxon_ids <-
                     get_data_taxon_ids(self$data[[data_index]])))) {
          return(NULL) # if there is no taxon id info, dont change anything
        }

        # Generate replacement taxon ids
        to_reassign <- ! data_taxon_ids %in% self$taxon_ids()[taxa_subset]
        supertaxa_key <- self$supertaxa(
          subset = unique(data_taxon_ids[to_reassign]),
          recursive = TRUE, simplify = FALSE, include_input = FALSE,
          return_type = "index", na = FALSE
        )
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
        supertaxa_key <- self$supertaxa(
          subset = unique(self$taxon_ids()[to_reassign]),
          recursive = TRUE, simplify = FALSE, include_input = FALSE,
          return_type = "index", na = FALSE)
        reassign_key <- vapply(supertaxa_key, reassign_one, character(1)
        )
        self$edge_list[to_reassign, "from"] <-
          reassign_key[self$taxon_ids()[to_reassign]]
      }


      # Remove taxonless observations
      taxonless <- parse_possibly_named_logical(
        taxonless,
        self$data,
        default = formals(self$filter_taxa)$taxonless
      )
      process_one <- function(my_index) {

        # Get the taxon ids of the current object
        if (is.null((data_taxon_ids <-
                     get_data_taxon_ids(self$data[[my_index]])))) {
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
            self$data[[my_index]] <-
              self$data[[my_index]][obs_subset, , drop = FALSE]
          } else {
            self$data[[my_index]] <- self$data[[my_index]][obs_subset]
          }
        }

      }
      unused_output <- lapply(seq_along(self$data), process_one)



      # Remove filtered taxa
      self$taxa <- self$taxa[self$taxon_ids()[taxa_subset]]
      self$edge_list <- self$edge_list[taxa_subset, , drop = FALSE]
      self$edge_list[! self$edge_list$from %in% self$taxon_ids(), "from"] <-
        as.character(NA)

      return(self)
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
      data_taxon_ids <- get_data_taxon_ids(self$data[[target]])
      self$data[[target]] <- self$data[[target]][selection, , drop = FALSE]

      # Remove unobserved taxa
      if (! unobserved & ! is.null(data_taxon_ids)) {
        unobserved_taxa <- self$supertaxa(unique(data_taxon_ids[-selection]),
                                          na = FALSE, recursive = TRUE,
                                          simplify = TRUE, include_input = TRUE,
                                          return_type = "index")
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

      self$data[[target]] <-
        dplyr::bind_cols(self$data[[target]][ , c("taxon_id"), drop = FALSE],
                         dplyr::select(self$data[[target]], ...))
      return(self)
    },


    mutate_obs = function(target, ...) {
      # Check that the target data exists
      private$check_dataset_name(target)

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

      data_used <- self$data_used(...)
      data_used <- data_used[! names(data_used) %in% names(self$data[[target]])]
      if (length(data_used) == 0) {
        self$data[[target]] <- dplyr::arrange(self$data[[target]], ...)
      } else {
        target_with_extra_cols <-
          dplyr::bind_cols(data_used, self$data[[target]])
        self$data[[target]] <-
          dplyr::arrange(target_with_extra_cols, ...)[, -seq_along(data_used)]
      }

      return(self)
    },

    arrange_taxa = function(...) {
      data_used <- self$data_used(...)
      data_used <- data_used[! names(data_used) %in% names(self$edge_list)]
      if (length(data_used) == 0) {
        self$edge_list <- dplyr::arrange(self$edge_list, ...)
      } else {
        target_with_extra_cols <- dplyr::bind_cols(data_used, self$edge_list)
        self$edge_list <-
          dplyr::arrange(target_with_extra_cols, ...)[, -seq_along(data_used)]
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

      # Calculate taxon component of taxon weights
      if (is.null(taxon_weight)) {
        obs_taxon_weight <- rep(1, nrow(self$data[[target]]))
      } else {
        obs_index <- match(get_data_taxon_ids(self$data[[target]]),
                           self$taxon_ids())
        my_supertaxa <- self$supertaxa(recursive = use_supertaxa,
                                       simplify = FALSE, include_input = TRUE,
                                       return_type = "index", na = FALSE)
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
        obs_weight <- rep(1, nrow(self$data[[target]]))
      }
      obs_weight <- obs_weight / sum(obs_weight)

      # Combine observation and taxon weight components
      combine_func <- prod
      weight <- mapply(obs_taxon_weight, obs_weight,
                       FUN = function(x, y) combine_func(c(x,y)))
      weight <- weight / sum(weight)

      # Sample observations
      sampled_rows <- sample.int(nrow(self$data[[target]]), size = size,
                                 replace = replace, prob = weight)
      self$filter_obs(target, sampled_rows, ...)
    },

    sample_frac_obs = function(target, size, replace = FALSE,
                               taxon_weight = NULL, obs_weight = NULL,
                               use_supertaxa = TRUE,
                               collapse_func = mean, ...) {
      self$sample_n_obs(target = target,
                        size = size * nrow(self$data[[target]]),
                        replace = replace,
                        taxon_weight = taxon_weight, obs_weight = obs_weight,
                        use_supertaxa = use_supertaxa,
                        collapse_func = collapse_func, ...)
    },

    sample_n_taxa = function(size, taxon_weight = NULL, obs_weight = NULL,
                             obs_target = NULL, use_subtaxa = TRUE,
                             collapse_func = mean, ...) {
      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(taxon_weight, obs_weight)))
      taxon_weight <- lazyeval::lazy_eval(lazyeval::lazy(taxon_weight),
                                          data = data_used)
      obs_weight <- lazyeval::lazy_eval(lazyeval::lazy(obs_weight),
                                        data = data_used)

      # Calculate observation component of taxon weights
      if (is.null(obs_weight)) {
        taxon_obs_weight <- rep(1, nrow(self$edge_list))
      } else {
        if (is.null(obs_target)) {
          stop(paste("If the option `obs_weight` is used, then `obs_target`",
                     "must also be defined."))
        }
        my_obs <- self$obs(obs_target, recursive = use_subtaxa,
                           simplify = FALSE)
        taxon_obs_weight <- vapply(my_obs,
                                   function(x) collapse_func(obs_weight[x]),
                                   numeric(1))
      }
      taxon_obs_weight <- taxon_obs_weight / sum(taxon_obs_weight)

      # Calculate taxon component of taxon weights
      if (is.null(taxon_weight)) {
        taxon_weight <- rep(1, nrow(self$edge_list))
      }
      taxon_weight <- taxon_weight / sum(taxon_weight)

      # Combine observation and taxon weight components
      combine_func <- prod
      weight <- mapply(taxon_weight, taxon_obs_weight,
                       FUN = function(x, y) combine_func(c(x,y)))
      weight <- weight / sum(weight)

      # Sample observations
      sampled_rows <- sample.int(nrow(self$edge_list), size = size,
                                 replace = FALSE, prob = weight)
      self$filter_taxa(sampled_rows, ...)
    },

    sample_frac_taxa = function(size = 1, taxon_weight = NULL,
                                obs_weight = NULL, obs_target = NULL,
                                use_subtaxa = TRUE, collapse_func = mean, ...) {
      self$sample_n_taxa(size = size * nrow(self$edge_list),
                         taxon_weight = taxon_weight,
                         obs_weight = obs_weight, obs_target = obs_target,
                         use_subtaxa = use_subtaxa,
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
    nse_accessible_funcs = c("taxon_names", "taxon_ids", "n_supertaxa",
                             "n_subtaxa", "n_subtaxa_1"),
    check_dataset_name = function(target) {
      if (! target %in% names(self$data)) {
        stop(paste0("The target `", target, "` is not the name of a data set.",
                    " Valid targets include: ",
                    paste0(names(self$data), collapse = ", ")))
      }
    }
  )
)

