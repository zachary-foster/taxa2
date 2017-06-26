#' Taxmap class
#'
#' A class designed to store a taxonomy and associated information.
#' This class builds on the [taxonomy()] class.
#' User defined data can be stored in the list `obj$data`, where `obs` is a taxmap object.
#' Data that is associated with taxa can be manipulated in a variety of ways using functions like `filter_taxa` and `filter_obs`.
#' To associate the items of lists/vectors with taxa, name them by [taxon_ids()].
#' For tables, add a column named `taxon_id` that stores [taxon_ids()].
#'
#' @export
#' @param ... Any number of object of class [hierarchy()] or character vectors.
#' @param data A list of tables with data associated with the taxa.
#' @family classes
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

      # Get item names
      if (is.null(names(self$data))) {
        data_names <- paste0("[[", seq_len(length(self$data)), "]]")
      } else {
        data_names <- names(self$data)
        data_names[data_names == ""] <- paste0("[[", which(data_names == ""),
                                                "]]")
      }

      # Print a subset of each item, up to a max number, then just print names
      cat(paste0("  ", length(self$data), " data sets:\n"))
      if (length(self$data) > 0) {
        for (i in 1:min(c(max_items, length(self$data)))) {
          print_item(self$data[[i]],
                     name = data_names[i], max_rows = max_rows,
                     max_width = max_width, prefix = "    ")
        }
        if (length(self$data) > max_items) {
          cat(paste0("    And ", length(self$data) - max_items,
                     " more data sets:"))
          limited_print(data_names[(max_items + 1):length(self$data)])
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


    obs = function(data, value = NULL, subset = NULL, recursive = TRUE,
                   simplify = FALSE) {
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


    map_data = function(to = "taxon_names", from = "taxon_ids") {
      from_data <- self$get_data(from)[[1]]
      to_data <- self$get_data(to)[[1]]
      stats::setNames(to_data[match(names(from_data), names(to_data))],
                      from_data)



    },


    obs_apply = function(data, func, simplify = FALSE, value = NULL,
                         subset = NULL, recursive = TRUE, ...) {
      my_obs <- eval(substitute(self$obs(data, simplify = FALSE, value = value, subset = subset,
                                         recursive = recursive)))
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



#' Convert one or more data sets to taxmap
#'
#' Parses taxonomic information and assoiacted data and stores it in a
#' [taxa::taxmap()] object. [Taxonomic
#' classifications](https://en.wikipedia.org/wiki/Taxonomy_(biology)#Classifying_organisms)
#' must be present somewhere in the first input.
#'
#' @param tax_data A table, list, or vector that contains the names of taxa that
#'   represent [taxonomic classifications](https://en.wikipedia.org/wiki/Taxonomy_(biology)#Classifying_organisms).
#'    Accepted representations of classifications include:
#'   * A list/vector or table with column(s) of taxon names: Something like
#'   `"Animalia;Chordata;Mammalia;Primates;Hominidae;Homo"`. What separator(s)
#'   is used (";" in this example) can be changed with the `class_sep` option.
#'   For tables, the classification can be spread over multiple columns and the
#'   separator(s) will be applied to each column, although each column could
#'   just be single taxon names with no separator. Use the `class_cols` option
#'   to specify which columns have taxon names.
#'   * A list in which each entry is a classificaiton. For example,
#'   `list(c("Animalia", "Chordata", "Mammalia", "Primates", "Hominidae",
#'   "Homo"), ...)`.
#'   * A list of data.frames where each represents a classification with one
#'   taxon per row. The column that contains taxon names is specified using the
#'   `class_cols` option. In this instance, it only makes sense to specify a
#'   single column.
#' @param datasets Additional lists/vectors/tables that should be included in
#'   the resulting `taxmap` object. The `mappings` option is use to specify how
#'   these data sets relate to the `tax_data` and, by inference, what taxa apply
#'   to each item.
#' @param class_cols (`character` or `integer`) The names or indexes of columns
#'   that contain classifications if the first input is a table. If mutliple
#'   columns are specified, they will be combined in the order given.
#' @param class_sep (`character`) One or more separators that delineate taxon
#'   names in a classification. For example, if one column had `"Homo sapiens"`
#'   and another had `"Animalia;Chordata;Mammalia;Primates;Hominidae"`, then
#'   `class_sep = c(" ", ";")`. All separators are applied to each column so
#'   order does not matter.
#' @param sep_is_regex (`TRUE`/`FALSE`) Whether or not `class_sep` should be
#'   used as a [regular
#'   expression](https://en.wikipedia.org/wiki/Regular_expression).
#' @param mappings (named `character`) This defines how the taxonomic
#'   information in `tax_data` applies to data set in `datasets`. This option
#'   should have the same number of inputs as `datasets`, with values
#'   corresponding to each data set. The names of the character vector specify
#'   what information in `tax_data` is shared with info in each `dataset`, which
#'   is specified by the corresponding values of the character vector. If there
#'   are no shared variables, you can add `NA` as a placeholder, but you could
#'   just leave that data out since it is not benifiting from being in the
#'   taxmap object. The names/values can be one of the following:
#'   * For tables, the names of columns can be used.
#'   * `"{{index}}"` : This means to use the index of rows/items
#'   * `"{{name}}"`  : This means to use row/item names.
#'   * `"{{value}}"` : This means to use the values in vectors or lists. Lists
#'   will be converted to vectors using [unlist()].
#' @param include_tax_data (`TRUE`/`FALSE`) Whether or not to include `tax_data`
#'   as a dataset, like those in `datasets`.
#'
#' @examples
#'   # Make example data with taxonomic classifications
#'   species_data <- data.frame(taxonomy = c("Mammalia;Carnivora;Felidae",
#'                                           "Mammalia;Carnivora;Felidae",
#'                                           "Mammalia;Carnivora;Ursidae"),
#'                              species = c("Panthera leo",
#'                                          "Panthera tigris",
#'                                          "Ursus americanus"),
#'                              species_id = c("A", "B", "C"))
#'
#'   # Make example data associated with the taxonomic data
#'   # Note how this does not contain classifications, but
#'   # does have a varaible in common with "species_data" ("id" = "species_id")
#'   abundance <- data.frame(id = c("A", "B", "C", "A", "B", "C"),
#'                           sample = c(1, 1, 1, 2, 2, 2),
#'                           counts = c(23, 4, 3, 34, 5, 13))
#'
#'   # Make another related data set named by species id
#'   common_names <- c(A = "Lion", B = "Tiger", C = "Bear", "Oh my!")
#'
#'   # Make another related data set with no names
#'   foods <- list(c("ungulates", "boar"),
#'                 c("ungulates", "boar"),
#'                 c("salmon", "fruit", "nuts"))
#'
#'   # Make a taxmap object with these three datasets
#'   x = parse_tax_data(species_data,
#'                      datasets = list(counts = abundance,
#'                                      names = common_names,
#'                                      foods = foods),
#'                      mappings = c("species_id" = "id",
#'                                   "species_id" = "{{name}}",
#'                                   "{{index}}" = "{{index}}"),
#'                      class_cols = c("taxonomy", "species"),
#'                      class_sep = c(" ", ";"))
#'
#'   # Note how all the datasets have taxon ids now
#'   x$data
#'
#'   # This allows for complex mappings between variables that other functions use
#'   map_data(x, "foods", "names")
#'   map_data(x, "names", "counts")
#'
#' @export
parse_tax_data <- function(tax_data, datasets = list(), class_cols = 1,
                           class_sep = ";", sep_is_regex = FALSE,
                           mappings = c(), include_tax_data = TRUE) {

  # Check for nonsensical options
  if (length(datasets) != length(mappings)) {
    stop(paste0('The `mappings` option must have the same number of values (',
                length(datasets), ') as the `datasets` option.'))
  }
  if (length(mappings) > 0 && is.null(names(mappings))) {
    stop(paste0('The mapping options must be named.'))
  }
  for (i in seq_len(length(datasets))) {
    valid_mappings <- c("{{index}}", "{{name}}", "{{value}}")
    if (is.data.frame(datasets[[i]])) {
      valid_mappings <- c(valid_mappings, colnames(datasets[[i]]))
    }
    if (is.data.frame(tax_data)) {
      valid_mappings <- c(valid_mappings, colnames(tax_data))
    }
    mappings_used <- c(mappings[[i]], names(mappings[[i]]))
    invalids <- mappings_used[! mappings_used %in% valid_mappings]
    if (length(invalids) > 0) {
      stop(paste0('Invalid inputs to the `mappings` found for input "', invalids[1], '". ',
                  'The names and values of `mappings` must be one of the following: ',
                  paste0(valid_mappings, collapse = ", ")))
    }
  }

  # Deal with edge cases
  if (length(tax_data) == 0) {
    return(taxmap())
  }

  # Get classificaitons
  multi_sep_split <- function(input, split, ...) {
    lapply(input, function(x) {
      for (sep in split) {
        x <- unlist(strsplit(x, split = sep, ...))
      }
      return(x)
    })
  }

  is_list_of_frames <- FALSE
  if (is.character(tax_data)) { # is a character vector
    parsed_tax <- multi_sep_split(tax_data, fixed = !sep_is_regex, split = class_sep)
  } else if (is.data.frame(tax_data)) { # is a data.frame
    parsed_tax <- lapply(seq_len(nrow(tax_data)),
                         function(i) {
                           class_source <- as.character(unlist(tax_data[i, class_cols]))
                           unname(unlist(multi_sep_split(class_source,
                                                  fixed = !sep_is_regex,
                                                  split = class_sep)))
                         })
  } else if (is.list(tax_data) && is.data.frame(tax_data[[1]])) { # is a list of data.frames
    if (length(class_cols) > 1) {
      stop("When the taxonomy source is a list of data.frames, it does not make sense to specify multiple columns.")
    }
    parsed_tax <- unlist(lapply(tax_data, function(x) {
      lapply(seq_len(nrow(x)), function(i) {
        as.character(x[1:i, class_cols])
      })
    }), recursive = FALSE)
    tax_data <- do.call(rbind, tax_data)
    is_list_of_frames <- TRUE
  } else if (is.list(tax_data) && is.character(tax_data[[1]])) { # is a list of characters
    parsed_tax <- lapply(tax_data, function(x) unlist(multi_sep_split(x, split = class_sep)))
  } else {
    stop("Unknown format for first input. Cannot parse taxonomic information.")
  }

  # Create taxmap object
  output <- do.call(taxmap, parsed_tax)

  # Add taxonomic source to datasets
  if (include_tax_data) {
    datasets <- c(list(tax_data = tax_data), datasets)
    mappings <- c("{{index}}" = "{{index}}", mappings)
  }

  # Convert additional tables to tibbles
  are_tables <- vapply(datasets, is.data.frame, logical(1))
  datasets[are_tables] <- lapply(datasets[are_tables], dplyr::as.tbl)

  # Add additional data sets
  get_sort_var <- function(data, var) {
    if (var == "{{index}}") {
      if (is.data.frame(data)) {
        return(seq_len(nrow(data)))
      } else {
        return(seq_len(length(data)))
      }
    } else if (var == "{{name}}") {
      if (is.data.frame(data)) {
        return(rownames(data))
      } else {
        return(names(data))
      }
    }  else if (var == "{{value}}") {
      if (is.data.frame(data)) {
        stop("The `{{value}}` setting of the `mappings` option cannot be used with data.frames.")
      } else {
        return(unlist(data))
      }
    } else if (is.data.frame(data) && var %in% colnames(data)) {
      return(data[[var]])
    } else {
      stop(paste0('No column named "', var, '"."'))
    }
  }

  name_datset <- function(dataset, sort_var) {
    target_value <- get_sort_var(dataset, sort_var)
    source_value <- get_sort_var(tax_data, names(sort_var))
    obs_taxon_ids <- output$input_ids[match(target_value, source_value)]
    if (is.data.frame(dataset)) {
      dataset <- dplyr::bind_cols(dplyr::tibble(taxon_id = obs_taxon_ids),
                                  dataset)
    } else {
      names(dataset) <- obs_taxon_ids
    }
    return(dataset)
  }

  output$data <- lapply(seq_len(length(datasets)),
                        function(i) name_datset(datasets[[i]], mappings[i]))

  # Name additional datasets
  names(output$data) <- names(datasets)

  # Fix incorrect taxon ids in data if a list of data.frames is given
  if (is_list_of_frames && include_tax_data) {
    output$data[[1]] <- output$data[[1]][!duplicated(output$data[[1]]), ]
  }

  return(output)
}
