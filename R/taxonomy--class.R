#' Taxonomy class
#'
#' Used to store taxonomic tree structures.
#'
#' @export
#' @param ... Any number of object of class [hierarchy()]
#' @return An `R6Class` object of class `Taxonomy`
#'
#' @details on initialize, we parse the inputs and find all duplicate
#' taxonomic names of the same rank, make an edge list
#'
#' @template taxonomyegs

taxonomy <- function(...) {
  Taxonomy$new(...)
}

Taxonomy <- R6::R6Class(
  "Taxonomy",
  lock_class = TRUE,
  public = list(
    taxa = NULL,
    edge_list = NULL, # Note: this should be made of taxon ids, not indexes
    input_ids = NULL, # Only used by `Taxmap` right now

    # A simple wrapper to make future changes easier
    # it returns ids named by ids for consistency with other funcs
    taxon_ids = function() {
      stats::setNames(self$edge_list$to, self$edge_list$to)
    },

    # A simple wrapper to make future changes easier
    taxon_names = function() {
      vapply(self$taxa[self$taxon_ids()],
             function(x) {
               if (is.null(x$name$name)) {
                 return(NA_character_)
               } else {
                 return(x$name$name)
               }
             },
             character(1))
    },

    taxon_ranks = function() {
      vapply(self$taxa[self$taxon_ids()],
             function(x) {
               if (is.null(x$rank$name)) {
                 return(NA_character_)
               } else {
                 return(x$rank$name)
               }
             },
             character(1))
    },

    # A simple wrapper to make future changes easier
    taxon_indexes = function() {
      stats::setNames(seq_len(nrow(self$edge_list)), self$taxon_ids())
    },

    initialize = function(...) {
      input <- list(...)

      # If character strings are supplied, convert to hierarcies
      char_input_index <- which(lapply(input, class) == "character")
      input[char_input_index] <- lapply(input[char_input_index], hierarchy)

      # Parse input
      parsed_data <- parse_heirarchies_to_taxonomy(input)
      self$taxa <- parsed_data$taxa
      self$edge_list <- parsed_data$edge_list
      self$input_ids <- parsed_data$input_ids
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Taxonomy>\n"))
      taxon_names <- vapply(self$taxa, function(x) x$name$name, character(1))
      taxon_ids <- names(self$taxa)
      if (length(self$taxa) > 0) {
        limited_print(paste(taxon_ids, taxon_names, sep = ". "),
                      prefix = paste0(indent, "  ",
                                      length(self$taxa), " taxa:"))
        limited_print(private$make_graph(),
                      prefix = paste0(indent, "  ",
                                      nrow(self$edge_list), " edges:"))
      } else {
        cat("Empty taxonomy")
      }
      invisible(self)
    },

    # Returns the names of things to be accessible using non-standard evaluation
    all_names = function() {
      output <- c()

      # Add functions included in the package
      output <- c(output, private$nse_accessible_funcs)

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
    get_data = function(name = NULL, ...) {
      # Get default if name is NULL
      if (is.null(name)) {
        name = self$all_names(...)
      }

      # Check that names provided are valid
      my_names <- self$all_names(...)
      if (any(unknown <- !name %in% my_names)) {
        stop(paste0("Cannot find the following data: ",
                    paste0(name[unknown], collapse = ", "), "\n ",
                    "Valid choices include: ",
                    paste0(my_names, collapse = ", "), "\n "))
      }

      # Format output
      name <- my_names[match(name, my_names)]
      output <- lapply(names(name),
                       function(x) eval(parse(text = paste0("self$", x))))
      names(output) <- name

      # Name each thing returned by taxon id if possibile
      #   This only applies to taxmap objects
      output[] <- lapply(seq_len(length(output)), function(index) {
        data_location <- names(name[index])
        if (startsWith(data_location, "data$")) {
          data_name <- strsplit(data_location,
                                split =  "$", fixed = TRUE)[[1]][2]
          return(stats::setNames(output[[index]],
                                 private$get_data_taxon_ids(data_name)))
        } else {
          return(output[[index]])
        }
      })

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


    supertaxa = function(subset = NULL, recursive = TRUE, simplify = FALSE,
                         include_input = FALSE, value = NULL, na = FALSE) {
      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(subset)))
      subset <- lazyeval::lazy_eval(lazyeval::lazy(subset), data = data_used)
      subset <- private$parse_nse_taxon_subset(subset)

      # Get supertaxa
      parent_index <- match(self$edge_list$from, self$edge_list$to)
      recursive_part <- function(taxon, n_recursions) {
        supertaxon <- parent_index[taxon]
        if (n_recursions) {
          if (is.na(supertaxon)) {
            output <- c(taxon, supertaxon)
          } else {
            if (is.numeric(n_recursions)) {
              n_recursions <- n_recursions - 1
            }
            output <- c(taxon, recursive_part(supertaxon,
                                              n_recursions = n_recursions))
          }
        } else {
          output <- c(taxon, supertaxon)
        }
        return(unname(output))
      }

      if (is.numeric(recursive)) {
        n_recursions <- recursive - 1 # This makes 1 the same as FALSE
      } else {
        n_recursions <- recursive
      }

      if (is.numeric(recursive) && recursive == 0) {
        output <- setNames(lapply(subset, function(x) numeric(0)), subset)
      } else {
        output <- lapply(subset, recursive_part, n_recursions = n_recursions)
      }

      # Remove query taxa from output
      if (! include_input) {
        output <- lapply(output, `[`, -1)
      }

      # Remove NAs from output
      if (! na) {
        output <- lapply(output, function(x) x[!is.na(x)])
      }

      # Look up values
      if (!is.null(value)) {
        possible_values <- self$get_data(value)[[1]]
        if (is.null(names(possible_values))) {
          output <- lapply(output, function(i) possible_values[i])
        } else {
          output <- lapply(output, function(i) possible_values[self$taxon_ids()[i]])
        }
      }

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    },


    supertaxa_apply = function(func, subset = NULL, recursive = TRUE,
                               simplify = FALSE, include_input = FALSE,
                               value = NULL, na = FALSE, ...) {
      my_sup <- eval(substitute(self$supertaxa(subset = subset,
                                               recursive = recursive,
                                               simplify = FALSE,
                                               include_input = include_input,
                                               value = value,
                                               na = na)))
      output <- lapply(my_sup, func, ...)
      if (simplify) {
        output <- unlist(output)
      }
      return(output)
    },


    roots = function(subset = NULL, value = NULL) {
      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(subset)))
      subset <- lazyeval::lazy_eval(lazyeval::lazy(subset), data = data_used)
      subset <- private$parse_nse_taxon_subset(subset)

      # Get roots
      parents <- self$supertaxa(subset = subset, recursive = TRUE,
                                include_input = TRUE, value = "taxon_indexes",
                                na = FALSE)
      is_global_root <- vapply(parents, length, numeric(1)) == 1
      if (missing(subset)) {
        is_root <- is_global_root
      } else {
        is_root <- is_global_root | vapply(parents,
                                           FUN.VALUE = logical(1),
                                           function(x) ! any(x[-1] %in% subset))
      }
      output <- unname(subset[is_root])

      # Look up values
      if (!is.null(value)) {
        possible_values <- self$get_data(value)[[1]]
        if (is.null(names(possible_values))) {
          output <- possible_values[output]
        } else {
          output <- possible_values[self$taxon_ids()[output]]
        }
      }

      return(output)
    },


    stems = function(subset = NULL, value = NULL, simplify = FALSE,
                     exclude_leaves = FALSE) {
      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(subset)))
      subset <- lazyeval::lazy_eval(lazyeval::lazy(subset), data = data_used)
      subset <- private$parse_nse_taxon_subset(subset)

      # Get roots to start search
      my_roots <- self$roots(subset = subset, value = "taxon_indexes")

      # Search until taxa with multiple subtaxa are found
      parent_index <- match(self$edge_list$from, self$edge_list$to)
      recursive_part <- function(taxon) {
        children <- which(parent_index == taxon)
        if (length(children) == 0 && ! exclude_leaves) {
          output <- taxon
        } else if (length(children) == 1) {
          output <- c(taxon, recursive_part(children))
        } else {
          output <- taxon
        }
        return(unname(output))
      }
      output <- lapply(my_roots, recursive_part)

      # Look up values
      if (!is.null(value)) {
        possible_values <- self$get_data(value)[[1]]
        if (is.null(names(possible_values))) {
          output <- lapply(output, function(i) possible_values[i])
        } else {
          output <- lapply(output, function(i) possible_values[self$taxon_ids()[i]])
        }
      }

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    },


    leaves = function(subset = NULL, value = NULL) {
      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(subset)))
      subset <- lazyeval::lazy_eval(lazyeval::lazy(subset), data = data_used)
      subset <- private$parse_nse_taxon_subset(subset)

      # Find taxa without subtaxa
      my_subtaxa <- self$subtaxa(subset = subset, recursive = TRUE,
                                 include_input = TRUE, value = "taxon_indexes")
      childless_taxa <- my_subtaxa[vapply(my_subtaxa, length, numeric(1)) == 1]
      output <- stats::setNames(unlist(childless_taxa), names(childless_taxa))

      # Look up values
      if (!is.null(value)) {
        possible_values <- self$get_data(value)[[1]]
        if (is.null(names(possible_values))) {
          output <- possible_values[output]
        } else {
          output <- possible_values[self$taxon_ids()[output]]
        }
      }

      return(output)
    },



    subtaxa = function(subset = NULL, recursive = TRUE,
                       simplify = FALSE, include_input = FALSE,
                       value = NULL) {
      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(subset)))
      subset <- lazyeval::lazy_eval(lazyeval::lazy(subset), data = data_used)
      subset <- private$parse_nse_taxon_subset(subset)

      # Return empty list if `subset` has no values
      if (length(subset) == 0) {
        if (simplify) {
          return(vector(mode = class( self$get_data(value)[[1]])))
        } else {
          return(list())
        }
      }

      # Get subtaxa
      parent_index <- match(self$edge_list$from, self$edge_list$to)

      get_children <- function(taxon) {
        which(parent_index == taxon)
      }

      recursive_part <- function(taxon) {
        # Get immediate children of current taxon
        children <- get_children(taxon)
        # Run this function on them to get their output
        child_output <- lapply(children, recursive_part) # stops if no children
        child_output <- stats::setNames(unlist(child_output, recursive = FALSE),
                                        unlist(lapply(child_output, names)))
        # Get all subtaxa from the names of the child output
        child_taxa <- c(taxon, as.numeric(names(child_output)))
        # Combine the child output with the subtaxa for the current taxon
        output <- stats::setNames(c(list(child_taxa), child_output),
                                  c(taxon, names(child_output)))
        return(output)
      }

      if (recursive) {
        starting_taxa <- unname(self$roots(subset = subset,
                                           value = "taxon_indexes"))
        output <- stats::setNames(
          unlist(lapply(starting_taxa, recursive_part),
                 recursive = FALSE)[as.character(subset)],
          names(subset)
        )
      } else {
        output <- lapply(subset, function(x) c(x, get_children(x)))
      }

      # Remove query taxa from output
      if (! include_input) {
        output <- lapply(output, `[`, -1)
      }

      # Simulate limited recursion
      #
      # To increase speed, the recursive algorithm only searches starting at
      # root taxa, but this makes it hard to limit the number of rankes returned
      # below each taxon during recursion. Instead, a finite number of
      # recursions are simulated by filtering the results of tarversing the
      # entire tree and comparing rank depth between each taxon and its subtaxa.
      if (is.numeric(recursive) && recursive >= 0) {
        all_taxa <- unique(c(names(output), unlist(output)))
        rank_depth <- vapply(self$supertaxa(all_taxa), length, numeric(1))
        output_names <- names(output)
        output <- lapply(seq_along(output), function(i) {
          subtaxa_ids <- self$taxon_ids()[output[[i]]]
          subtaxa_depth <- rank_depth[subtaxa_ids]
          current_depth <- rank_depth[names(output[i])]
          passing_taxa <- subtaxa_depth - current_depth <= recursive
          return(output[[i]][passing_taxa])
        })
        names(output) <- output_names
      }

      # Look up values
      if (!is.null(value)) {
        possible_values <- self$get_data(value)[[1]]
        if (is.null(names(possible_values))) {
          output <- lapply(output, function(i) possible_values[i])
        } else {
          output <- lapply(output, function(i) possible_values[self$taxon_ids()[i]])
        }
      }

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    },


    subtaxa_apply = function(func, subset = NULL, recursive = TRUE,
                             simplify = FALSE, include_input = FALSE,
                             value = NULL, ...) {
      my_sub <- eval(substitute(self$subtaxa(subset = subset,
                                             recursive = recursive,
                                             simplify = FALSE,
                                             include_input = include_input,
                                             value = value)))
      output <- lapply(my_sub, func, ...)
      if (simplify) {
        output <- unlist(output)
      }
      return(output)
    },


    id_classifications = function(sep = ";") {
      vapply(self$supertaxa(recursive = TRUE, include_input = TRUE,
                            value = "taxon_ids", na = FALSE),
             function(x) paste0(rev(x), collapse = sep), character(1))
    },

    name_classifications = function(sep = ";") {
      vapply(self$supertaxa(recursive = TRUE, include_input = TRUE,
                            value = "taxon_names", na = FALSE),
             function(x) paste0(rev(x), collapse = sep), character(1))
    },

    n_supertaxa = function() {
      vapply(self$supertaxa(recursive = TRUE, include_input = FALSE,
                            value = "taxon_indexes", na = FALSE),
             length, numeric(1))
    },

    n_subtaxa = function() {
      vapply(self$subtaxa(recursive = TRUE, include_input = FALSE,
                          value = "taxon_indexes"),
             length, numeric(1))
    },

    n_subtaxa_1 = function() {
      vapply(self$subtaxa(recursive = FALSE, include_input = FALSE,
                          value = "taxon_indexes"),
             length, numeric(1))
    },

    is_root = function() {
      stats::setNames(is.na(self$edge_list$from), self$taxon_ids())
    },

    is_leaf = function() {
      self$n_subtaxa() == 0
    },

    is_stem = function() {
      stats::setNames(self$taxon_ids() %in% self$stems(simplify = TRUE,
                                                       value = "taxon_indexes"),
                      self$taxon_ids())
    },

    is_branch = function() {
      stats::setNames(! (self$is_root() | self$is_leaf() | self$is_stem()),
                      self$taxon_ids())
    },

    filter_taxa = function(..., subtaxa = FALSE, supertaxa = FALSE,
                           taxonless = FALSE, reassign_obs = TRUE,
                           reassign_taxa = TRUE, invert = FALSE) {
      # Check that a taxmap option is not used with a taxonomy object
      is_taxmap <- "Taxmap" %in% class(self)
      if (!is_taxmap) {
        if (!missing(reassign_obs)) {
          warning(paste('The option "reassign_obs" can only be used with',
                        '`taxmap` objects. It will have no effect on a',
                        '`taxonomy` object.'))
        }
        if (!missing(taxonless)) {
          warning(paste('The option "taxonless" can only be used with',
                        '`taxmap` objects. It will have no effect on a',
                        '`taxonomy` object.'))
        }
      }

      # non-standard argument evaluation
      selection <- private$parse_nse_taxon_subset(...)

      # Get taxa of subset
      if (is.logical(subtaxa) && subtaxa == FALSE) {
        subtaxa = 0
      }
      if (is.logical(supertaxa) && supertaxa == FALSE) {
        supertaxa = 0
      }
      taxa_subset <- unique(c(selection,
                              self$subtaxa(subset = selection,
                                           recursive = subtaxa,
                                           value = "taxon_indexes",
                                           include_input = FALSE,
                                           simplify = TRUE),
                              self$supertaxa(subset = selection,
                                             recursive = supertaxa,
                                             value = "taxon_indexes",
                                             na = FALSE, simplify = TRUE,
                                             include_input = FALSE)
      ))

      # Invert selection
      if (invert) {
        taxa_subset <- (1:nrow(self$edge_list))[-taxa_subset]
      }

      # Reassign taxonless observations
      if (is_taxmap) {
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
                       private$get_data_taxon_ids(data_index)))) {
            return(NULL) # if there is no taxon id info, dont change anything
          }

          # Generate replacement taxon ids
          to_reassign <- ! data_taxon_ids %in% self$taxon_ids()[taxa_subset]
          supertaxa_key <- self$supertaxa(
            subset = unique(data_taxon_ids[to_reassign]),
            recursive = TRUE, simplify = FALSE, include_input = FALSE,
            value = "taxon_indexes", na = FALSE
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
      }

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
          value = "taxon_indexes", na = FALSE)
        reassign_key <- vapply(supertaxa_key, reassign_one, character(1)
        )
        self$edge_list[to_reassign, "from"] <-
          reassign_key[self$taxon_ids()[to_reassign]]
      }

      # Remove taxonless observations
      if (is_taxmap) {
        taxonless <- parse_possibly_named_logical(
          taxonless,
          self$data,
          default = formals(self$filter_taxa)$taxonless
        )
        process_one <- function(my_index) {

          # Get the taxon ids of the current object
          if (is.null((data_taxon_ids <-
                       private$get_data_taxon_ids(my_index)))) {
            return(NULL) # if there is no taxon id info, dont change anything
          }

          obs_subset <- data_taxon_ids %in% self$taxon_ids()[taxa_subset]
          private$remove_obs(dataset = my_index,
                             indexes = obs_subset,
                             unname_only = taxonless[my_index])
        }
        unused_output <- lapply(seq_along(self$data), process_one)
      }

      # Remove filtered taxa
      private$remove_taxa(taxa_subset)

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

    sample_n_taxa = function(size, taxon_weight = NULL, obs_weight = NULL,
                             obs_target = NULL, use_subtaxa = TRUE,
                             collapse_func = mean, ...) {
      # Check that a taxmap option is not used with a taxonomy object
      is_taxmap <- "Taxmap" %in% class(self)
      if (!is_taxmap) {
        if (!missing(obs_weight)) {
          warning(paste('The option "obs_weight" can only be used with',
                        '`taxmap` objects. It will have no effect on a',
                        '`taxonomy` object.'))
        }
        if (!missing(obs_target)) {
          warning(paste('The option "obs_target" can only be used with',
                        '`taxmap` objects. It will have no effect on a',
                        '`taxonomy` object.'))
        }
      }

      # non-standard argument evaluation
      data_used <- eval(substitute(self$data_used(taxon_weight, obs_weight)))
      taxon_weight <- lazyeval::lazy_eval(lazyeval::lazy(taxon_weight),
                                          data = data_used)
      obs_weight <- lazyeval::lazy_eval(lazyeval::lazy(obs_weight),
                                        data = data_used)

      # Calculate observation component of taxon weights
      if (is.null(obs_weight) || !is_taxmap) {
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

      # Sample
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


    map_data = function(from, to, warn = TRUE) {
      # non-standard argument evaluation
      data_used <- structure(list(taxon_names = structure(c("Mammalia", "Plantae",
                                                            "Felidae", "Notoryctidae", "Solanaceae", "unidentified", "unidentified",
                                                            "Panthera", "Puma", "Notoryctes", "Solanum", "tigris", "concolor",
                                                            "typhlops", "lycopersicum", "tuberosum"), .Names = c("1", "2",
                                                                                                                 "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14",
                                                                                                                 "15", "16")), taxon_ranks = structure(c("class", "kingdom", "family",
                                                                                                                                                         "family", "family", NA, NA, "genus", "genus", "genus", "genus",
                                                                                                                                                         "species", "species", "species", "species", "species"), .Names = c("1",
                                                                                                                                                                                                                            "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
                                                                                                                                                                                                                            "14", "15", "16"))), .Names = c("taxon_names", "taxon_ranks"))
      to_data <- lazyeval::lazy_eval(lazyeval::lazy(to), data = data_used)
      from_data <- lazyeval::lazy_eval(lazyeval::lazy(from), data = data_used)

      # # check that arguments have taxon ids and evaluate
      # validate_and_eval <- function(unparsed) {
      #   parsed <- lazyeval::lazy_eval(lazyeval::lazy(unparsed),
      #                                 data = data_used)
      #   if (! private$valid_taxon_ids(names(parsed))) {
      #     stop(paste0("The value `", deparse(match.call()$unparsed),
      #                 "` is not named by taxon id or contains invalid ids. ",
      #                 "Use `taxon_ids()` to see the valid ids. ",
      #                 "Use `warn = FALSE` to ignore this."))
      #   }
      #   return(parsed)
      # }
      # to_data <- eval(substitute(validate_and_eval(to)))
      # from_data <- eval(substitute(validate_and_eval(from)))

      # Check for multiple different values of `to` for each `from`
      is_one_to_one <- vapply(unique(names(to_data)),
                              function(n) {
                                length(unique(to_data[names(to_data)==n])) == 1
                              },
                              logical(1))
      if (warn && any(! is_one_to_one)) {
        warning(paste0('There are multiple unique values of "',
                       deparse(match.call()$to),
                       '" for at least one value of "',
                       deparse(match.call()$from),
                       '". Only the first instance will be returned. ',
                       'To get all instances, use the `obs` function.'))
      }

      # Map values using taxon ids
      stats::setNames(to_data[match(names(from_data), names(to_data))],
                      from_data)
    }

  ),

  private = list(
    nse_accessible_funcs = c("taxon_names", "taxon_ids", "taxon_indexes",
                             "n_supertaxa", "n_subtaxa", "n_subtaxa_1",
                             "taxon_ranks", "is_root", "is_stem", "is_branch",
                             "is_leaf"),

    make_graph = function() {
      apply(self$edge_list, 1, paste0, collapse = "->")
    },

    remove_taxa = function(el_indexes) {
      # Remove taxa objects
      self$taxa <- self$taxa[self$taxon_ids()[el_indexes]]

      # Remove corresponding rows in the edge list
      self$edge_list <- self$edge_list[el_indexes, , drop = FALSE]

      # Replace and edges going to removed taxa with NA
      self$edge_list[! self$edge_list$from %in% self$taxon_ids(), "from"] <-
        as.character(NA)
    },

    # Takes one ore more NSE expressions and resolves them to indexes of edgelist rows
    # Each expression can resolve to taxon ids, edgelist indexes, or logical.
    parse_nse_taxon_subset = function(...) {
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

      # convert to indexes, named by taxon id
      names(selection) <- self$taxon_ids()
      selection <- which(selection)

      return(selection)
    },

    # check if a set of putative taxon ids are valid.
    # returns TRUE/FALSE
    valid_taxon_ids = function(ids) {
      !is.null(ids) && all(ids %in% self$taxon_ids())
    }
  )
)
