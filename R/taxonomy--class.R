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
    taxon_ids = function() {
      self$edge_list$to
    },

    # A simple wrapper to make future changes easier
    taxon_names = function() {
      vapply(self$taxa[self$edge_list$to],
             function(x) x$name$name, character(1))
    },

    # A simple wrapper to make future changes easier
    taxon_indexes = function() {
      seq_len(nrow(self$edge_list))
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

    supertaxa = function(subset = NULL, recursive = TRUE, simplify = FALSE,
                         include_input = FALSE, return_type = "id",
                         na = FALSE) {
      # Parse arguments
      subset <- format_taxon_subset(names(self$taxa), subset)

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

      # Convert to return type
      output <- lapply(output, private$get_return_type,
                       return_type = return_type)

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    },

    roots = function(subset = NULL, return_type = "id") {
      # Parse arguments
      subset <- format_taxon_subset(names(self$taxa), subset)

      # Get roots
      parents <- self$supertaxa(subset = subset, recursive = TRUE,
                                include_input = TRUE, return_type = "index",
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

      # Convert to return type
      output <- stats::setNames(
        private$get_return_type(output, return_type = return_type),
        self$edge_list$to[output]
      )

      return(output)
    },


    stems = function(subset = NULL, return_type = "id", simplify = FALSE,
                     exclude_leaves = FALSE) {
      # Parse arguments
      subset <- format_taxon_subset(names(self$taxa), subset)

      # Get roots to start search
      my_roots <- self$roots(subset = subset, return_type = "index")

      # Search until taxa with multiple subtaxa are found
      parent_index <- match(self$edge_list$from, self$edge_list$to)
      recursive_part <- function(taxon) {
        children <- which(parent_index == taxon)
        if (length(children) == 0 && ! exclude_leaves) {
          output <- taxon
        } else if (length(children) == 1) {
          output <- c(taxon, recursive_part(supertaxon))
        } else {
          output <- taxon
        }
        return(unname(output))
      }
      output <- lapply(my_roots, recursive_part)

      # Convert to return type
      output <- stats::setNames(lapply(output, private$get_return_type,
                                       return_type = return_type),
                                self$edge_list$to[my_roots])

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    },


    leaves = function(subset = NULL, return_type = "id") {
      # Parse arguments
      subset <- format_taxon_subset(names(self$taxa), subset)

      # Find taxa without subtaxa
      my_subtaxa <- self$subtaxa(subset = subset, recursive = TRUE,
                                 include_input = TRUE, return_type = "index")
      output <- unlist(my_subtaxa[vapply(my_subtaxa, length, numeric(1)) == 1])

      # Convert to return type
      output <- stats::setNames(
        private$get_return_type(output,return_type = return_type),
        self$edge_list$to[output]
      )

      return(output)
    },



    subtaxa = function(subset = NULL, recursive = TRUE,
                       simplify = FALSE, include_input = FALSE,
                       return_type = "id") {
      # Parse arguments
      subset <- format_taxon_subset(names(self$taxa), subset)

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
                                           return_type = "index"))
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

      # Convert to return type
      output <- lapply(output, private$get_return_type,
                       return_type = return_type)


      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    },

    id_classifications = function(sep = ";") {
      vapply(self$supertaxa(recursive = TRUE, include_input = TRUE,
                            return_type = "id", na = FALSE),
             function(x) paste0(rev(x), collapse = sep), character(1))
    },

    name_classifications = function(sep = ";") {
      vapply(self$supertaxa(recursive = TRUE, include_input = TRUE,
                            return_type = "name", na = FALSE),
             function(x) paste0(rev(x), collapse = sep), character(1))
    },

    n_supertaxa = function() {
      vapply(self$supertaxa(recursive = TRUE, include_input = FALSE,
                            return_type = "index", na = FALSE),
             length, numeric(1))
    },

    n_subtaxa = function() {
      vapply(self$subtaxa(recursive = TRUE, include_input = FALSE,
                          return_type = "index"),
             length, numeric(1))
    },

    n_subtaxa_1 = function(obj) {
      vapply(self$subtaxa(recursive = FALSE, include_input = FALSE,
                          return_type = "index"),
             length, numeric(1))
    }

  ),

  private = list(
    make_graph = function() {
      apply(self$edge_list, 1, paste0, collapse = "->")
    },

    get_return_type = function(
      indexes,
      return_type = c("index", "id", "taxa", "hierarchies", "name")) {
      return_type <- match.arg(return_type)
      if (return_type == "index") {
        return(as.integer(indexes))
      } else if (return_type == "id") {
        return(self$edge_list$to[indexes])
      } else if (return_type == "taxa") {
        return(self$taxa[self$edge_list$to[indexes]])
      } else if (return_type == "hierarchies") {
        return(lapply(self$supertaxa(indexes, include_input = TRUE,
                                     return_type = "taxa"),
                      function(t) do.call(hierarchy, t)))
      } else if (return_type == "name") {
        return(self$taxon_names()[indexes])
      } else {
        stop(paste('Invailed return type: "return_type"',
                   'must be one of the following:',
                   paste(return_type, collapse = ", ")))
      }
    }
  )
)
