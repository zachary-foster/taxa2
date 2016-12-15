#' Taxonomy class
#'
#' Used to store taxonomic tree structures.
#'
#' @export
#' @param ... Any number of object of class \code{\link{hierarchy}}
#' @return An \code{R6Class} object of class \code{Taxonomy}
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
    edge_list = NULL, # Note: this should be made of taxon ids, not indexes, for consistency between subsets

    initialize = function(...) {
      input <- list(...)

      # If character strings are supplied, convert to hierarcies
      char_input_index <- which(lapply(input, class) == "character")
      input[char_input_index] <- lapply(input[char_input_index], hierarchy)

      # Parse input
      parse_data <- parse_heirarchies_to_taxonomy(input)
      self$taxa <- parse_data$taxa
      self$edge_list <- parse_data$edge_list
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Taxonomy>\n"))
      taxon_names <- vapply(self$taxa, function(x) x$name$name, character(1))
      taxon_ids <- names(self$taxa)
      if (length(self$taxa) > 0) {
        limited_print(paste(taxon_ids, taxon_names, sep = ". "),
                      prefix = paste0(indent, "  ", length(self$taxa), " taxa:"))
        limited_print(private$make_graph(),
                      prefix = paste0(indent, "  ", nrow(self$edge_list), " edges:"))
      } else {
        cat("Empty taxonomy")
      }
      invisible(self)
    },

    supertaxa = function(subset = NULL, recursive = TRUE, simplify = FALSE,
                         include_input = FALSE, return_type = "id", na = FALSE) {
      # Parse arguments
      subset <- format_taxon_subset(names(self$taxa), subset)

      # Get supertaxa
      parent_index <- match(self$edge_list$from, self$edge_list$to) # precomputing makes it much faster
      recursive_part <- function(taxon) {
        supertaxon <- parent_index[taxon]
        if (recursive) {
          if (is.na(supertaxon)) {
            output <- c(taxon, supertaxon)
          } else {
            output <- c(taxon, recursive_part(supertaxon))
          }
        } else {
          output <- c(taxon, supertaxon)
        }
        return(unname(output))
      }
      output <- lapply(subset, recursive_part)

      # Remove query taxa from output
      if (! include_input) {
        output <- lapply(output, `[`, -1)
      }

      # Remove NAs from output
      if (! na) {
        output <- lapply(output, function(x) x[!is.na(x)])
      }

      # Convert to return type
      output <- lapply(output, private$get_return_type, return_type = return_type)

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
                                include_input = TRUE, return_type = "index", na = FALSE)
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
      output <- stats::setNames(private$get_return_type(output, return_type = return_type),
                                self$edge_list$to[output])

      return(output)
    },


    stems = function(subset = NULL, return_type = "id", simplify = FALSE, exclude_leaves = FALSE) {
      # Parse arguments
      subset <- format_taxon_subset(names(self$taxa), subset)

      # Get roots to start search
      my_roots <- self$roots(subset = subset, return_type = "index")

      # Search until taxa with multiple subtaxa are found
      parent_index <- match(self$edge_list$from, self$edge_list$to) # precomputing makes it much faster
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
      output <- stats::setNames(lapply(output, private$get_return_type, return_type = return_type),
                                self$edge_list$to[my_roots])

      # Reduce dimensionality
      if (simplify) {
        output <- unique(unname(unlist(output)))
      }

      return(output)
    },


    subtaxa = function(subset = NULL, recursive = TRUE,
                       simplify = FALSE, include_input = FALSE, return_type = "id") {
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
        child_output <- lapply(children, recursive_part)
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
        starting_taxa <- unname(self$roots(subset = subset, return_type = "index"))
        output <- stats::setNames(unlist(lapply(starting_taxa, recursive_part), recursive = FALSE)[as.character(subset)],
                                  names(subset))
      } else {
        output <- lapply(subset, function(x) c(x, get_children(x)))
      }

      # Remove query taxa from output
      if (! include_input) {
        output <- lapply(output, `[`, -1)
      }

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
    make_graph = function() {
      apply(self$edge_list, 1, paste0, collapse = "->")
    },

    get_return_type = function(indexes, return_type = c("index", "id", "taxa", "hierarchies")) {
      return_type <- match.arg(return_type)
      if (return_type == "index") {
        return(as.integer(indexes))
      } else if (return_type == "id") {
        return(self$edge_list$to[indexes])
      } else if (return_type == "taxa") {
        return(self$taxa[self$edge_list$to[indexes]])
      } else if (return_type == "hierarchies") {
        return(lapply(self$supertaxa(indexes, include_input = TRUE, return_type = "taxa"),
                      function(t) do.call(hierarchy, t)))
      } else {
        stop(paste('Invailed return type: "return_type" must be one of the following:',
                   paste(return_type, collapse = ", ")))
      }
    }
  )
)

#' @keywords internal
parse_heirarchies_to_taxonomy <- function(heirarchies) {

  # Look for input edge cases
  total_taxa_count <- sum(vapply(heirarchies, function(x) length(x$taxa), numeric(1)))
  if (length(heirarchies) == 0 || total_taxa_count == 0) {
    return(list(taxa = list(), edge_list = data.frame(from = character(), to = character(),
                                                      stringsAsFactors=FALSE)))
  }

  # This is used to store both taxon names and their IDs once assigned. The IDs will be added as
  # names to the taxon character vectors
  taxon_names <- lapply(heirarchies,
                        function(heirarchy) stats::setNames(c(NA, vapply(heirarchy$taxa,
                                                                         function(taxon) taxon$name$name, character(1))), NA))
  # initialize output lists
  unique_taxa <- list()
  edge_list <- list() # matrix?
  max_id <- 0 # used to keep track of what IDs have already been used

  # Find the maximum depth of the classifications
  heirarchies_depths <- vapply(taxon_names, length, numeric(1))
  max_depth <- max(heirarchies_depths)

  # For each level in the classifications, identify unique taxa and add them to the taxon list and
  # the edge list. NOTE: This function modifies variables outside of it and is not independent.
  process_one_level <- function(depth) {

    # Identify unique pairs of taxon ids and unclassified taxon names and make new IDs
    all_pairs <- lapply(taxon_names[heirarchies_depths >= depth], function(x) c(names(x)[depth - 1], x[depth]))
    unique_encoding <- match(all_pairs, unique(all_pairs))
    new_ids <- unique_encoding + max_id
    max_id <<- max(new_ids)

    # Add new IDs to `taxon_names` as vector names
    unused_output <- mapply(seq_along(taxon_names)[heirarchies_depths >= depth], new_ids,
                            FUN = function(index, id) {names(taxon_names[[index]])[depth] <<- id})

    # Get representative taxa objects to add to the taxon list.
    # The `depth - 1` is because `NA` was added to each hierachy in `taxon_names` so the indexes
    # are one off.
    taxon_objects <- lapply(heirarchies[heirarchies_depths > (depth - 1)],
                            function(heirarchy) heirarchy$taxa[[depth - 1]])
    new_taxa <- stats::setNames(taxon_objects[match(unique(unique_encoding), unique_encoding)],
                                unique(new_ids))

    # TODO: either check that unique taxa identified by name are actually unique when considering
    # all fields (e.g. `id`) or rework function to optionally consider all fields with identifiying
    # unique taxa in the first place

    # Get edge list additions
    new_edges <- unique(lapply(taxon_names[heirarchies_depths >= depth],
                               function(x) c(names(x)[c(depth - 1, depth)])))

    # Append classified taxa and edges to ouptut of parent function
    unique_taxa <<- c(unique_taxa, new_taxa)
    edge_list <<- c(edge_list, new_edges)
  }
  no_ouput <- lapply(2:max_depth, process_one_level) # starts at 2 because of NA being prepended

  # Convert edge list to matrix
  edge_list <- stats::setNames(as.data.frame(do.call(rbind, edge_list), stringsAsFactors = FALSE),
                               c("from", "to"))

  return(list(taxa = unique_taxa, edge_list = edge_list))
}



#' @export
supertaxa <- function(obj, ...) {
  UseMethod("supertaxa")
}

#' @export
supertaxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
supertaxa.Taxonomy <- function(obj, ...) {
  obj$supertaxa(...)
}

#' Get all supertaxa of a taxon
#'
#' Return the taxon IDs or indexes of all supertaxa (i.e. all taxa the target taxa
#' are a part of) in an object of type \code{\link{taxonomy}} or \code{\link{taxmap}}.
#' \preformatted{
#' obj$supertaxa(subset = NULL, recursive = TRUE,
#'               simplify = FALSE, include_input = FALSE,
#'               return_type = "id", na = FALSE)
#' supertaxa(obj, ...)}
#'
#' @param obj The \code{taxonomy} or \code{taxmap} object containing taxon information to be
#'   queried.
#' @param subset (\code{character}) \code{taxon_ids} or indexes of \code{taxon_data} for which
#'   supertaxa will be returned. Default: All taxa in \code{obj} will be used.
#' @param recursive (\code{logical}) If \code{FALSE}, only return the supertaxa one level above the
#'   target taxa. If \code{TRUE}, return all the supertaxa of every supertaxa, etc.
#' @param simplify (\code{logical}) If \code{TRUE}, then combine all the results into a single
#'   vector of unique values.
#' @param include_input (\code{logical}) If \code{TRUE}, the input taxa are included in the output
#' @param return_type (\code{logical}) Controls output type: "index", "id", "taxa", or "hierarchies".
#' Note that "index" is the index of the edge list, not the taxon list.
#' @param na (\code{logical}) If \code{TRUE}, return \code{NA} where information is not available.
#' @param ... U Used by the S3 method to pass the parameters to the R6 method of \code{\link{taxonomy}}
#'
#' @return If \code{simplify = FALSE}, then a list of vectors are returned corresponding to the
#'   \code{subset} argument. If \code{simplify = TRUE}, then unique values are returned in a single
#'   vector.
#'
#' @family taxmap taxonomy functions
#'
#' @name supertaxa
NULL


#' @export
roots <- function(obj, ...) {
  UseMethod("roots")
}

#' @export
roots.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
roots.Taxonomy <- function(obj, ...) {
  obj$roots(...)
}

#' Get root taxa
#'
#' Return the root taxa for a \code{\link{taxmap}} object. Can also be used to get the roots of
#' a subset of taxa.
#' \preformatted{
#' obj$roots(subset = NULL, return_type = "id")
#' roots(obj, ...)}
#'
#' @param obj The \code{taxonomy} or \code{taxmap} object containing taxon information to be
#'   queried.
#' @param subset (\code{character}) Taxon IDs for which root taxa will be returned. Default: All
#'   taxon in \code{obj} will be used.
#' @param return_type (\code{logical}) Controls output type: "index", "id", "taxa", or "hierarchies".
#' Note that "index" is the index of the edge list, not the taxon list.
#' @param ... Used by the S3 method to pass the parameters to the R6 method of \code{\link{taxonomy}}
#'
#' @return \code{character}
#'
#' @name roots
NULL

#' @export
subtaxa <- function(obj, ...) {
  UseMethod("subtaxa")
}

#' @export
subtaxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
subtaxa.Taxonomy <- function(obj, ...) {
  obj$subtaxa(...)
}



#' Get subtaxa
#'
#' Return the taxon IDs or \code{taxon_data} indexes of all subtaxa in an object of type \code{taxmap}
#' \preformatted{
#' obj$subtaxa(subset = NULL, recursive = TRUE,
#'             simplify = FALSE, include_input = FALSE,
#'             return_type = "id", na = FALSE)
#' subtaxa(obj, ...)}
#'
#' @param obj The \code{taxonomy} or \code{taxmap} object containing taxon information to be
#'   queried.
#' @param subset (\code{character}) \code{taxon_ids} or indexes of \code{taxon_data} for which
#'   supertaxa will be returned. Default: All taxa in \code{obj} will be used.
#' @param recursive (\code{logical}) If \code{FALSE}, only return the subtaxa one level bwlow the
#'   target taxa. If \code{TRUE}, return all the subtaxa of every subtaxa, etc.
#' @param simplify (\code{logical}) If \code{TRUE}, then combine all the results into a single
#'   vector of unique values.
#' @param include_input (\code{logical}) If \code{TRUE}, the input taxa are included in the output
#' @param return_type (\code{logical}) Controls output type: "index", "id", "taxa", or "hierarchies".
#' Note that "index" is the index of the edge list, not the taxon list.
#'
#' @return If \code{simplify = FALSE}, then a list of vectors are returned corresponding to the
#'   \code{target} argument. If \code{simplify = TRUE}, then the unique values are returned in a
#'   single vector.
#'
#' @name subtaxa
NULL


#' @export
stems <- function(obj, ...) {
  UseMethod("stems")
}

#' @export
stems.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
stems.Taxonomy <- function(obj, ...) {
  obj$stems(...)
}

#' Get stem taxa
#'
#' Return the stem taxa for a \code{\link{taxonomy}} or a \code{\link{taxmap}} object.
#' Stem taxa are all those from the roots to the first taxon with more than one subtaxon.
#' \preformatted{
#' obj$stems(subset = NULL, simplify = FALSE,
#'           return_type = "id", exclude_leaves = FALSE)
#' stems(obj, ...)}
#'
#' @param obj The \code{taxonomy} or \code{taxmap} object containing taxon information to be
#'   queried.
#' @param subset (\code{character}) Taxon IDs for which stem taxa will be returned. Default: All
#'   taxon in \code{obj} will be used.
#' @param return_type (\code{logical}) Controls output type: "index", "id", "taxa", or "hierarchies".
#' Note that "index" is the index of the edge list, not the taxon list.
#' @param simplify (\code{logical}) If \code{TRUE}, then combine all the results into a single
#'   vector of unique values.
#' @param exclude_leaves (\code{logical}) If \code{TRUE}, the do not include taxa with no subtaxa.
#' @param ... Used by the S3 method to pass the parameters to the R6 method of \code{\link{taxonomy}}
#'
#' @return \code{character}
#'
#' @name stems
NULL
