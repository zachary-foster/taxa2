#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxonomy constructor
#'
#' Minimal taxonomy constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param taxa A [taxon()] vector.
#' @param supertaxa The indexes of `taxa` for each taxon's supertaxon.
#'
#' @return An `S3` object of class `taxa_taxon`
#'
#' @keywords internal
new_taxonomy <- function(taxa = taxon(), supertaxa = integer()) {
  # Check that values are the correct type
  vctrs::vec_assert(taxa, ptype = taxon())
  vctrs::vec_assert(supertaxa, ptype = integer())

  # Create new object
  vctrs::new_rcrd(list(taxa = taxa, supertaxa = supertaxa),
                  class = 'taxa_taxonomy')
}


#' Taxonomy class
#'
#' \Sexpr[results=rd, stage=render]{taxa:::lifecycle("experimental")}
#' Used to store information about a set of taxa forming a taxonomic tree.
#'
#' @param ... Used to pass arguments to methods and allow methods to used additional arguments.
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon`
#' @family classes
#'
#' @examples
#'
#'
#' @export
taxonomy <- function(...) {
  UseMethod("taxonomy")
}


#' @rdname taxonomy
#'
#' @param taxa A [taxon()] vector.
#' @param supertaxa The indexes of `taxa` for each taxon's supertaxon.
#'
#' @export
taxonomy.default <- function(taxa = taxon(), supertaxa = integer(), ...) {
  # Cast inputs to correct values
  taxa <- vctrs::vec_cast(taxa, taxon())
  supertaxa <- vctrs::vec_cast(supertaxa, integer())

  # Recycle ranks and databases to common length
  c(taxa, supertaxa) %<-% vctrs::vec_recycle_common(taxa, supertaxa)

  # Create taxon object
  new_taxonomy(taxa = taxa, supertaxa = supertaxa)
}


setOldClass(c("taxa_taxon", "vctrs_vctr"))





#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------



#' @rdname taxa_printing_funcs
#' @rdname taxon
#' @export
#' @keywords internal
obj_print_data.taxa_taxonomy <- function(x) {
  # Dont print anything if nothing to print
  if (length(x) == 0) {
    return()
  }

  # Make tree print out
  tax_char <- paste0(seq_len(length(x)),
                     font_punct(': '),
                     printed_taxon(vctrs::field(x, 'taxa'), color = TRUE))
  sub_char <- lapply(subtaxa(x, recursive = FALSE), function(i) tax_char[i])
  tree_data <- data.frame(stringsAsFactors = FALSE,
                          taxa = tax_char,
                          subtaxa = I(sub_char))
  trees <- lapply(roots(x), function(i) cli::tree(tree_data, root = tax_char[i]))
  trees <- unlist(trees)

  # if (value != "taxon_ids") {
  #   value <- self$get_data(value)[[1]]
  #   ids_in_tree <- sub(trees, pattern = "^[\u2502 \u251C\u2500\u2514]*", replacement = "")
  #   trees <- vapply(seq_len(length(trees)), FUN.VALUE = character(1),
  #                   function(i) {
  #                     sub(trees[i], pattern = ids_in_tree[i],
  #                         replacement = value[ids_in_tree[i]])
  #
  #                   })
  # }

  # Return tree
  class(trees) <- unique(c("tree", "character"))
  print(trees)
  invisible(x)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
obj_print_footer.taxa_taxonomy <- function(x) {
  vctrs::obj_print_footer(vctrs::field(x, 'taxa'))
}


#' @export
is_root <- function(x, ...) {
  UseMethod('is_root')
}


#' @rdname is_root
#'
#' @export
is_root.taxa_taxonomy <- function(x, ...) {
  is.na(vctrs::field(x, 'supertaxa'))
}


#' @export
roots <- function(x, ...) {
  UseMethod('roots')
}


#' @rdname roots
#'
#' @export
roots.taxa_taxonomy <- function(x, ...) {
  which(is_root(x))
}


#' @export
subtaxa <- function(x, ...) {
  UseMethod('subtaxa')
}


#' @rdname roots
#'
#' @export
subtaxa.taxa_taxonomy <- function(x, recursive = TRUE, include_input = FALSE,
                                  value = "taxon_indexes", ...) {
  # Get subtaxa
  get_children <- function(taxon) {
    which(vctrs::field(x, 'supertaxa') == taxon)
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
    output <- stats::setNames(
      unlist(lapply(roots(x), recursive_part), recursive = FALSE),
      names(subset)
    )
  } else {
    output <- lapply(seq_len(length(x)), function(i) c(i, get_children(i)))
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
    stop('not implemented')
    # all_taxa <- unique(c(self$map_data(taxon_ids, taxon_indexes)[names(output)],
    #                      unlist(output)))
    # rank_depth <- vapply(self$supertaxa(all_taxa), length, numeric(1))
    # output_names <- names(output)
    # output <- lapply(seq_along(output), function(i) {
    #   subtaxa_ids <- self$taxon_ids()[output[[i]]]
    #   subtaxa_depth <- rank_depth[subtaxa_ids]
    #   current_depth <- rank_depth[names(output[i])]
    #   passing_taxa <- subtaxa_depth - current_depth <= recursive
    #   return(output[[i]][passing_taxa])
    # })
    # names(output) <- output_names
  }

  return(output)
}

