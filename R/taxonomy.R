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
  # vctrs::vec_assert(taxa, ptype = taxon())
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
#' @param taxa A [taxon()] vector.
#' @param supertaxa The indexes of `taxa` for each taxon's supertaxon.
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon`
#' @family classes
#'
#' @examples
#'
#' x <- taxonomy(c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo',
#'                 'Panthera tigris', 'Ursidae', 'Ursus', 'Ursus arctos'),
#'               supertaxa = c(NA, 1, 2, 3, 3, 1, 6, 7))
#'
#'
#'
#' x <- taxonomy(taxon(name = c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo',
#'                              'Panthera tigris', 'Ursidae', 'Ursus', 'Ursus arctos'),
#'                     rank = c('order', 'family', 'genus', 'species',
#'                              'species', 'family', 'genus', 'species'),
#'                     id = taxon_id(c('33554', '9681', '9688', '9689',
#'                                     '9694', '9632', '9639', '9644'),
#'                                   db = 'ncbi'),
#'                     auth = c('Bowdich, 1821', 'Fischer de Waldheim, 1817', 'Oken, 1816', 'L., 1758',
#'                              'L., 1758', 'Fischer de Waldheim, 1817', 'L., 1758', 'L., 1758')),
#'               supertaxa = c(NA, 1, 2, 3, 3, 1, 6, 7))
#'
#' # Get parts of the taxonomy vector
#' tax_name(x)
#' tax_rank(x)
#' tax_id(x)
#' tax_db(x)
#' tax_auth(x)
#' tax_author(x)
#' tax_date(x)
#' tax_cite(x)
#'
#' # Set parts of the taxonomy vector
#' tax_name(x) <- tolower(tax_name(x))
#' tax_rank(x)[1] <- NA
#' tax_id(x) <- '9999'
#' tax_db(x) <- 'itis'
#' tax_auth(x) <- NA
#' tax_author(x)[2:3] <- c('Joe', 'Billy')
#' tax_date(x) <- c('1999', '2013', '1796', '1899')
#' tax_cite(x)[1] <- 'Linnaeus, C. (1771). Mantissa plantarum altera generum.'
#'
#' @export
taxonomy <- function(taxa = taxon(), supertaxa = integer()) {
  # Cast inputs to correct values
  #   NOTE: for some reason vec_cast is not working with taxon vectors in all cases, so not used here.
  if (!is_taxon(taxa)) {
    taxa <- taxon(taxa)
  }
  supertaxa <- vctrs::vec_cast(supertaxa, integer())

  # Recycle ranks and databases to common length
  c(taxa, supertaxa) %<-% vctrs::vec_recycle_common(taxa, supertaxa)

  # Create taxon object
  new_taxonomy(taxa = taxa, supertaxa = supertaxa)
}


#' @importFrom methods setOldClass
#' @exportClass taxa_taxonomy
setOldClass(c("taxa_taxonomy", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------

#' @rdname tax_id
#' @export
tax_id.taxa_taxonomy <- function(x) {
  tax_id(vctrs::field(x, "taxa"))
}

#' @rdname tax_id
#' @export
`tax_id<-.taxa_taxonomy` <- function(x, value) {
  tax_id(vctrs::field(x, "taxa")) <- value
  return(x)
}



#' @rdname tax_db
#' @export
tax_db.taxa_taxonomy <- function(x) {
  tax_db(tax_id(x))
}

#' @rdname tax_db
#' @export
`tax_db<-.taxa_taxonomy` <- function(x, value) {
  tax_db(tax_id(x)) <- value
  return(x)
}



#' @rdname tax_author
#' @export
tax_author.taxa_taxonomy <- function(x) {
  tax_author(tax_auth(x))
}

#' @rdname tax_author
#' @export
`tax_author<-.taxa_taxonomy` <- function(x, value) {
  tax_author(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_date
#' @export
tax_date.taxa_taxonomy <- function(x) {
  tax_date(tax_auth(x))
}

#' @rdname tax_date
#' @export
`tax_date<-.taxa_taxonomy` <- function(x, value) {
  tax_date(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_cite
#' @export
tax_cite.taxa_taxonomy <- function(x) {
  tax_cite(tax_auth(x))
}

#' @rdname tax_cite
#' @export
`tax_cite<-.taxa_taxonomy` <- function(x, value) {
  tax_cite(tax_auth(x)) <- value
  return(x)
}



#' @rdname tax_name
#' @export
tax_name.taxa_taxonomy <- function(x) {
  tax_name(vctrs::field(x, "taxa"))
}

#' @rdname tax_name
#' @export
`tax_name<-.taxa_taxonomy` <- function(x, value) {
  tax_name(vctrs::field(x, "taxa")) <- value
  return(x)
}



#' @rdname tax_auth
#' @export
tax_auth.taxa_taxonomy <- function(x) {
  tax_auth(vctrs::field(x, "taxa"))
}

#' @rdname tax_auth
#' @export
`tax_auth<-.taxa_taxonomy` <- function(x, value) {
  tax_auth(vctrs::field(x, "taxa")) <- value
  return(x)
}



#' @rdname tax_rank
#' @export
tax_rank.taxa_taxonomy <- function(x) {
  tax_auth(vctrs::field(x, "taxa"))
}

#' @rdname tax_rank
#' @export
`tax_rank<-.taxa_taxonomy` <- function(x, value) {
  tax_auth(vctrs::field(x, "taxa")) <- value
  return(x)
}



#' @rdname taxonomy
#' @export
names.taxa_taxonomy <- function(x) {
  names(vctrs::field(x, "taxa"))
}

#' @rdname taxonomy
#' @export
`names<-.taxa_taxonomy` <- function(x, value) {
  names(vctrs::field(x, "taxa")) <- value
  return(x)
}


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
  if (! is.null(names(x)) && ! all(is.na(names(x)))) {
    name_printed <- paste0(font_punct('['), names(x), font_punct(']'))
  } else {
    name_printed <- ''
  }
  tax_char <- paste0(seq_len(length(x)),
                     name_printed,
                     font_punct(': '),
                     printed_taxon(vctrs::field(x, 'taxa'), color = TRUE))
  sub_char <- lapply(subtaxa(x, recursive = FALSE), function(i) tax_char[i])
  tree_data <- data.frame(stringsAsFactors = FALSE,
                          taxa = tax_char,
                          subtaxa = I(sub_char))
  trees <- lapply(roots(x), function(i) cli::tree(tree_data, root = tax_char[i]))
  trees <- unlist(trees)

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



#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxonomy <- function(x) {
  "taxonomy"
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_full.taxa_taxonomy <- function(x) {
  "taxonomy"
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




#' @export
supertaxa <- function(x, ...) {
  UseMethod('supertaxa')
}


#' @rdname supertaxa
#'
#' @export
supertaxa <- function(subset = NULL, recursive = TRUE, simplify = FALSE,
                      include_input = FALSE, value = "taxon_indexes", na = FALSE) {
  # non-standard argument evaluation
  data_used <- eval(substitute(self$data_used(subset)))
  subset <- rlang::eval_tidy(rlang::enquo(subset), data = data_used)
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
    output <- simplify(output)
  }

  return(output)
}