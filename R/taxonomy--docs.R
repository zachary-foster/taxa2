#' Get taxon IDs
#'
#' Return the taxon IDs in a [taxmap()] object.
#' They are in the order they appear in the edge list.
#'
#' \preformatted{
#' obj$taxon_ids()
#' taxon_ids(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @name taxon_ids
NULL


#' Get taxon names
#'
#' Return the taxon names in a [taxmap()] object.
#' They are in the order they appear in the edge list.
#'
#' \preformatted{
#' obj$taxon_names()
#' taxon_names(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @name taxon_names
NULL


#' Get all supertaxa of a taxon
#'
#' Return the taxon IDs or indexes of all supertaxa (i.e. all taxa the target
#' taxa are a part of) in an object of type [taxonomy()] or
#' [taxmap()]. \preformatted{ obj$supertaxa(subset = NULL, recursive =
#' TRUE, simplify = FALSE, include_input = FALSE, return_type = "id", na =
#' FALSE) supertaxa(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object containing taxon
#'   information to be queried.
#' @param subset (`character`) `taxon_ids` or indexes of
#'   `taxon_data` for which supertaxa will be returned. Default: All taxa
#' in `obj` will be used.
#' @param recursive (`logical` or `numeric`) If `FALSE`, only return the
#'   supertaxa one rank above the target taxa. If `TRUE`, return all the
#'   supertaxa of every supertaxa, etc. Positive numbers indicate the number of
#'   recursions (i.e. number of ranks above the target taxon to return). `0` is
#'   equivalent to `FALSE`. Negative numbers are equivalent to `TRUE`.
#' @param simplify (`logical`) If `TRUE`, then combine all the results into a
#'   single vector of unique values.
#' @param include_input (`logical`) If `TRUE`, the input taxa are included in
#'   the output
#' @param return_type (`logical`) Controls output type: "index", "id",
#'   "taxa", or "hierarchies". Note that "index" is the index of the edge list,
#'   not the taxon list.
#' @param na (`logical`) If `TRUE`, return `NA` where information
#'   is not available.
#' @param ... U Used by the S3 method to pass the parameters to the R6 method of
#'   [taxonomy()]
#'
#' @return If `simplify = FALSE`, then a list of vectors are returned
#'   corresponding to the `subset` argument. If `simplify = TRUE`,
#'   then unique values are returned in a single vector.
#'
#' @family taxmap taxonomy functions
#'
#' @name supertaxa
NULL


#' Get root taxa
#'
#' Return the root taxa for a [taxmap()] object. Can also be used to
#' get the roots of a subset of taxa. \preformatted{ obj$roots(subset = NULL,
#' return_type = "id") roots(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object containing taxon
#'   information to be queried.
#' @param subset (`character`) Taxon IDs for which root taxa will be
#'   returned. Default: All taxon in `obj` will be used.
#' @param return_type (`logical`) Controls output type: "index", "id",
#'   "taxa", or "hierarchies". Note that "index" is the index of the edge list,
#'   not the taxon list.
#' @param ... Used by the S3 method to pass the parameters to the R6 method of
#'   [taxonomy()]
#'
#' @return `character`
#'
#' @name roots
NULL


#' Get subtaxa
#'
#' Return the taxon IDs or `taxon_data` indexes of all subtaxa in an object
#' of type `taxmap` \preformatted{ obj$subtaxa(subset = NULL, recursive =
#' TRUE, simplify = FALSE, include_input = FALSE, return_type = "id", na =
#' FALSE) subtaxa(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object containing taxon
#'   information to be queried.
#' @param subset (`character`) `taxon_ids` or indexes of
#'   `taxon_data` for which supertaxa will be returned. Default: All taxa
#'   in `obj` will be used.
#' @param recursive (`logical`) If `FALSE`, only return the subtaxa
#'   one level bwlow the target taxa. If `TRUE`, return all the subtaxa of
#'   every subtaxa, etc.
#' @param simplify (`logical`) If `TRUE`, then combine all the results
#'   into a single vector of unique values.
#' @param include_input (`logical`) If `TRUE`, the input taxa are
#'   included in the output
#' @param return_type (`logical`) Controls output type: "index", "id",
#'   "taxa", or "hierarchies". Note that "index" is the index of the edge list,
#'   not the taxon list.
#'
#' @return If `simplify = FALSE`, then a list of vectors are returned
#'   corresponding to the `target` argument. If `simplify = TRUE`,
#'   then the unique values are returned in a single vector.
#'
#' @name subtaxa
NULL


#' Get stem taxa
#'
#' Return the stem taxa for a [taxonomy()] or a [taxmap()]
#' object. Stem taxa are all those from the roots to the first taxon with more
#' than one subtaxon. \preformatted{ obj$stems(subset = NULL, simplify = FALSE,
#' return_type = "id", exclude_leaves = FALSE) stems(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object containing taxon
#'   information to be queried.
#' @param subset (`character`) Taxon IDs for which stem taxa will be
#'   returned. Default: All taxon in `obj` will be used.
#' @param return_type (`logical`) Controls output type: "index", "id",
#'   "taxa", or "hierarchies". Note that "index" is the index of the edge list,
#'   not the taxon list.
#' @param simplify (`logical`) If `TRUE`, then combine all the results
#'   into a single vector of unique values.
#' @param exclude_leaves (`logical`) If `TRUE`, the do not include
#'   taxa with no subtaxa.
#' @param ... Used by the S3 method to pass the parameters to the R6 method of
#'   [taxonomy()]
#'
#' @return `character`
#'
#' @name stems
NULL


#' Get leaf taxa
#'
#' Return the leaf taxa for a [taxmap()] object. Leaf taxa are taxa
#' with no subtaxa. \preformatted{ obj$leaves(subset = NULL, return_type = "id")
#' leaves(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object containing taxon
#'   information to be queried.
#' @param subset (`character`) Taxon IDs for which leaf taxa will be
#'   returned. Default: All taxon in `obj` will be used.
#' @param return_type (`logical`) Controls output type: "index", "id",
#'   "taxa", or "hierarchies". Note that "index" is the index of the edge list,
#'   not the taxon list.
#' @param ... Used by the S3 method to pass the parameters to the R6 method of
#'   [taxonomy()]
#'
#' @return `character`
#'
#' @name leaves
NULL


#' Get classifications of taxa
#'
#' Get classification strings of taxa in an object of type [taxmap()]
#' composed of taxon IDs. Each classification is constructed by concatenating
#' the taxon ids of the given taxon and its supertaxa. \preformatted{
#' obj$id_classifications(sep = ";") id_classifications(obj, sep = ";")}
#'
#' @param obj ([taxmap()])
#' @param sep (`character` of length 1) The character(s) to place between
#'   taxon IDs
#'
#' @return `character`
#'
#' @examples
#' id_classifications(ex_taxmap)
#'
#' @family taxon_funcs
#'
#' @name id_classifications
NULL


#' Get classifications of taxa
#'
#' Get classification strings of taxa in an object of type [taxmap()]
#' composed of taxon names. Each classification is constructed by concatenating
#' the taxon names of the given taxon and its supertaxa. \preformatted{
#' obj$name_classifications(sep = ";") name_classifications(obj, sep = ";")}
#'
#' @param obj ([taxmap()])
#' @param sep (`character` of length 1) The character(s) to place between
#'   taxon names
#'
#' @return `character`
#'
#' @examples
#' name_classifications(ex_taxmap)
#'
#' @family taxon_funcs
#'
#' @name name_classifications
NULL


#' Get number of supertaxa
#'
#' Get number of supertaxa for each taxon in an object of type
#' [taxmap()] \preformatted{ obj$n_supertaxa() n_supertaxa(obj)}
#'
#' @param obj ([taxmap()])
#'
#' @return \code{numeric}
#'
#' @examples
#' n_supertaxa(ex_taxmap)
#'
#' @family taxon_funcs
#'
#' @name n_supertaxa
NULL


#' Get number of subtaxa
#'
#' Get number of subtaxa for each taxon in an object of type
#' [taxmap()] \preformatted{ obj$n_subtaxa() n_subtaxa(obj)}
#'
#' @param obj ([taxmap()])
#'
#' @return \code{numeric}
#'
#' @examples
#' n_subtaxa(ex_taxmap)
#'
#' @family taxon_funcs
#'
#' @name n_subtaxa
NULL


#' Get number of subtaxa
#'
#' Get number of subtaxa for each taxon in an object of type
#' [taxmap()], not including subtaxa of subtaxa etc. This does not
#' include subtaxa assigned to subtaxa. \preformatted{ obj$n_subtaxa_1()
#' n_subtaxa_1(obj)}
#'
#' @param obj ([taxmap()])
#'
#' @return \code{numeric}
#'
#' @examples
#' n_subtaxa_1(ex_taxmap)
#'
#' @family taxon_funcs
#'
#' @name n_subtaxa_1
NULL
