#' Get taxon IDs
#'
#' Return the taxon IDs in a [taxonomy()] or [taxmap()] object.
#' They are in the order they appear in the edge list.
#' \preformatted{
#' obj$taxon_ids()
#' taxon_ids(obj)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#'
#' @family taxonomy data functions
#'
#' @examples
#' taxa:::taxon_ids(ex_taxmap)
#'
#' @name taxon_ids
#' @keywords internal
NULL


#' Get taxon indexes
#'
#' Return the taxon indexes in a [taxonomy()] or [taxmap()] object.
#' They are the indexes of the edge list rows.
#' \preformatted{
#' obj$taxon_indexes()
#' taxon_indexes(obj)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#'
#' @family taxonomy data functions
#'
#' @examples
#' taxon_indexes(ex_taxmap)
#'
#' @name taxon_indexes
NULL


#' Get taxon names
#'
#' Return the taxon names in a [taxonomy()] or [taxmap()] object.
#' They are in the order they appear in the edge list.
#' \preformatted{
#' obj$taxon_names()
#' taxon_names(obj)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#'
#' @family taxonomy data functions
#'
#' @examples
#' taxa:::taxon_names(ex_taxmap)
#'
#' @name taxon_names
#' @keywords internal
NULL


#' Get taxon ranks
#'
#' Return the taxon ranks in a [taxonomy()] or [taxmap()] object.
#' They are in the order taxa appear in the edge list.
#' \preformatted{
#' obj$taxon_ranks()
#' taxon_ranks(obj)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#'
#' @family taxonomy data functions
#'
#' @examples
#' taxa:::taxon_ranks(ex_taxmap)
#'
#' @name taxon_ranks
#' @keywords internal
NULL


#' Get all supertaxa of a taxon
#'
#' Return data for supertaxa (i.e. all taxa the target
#' taxa are a part of) of each taxon in a [taxonomy()] or [taxmap()] object.
#' \preformatted{
#' obj$supertaxa(subset = NULL, recursive = TRUE,
#'   simplify = FALSE, include_input = FALSE,
#'   value = NULL, na = FALSE)
#' supertaxa(obj, subset = NULL, recursive = TRUE,
#'   simplify = FALSE, include_input = FALSE,
#'   value = NULL, na = FALSE)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object containing taxon
#'   information to be queried.
#' @param subset (`character`) `taxon_ids` or indexes of
#'   `taxon_data` for which supertaxa will be returned. Default: All taxa
#' in `obj` will be used.
#' @param recursive (`logical` or `numeric`) If `FALSE`, only return the
#'   supertaxa one rank above the target taxa. If `TRUE`, return all the
#'   supertaxa of every supertaxa, etc. Positive numbers indicate the number of
#'   recursions (i.e. number of ranks above the target taxon to return). `1` is
#'   equivalent to `FALSE`. Negative numbers are equivalent to `TRUE`.
#' @param simplify (`logical`) If `TRUE`, then combine all the results into a
#'   single vector of unique values.
#' @param include_input (`logical`) If `TRUE`, the input taxa are included in
#'   the output
#' @param value What data to return. Any result of [all_names()] can be used, but it
#'   usually only makes sense to use data that has an associated taxon id.
#' @param na (`logical`) If `TRUE`, return `NA` where information
#'   is not available.
#'
#' @return If `simplify = FALSE`, then a list of vectors are returned
#'   corresponding to the `subset` argument. If `simplify = TRUE`,
#'   then unique values are returned in a single vector.
#'
#' @family taxonomy indexing functions
#'
#' @name supertaxa
#'
#' @examples
#' # return the indexes for supertaxa for each taxon
#' supertaxa(ex_taxmap)
#'
#' # Only return data for some taxa using taxon indexes
#' supertaxa(ex_taxmap, subset = 1:3)
#'
#' # Only return data for some taxa using taxon ids
#' supertaxa(ex_taxmap, subset = c("3", "4"))
#'
#' # Only return data for some taxa using logical tests
#' supertaxa(ex_taxmap, subset = taxon_ranks == "species")
#'
#' # Only return supertaxa one level above
#' supertaxa(ex_taxmap, recursive = FALSE)
#'
#' # Only return supertaxa some number of ranks above
#' supertaxa(ex_taxmap, recursive = 2)
#'
#' # Return something besides taxon indexes
#' supertaxa(ex_taxmap, value = "taxon_names")
NULL


#' Apply function to supertaxa of each taxon
#'
#' Apply a function to the supertaxa for each taxon. This is similar
#' to using [supertaxa()] with [lapply()] or [sapply()].
#' \preformatted{
#' obj$supertaxa_apply(func, subset = NULL, recursive = TRUE,
#'   simplify = FALSE, include_input = FALSE, value = NULL,
#'   na = FALSE, ...)
#' supertaxa_apply(obj, func, subset = NULL, recursive = TRUE,
#'   simplify = FALSE, include_input = FALSE, value = NULL,
#'   na = FALSE, ....)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object containing taxon
#'   information to be queried.
#' @param func (`function`) The function to apply.
#' @param subset (`character`) `taxon_ids` or indexes of
#'   `taxon_data` for which supertaxa will be returned. Default: All taxa
#' in `obj` will be used.
#' @param recursive (`logical` or `numeric`) If `FALSE`, only return the
#'   supertaxa one rank above the target taxa. If `TRUE`, return all the
#'   supertaxa of every supertaxa, etc. Positive numbers indicate the number of
#'   recursions (i.e. number of ranks above the target taxon to return). `1` is
#'   equivalent to `FALSE`. Negative numbers are equivalent to `TRUE`.
#' @param simplify (`logical`) If `TRUE`, then combine all the results into a
#'   single vector of unique values.
#' @param include_input (`logical`) If `TRUE`, the input taxa are included in
#'   the output
#' @param value What data to give to the function. Any result of
#'   `all_names(obj)` can be used, but it usually only makes sense to use data
#'   that has an associated taxon id.
#' @param na (`logical`) If `TRUE`, return `NA` where information
#'   is not available.
#' @param ... Extra arguments are passed to the function.
#'
#' @name supertaxa_apply
#'
#' @examples
#' # Get classifications for each taxon
#' supertaxa_apply(ex_taxmap, paste, collapse = ";", include_input = TRUE,
#'                 value = "taxon_names")
NULL


#' Get root taxa
#'
#' Return the root taxa for a [taxonomy()] or [taxmap()] object. Can also be used to
#' get the roots of a subset of taxa.
#' \preformatted{
#' obj$roots(subset = NULL, value = NULL)
#' roots(obj, subset = NULL, value = NULL)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object containing taxon
#'   information to be queried.
#' @param subset (`character`) Taxon IDs for which root taxa will be
#'   returned. Default: All taxon in `obj` will be used.
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of `all_names(obj)` can be used, but it
#'   usually only makes sense to data that corresponds to taxa 1:1, such as
#'   [taxon_ranks()]. By default, taxon indexes are returned.
#'
#' @family taxonomy indexing functions
#'
#' @return `character`
#'
#' @examples
#' # Return indexes of root taxa
#' roots(ex_taxmap)
#'
#' # Return indexes for a subset of taxa
#' roots(ex_taxmap, subset = 2:17)
#'
#' # Return something besides taxon indexes
#' roots(ex_taxmap, value = "taxon_names")
#'
#' @name roots
NULL


#' Get "branch" taxa
#'
#' Return the "branch" taxa for a [taxonomy()] or [taxmap()] object. A branch is
#' anything that is not a root, stem, or, leaf. Its the interior of the tree
#' after the first split starting from the roots. Can also be used to get the
#' branches of a subset of taxa.
#' \preformatted{
#' obj$branches(subset = NULL, value = NULL)
#' branches(obj, subset = NULL, value = NULL)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object containing taxon
#'   information to be queried.
#' @param subset Taxon IDs or TRUE/FALSE vector used to subset the tree prior to
#'   determining branches. Default: All taxa in `obj` will be used. Any variable
#'   name that appears in [all_names()] can be used as if it was a vector on its
#'   own. Note that branches are determined after the filtering, so a given
#'   taxon might be a branch on the unfiltered tree, but not a branch
#'   on the filtered tree.
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of [all_names()] can be used, but it
#'   usually only makes sense to use data that corresponds to taxa 1:1, such as
#'   [taxon_ranks()]. By default, taxon indexes are returned.
#'
#' @family taxonomy indexing functions
#'
#' @return `character`
#'
#' @examples
#' # Return indexes of branch taxa
#' branches(ex_taxmap)
#'
#' # Return indexes for a subset of taxa
#' branches(ex_taxmap, subset = 2:17)
#' branches(ex_taxmap, subset = n_obs > 1)
#'
#' # Return something besides taxon indexes
#' branches(ex_taxmap, value = "taxon_names")
#'
#' @name branches
NULL


#' Get "internode" taxa
#'
#' Return the "internode" taxa for a [taxonomy()] or [taxmap()] object. An
#' internode is any taxon with a single immediate supertaxon and a single
#' immediate subtaxon. They can be removed from a tree without any loss of
#' information on the relative relationship between remaining taxa. Can also be
#' used to get the internodes of a subset of taxa.
#' \preformatted{
#' obj$internodes(subset = NULL, value = NULL)
#' internodes(obj, subset = NULL, value = NULL)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object containing taxon
#'   information to be queried.
#' @param subset Taxon IDs, TRUE/FALSE vector, or taxon IDs used to subset the tree prior to
#'   determining internodes. Default: All taxa in `obj` will be used. Any variable
#'   name that appears in [all_names()] can be used as if it was a vector on its
#'   own. Note that internodes are determined after the filtering, so a given
#'   taxon might be a internode on the unfiltered tree, but not a internode
#'   on the filtered tree.
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of [all_names()] can be used, but it
#'   usually only makes sense to use data that corresponds to taxa 1:1, such as
#'   [taxon_ranks()]. By default, taxon indexes are returned.
#'
#' @family taxonomy indexing functions
#'
#' @return `character`
#'
#' @examples
#' \dontrun{
#'
#' # Return indexes of branch taxa
#' internodes(ex_taxmap)
#'
#' # Return indexes for a subset of taxa
#' internodes(ex_taxmap, subset = 2:17)
#' internodes(ex_taxmap, subset = n_obs > 1)
#'
#' # Return something besides taxon indexes
#' internodes(ex_taxmap, value = "taxon_names")
#'
#' # Visualize which taxa are internodes
#' library(metacoder)
#' heat_tree(ex_taxmap, node_label = taxon_names,
#'           node_color = ifelse(is_internode, "red", "grey"))
#' }
#' @name internodes
NULL


#' Get subtaxa
#'
#' Return data for the subtaxa of each taxon in an [taxonomy()] or [taxmap()]
#' object.
#' \preformatted{
#' obj$subtaxa(subset = NULL, recursive = TRUE,
#'   simplify = FALSE, include_input = FALSE, value = NULL)
#' subtaxa(obj, subset = NULL, recursive = TRUE,
#'   simplify = FALSE, include_input = FALSE, value = NULL)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object containing taxon
#'   information to be queried.
#' @param subset (`character`) `taxon_ids` or taxon indexes for which supertaxa
#'   will be returned. Default: All taxa in `obj` will be used.
#' @param recursive (`logical` or `numeric`) If `FALSE`, only return the subtaxa
#'   one rank below the target taxa. If `TRUE`, return all the subtaxa of every
#'   subtaxa, etc. Positive numbers indicate the number of ranks below the
#'   immediate subtaxa to return. `1` is equivalent to `FALSE`. Negative numbers
#'   are equivalent to `TRUE`. Since the algorithm is optimized for traversing
#'   all of large trees, `numeric` values greater than 0 for this option
#'   actually take slightly longer to compute than either TRUE or FALSE.
#' @param simplify (`logical`) If `TRUE`, then combine all the results
#'   into a single vector of unique values.
#' @param include_input (`logical`) If `TRUE`, the input taxa are
#'   included in the output
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of [all_names()] can be used, but it
#'   usually only makes sense to data that corresponds to taxa 1:1, such as
#'   [taxon_ranks()]. By default, taxon indexes are returned.
#'
#' @return If `simplify = FALSE`, then a list of vectors are returned
#'   corresponding to the `target` argument. If `simplify = TRUE`,
#'   then the unique values are returned in a single vector.
#'
#' @family taxonomy indexing functions
#'
#' @name subtaxa
#'
#' @examples
#' # return the indexes for subtaxa for each taxon
#' subtaxa(ex_taxmap)
#'
#' # Only return data for some taxa using taxon indexes
#' subtaxa(ex_taxmap, subset = 1:3)
#'
#' # Only return data for some taxa using taxon ids
#' subtaxa(ex_taxmap, subset = c("3", "4"))
#'
#' # Only return data for some taxa using logical tests
#' subtaxa(ex_taxmap, subset = taxon_ranks == "genus")
#'
#' # Only return subtaxa one level below
#' subtaxa(ex_taxmap, recursive = FALSE)
#'
#' # Only return subtaxa some number of ranks below
#' subtaxa(ex_taxmap, recursive = 2)
#'
#' # Return something besides taxon indexes
#' subtaxa(ex_taxmap, value = "taxon_names")
NULL


#' Apply function to subtaxa of each taxon
#'
#' Apply a function to the subtaxa for each taxon. This is similar
#' to using [subtaxa()] with [lapply()] or [sapply()].
#' \preformatted{
#' obj$subtaxa_apply(func, subset = NULL, recursive = TRUE,
#'   simplify = FALSE, include_input = FALSE, value = NULL, ...)
#' subtaxa_apply(obj, func, subset = NULL, recursive = TRUE,
#'   simplify = FALSE, include_input = FALSE, value = NULL, ...)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object containing taxon
#'   information to be queried.
#' @param func (`function`) The function to apply.
#' @param subset (`character`) `taxon_ids` or indexes of
#'   `taxon_data` for which supertaxa will be returned. Default: All taxa
#' in `obj` will be used.
#' @param recursive (`logical` or `numeric`) If `FALSE`, only return the
#'   supertaxa one rank above the target taxa. If `TRUE`, return all the
#'   supertaxa of every supertaxa, etc. Positive numbers indicate the number of
#'   recursions (i.e. number of ranks above the target taxon to return). `1` is
#'   equivalent to `FALSE`. Negative numbers are equivalent to `TRUE`.
#' @param simplify (`logical`) If `TRUE`, then combine all the results into a
#'   single vector of unique values.
#' @param include_input (`logical`) If `TRUE`, the input taxa are included in
#'   the output
#' @param value What data to give to the function. Any result of
#'   `all_names(obj)` can be used, but it usually only makes sense to use data
#'   that has an associated taxon id.
#' @param ... Extra arguments are passed to the function.
#'
#' @name subtaxa_apply
#'
#' @examples
#' # Count number of subtaxa in each taxon
#' subtaxa_apply(ex_taxmap, length)
NULL



#' Get stem taxa
#'
#' Return the stem taxa for a [taxonomy()] or a [taxmap()]
#' object. Stem taxa are all those from the roots to the first taxon with more
#' than one subtaxon.
#' \preformatted{
#' obj$stems(subset = NULL, simplify = FALSE,
#'   value = NULL, exclude_leaves = FALSE)
#' stems(obj, subset = NULL, simplify = FALSE,
#'   value = NULL, exclude_leaves = FALSE)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object containing taxon
#'   information to be queried.
#' @param subset (`character`) Taxon IDs for which stem taxa will be
#'   returned. Default: All taxon in `obj` will be used.
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of `all_names(obj)` can be used, but it
#'   usually only makes sense to data that corresponds to taxa 1:1, such as
#'   [taxon_ranks()]. By default, taxon indexes are returned.
#' @param simplify (`logical`) If `TRUE`, then combine all the results
#'   into a single vector of unique values.
#' @param exclude_leaves (`logical`) If `TRUE`, the do not include
#'   taxa with no subtaxa.
#'
#' @return `character`
#'
#' @family taxonomy indexing functions
#'
#' @examples
#' # Return indexes of stem taxa
#' stems(ex_taxmap)
#'
#' # Return indexes for a subset of taxa
#' stems(ex_taxmap, subset = 2:17)
#'
#' # Return something besides taxon indexes
#' stems(ex_taxmap, value = "taxon_names")
#'
#' @name stems
NULL


#' Get leaf taxa
#'
#' Return the leaf taxa for a [taxonomy()] or [taxmap()] object. Leaf taxa are taxa
#' with no subtaxa.
#' \preformatted{
#' obj$leaves(subset = NULL, value = NULL)
#' leaves(obj, subset = NULL, value = NULL)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object containing taxon
#'   information to be queried.
#' @param subset (`character`) Taxon IDs for which leaf taxa will be
#'   returned. Default: All taxon in `obj` will be used.
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of `all_names(obj)` can be used, but it
#'   usually only makes sense to data that corresponds to taxa 1:1, such as
#'   [taxon_ranks()]. By default, taxon indexes are returned.
#'
#' @return `character`
#'
#' @family taxonomy indexing functions
#'
#' @examples
#' # Return indexes of leaf taxa
#' leaves(ex_taxmap)
#'
#' # Return indexes for a subset of taxa
#' leaves(ex_taxmap, subset = 2:17)
#'
#' # Return something besides taxon indexes
#' leaves(ex_taxmap, value = "taxon_names")
#'
#' @name leaves
NULL


#' Get classifications of taxa
#'
#' Get classifications of taxa in an object of type [taxonomy()] or [taxmap()]
#' composed of data assoicated with taxa. Each classification is constructed by
#' concatenating the data of the given taxon and all of its supertaxa.
#' \preformatted{
#' obj$classifications(value = "taxon_names", sep = ";")
#' classifications(obj, value = "taxon_names", sep = ";")}
#'
#' @param obj ([taxonomy()] or [taxmap()])
#' @param value What data to return. Any result of `all_names(obj)` can be used,
#'   but it usually only makes sense to data that corresponds to taxa 1:1, such
#'   as [taxon_ranks()]. By default, taxon indexes are returned.
#' @param sep (`character` of length 1) The character(s) to place between
#'   taxon IDs
#'
#' @return `character`
#'
#' @examples
#'
#' # Defualt settings returns taxon names separated by ;
#' classifications(ex_taxmap)
#'
#' # Other values can be returned besides taxon names
#' classifications(ex_taxmap, value = "taxon_ids")
#'
#' # The separator can also be changed
#' classifications(ex_taxmap, value = "taxon_ranks", sep = "||")
#'
#' @family taxonomy data functions
#'
#' @name classifications
NULL


#' Get ID classifications of taxa
#'
#' Get classification strings of taxa in an object of type [taxonomy()] or [taxmap()]
#' composed of taxon IDs. Each classification is constructed by concatenating
#' the taxon ids of the given taxon and its supertaxa.
#' \preformatted{
#' obj$id_classifications(sep = ";")
#' id_classifications(obj, sep = ";")}
#'
#' @param obj ([taxonomy()] or [taxmap()])
#' @param sep (`character` of length 1) The character(s) to place between
#'   taxon IDs
#'
#' @return `character`
#'
#' @examples
#' id_classifications(ex_taxmap)
#'
#' @family taxonomy data functions
#'
#' @name id_classifications
NULL


#' Get number of supertaxa
#'
#' Get number of supertaxa for each taxon in an object of type
#' [taxonomy()] or [taxmap()].
#' \preformatted{
#' obj$n_supertaxa()
#' n_supertaxa(obj)}
#'
#' @param obj ([taxonomy()] or [taxmap()])
#'
#' @return \code{numeric}
#'
#' @examples
#' n_supertaxa(ex_taxmap)
#'
#' @family taxonomy data functions
#'
#' @name n_supertaxa
NULL


#' Get number of supertaxa
#'
#' Get number of immediate supertaxa (i.e. not supertaxa of supertaxa, etc) for
#' each taxon in an object of type [taxonomy()] or [taxmap()]. This should
#' always be either 1 or 0.
#' \preformatted{
#' obj$n_supertaxa_1()
#' n_supertaxa_1(obj)}
#'
#' @param obj ([taxonomy()] or [taxmap()])
#'
#' @return \code{numeric}
#'
#' @examples
#' n_supertaxa_1(ex_taxmap)
#'
#' @family taxonomy data functions
#'
#' @name n_supertaxa_1
NULL


#' Get number of subtaxa
#'
#' Get number of subtaxa for each taxon in an object of type
#' [taxonomy()] or [taxmap()]
#' \preformatted{
#' obj$n_subtaxa()
#' n_subtaxa(obj)}
#'
#' @param obj ([taxonomy()] or [taxmap()])
#'
#' @return \code{numeric}
#'
#' @examples
#' n_subtaxa(ex_taxmap)
#'
#' @family taxonomy data functions
#'
#' @name n_subtaxa
NULL


#' Get number of subtaxa
#'
#' Get number of subtaxa for each taxon in an object of type
#' [taxonomy()] or [taxmap()], not including subtaxa of subtaxa etc. This does not
#' include subtaxa assigned to subtaxa.
#' \preformatted{
#' obj$n_subtaxa_1()
#' n_subtaxa_1(obj)}
#'
#' @param obj ([taxonomy()] or [taxmap()])
#'
#' @return \code{numeric}
#'
#' @examples
#' n_subtaxa_1(ex_taxmap)
#'
#' @family taxonomy data functions
#'
#' @name n_subtaxa_1
NULL


#' Return names of data in [taxonomy()] or [taxmap()]
#'
#' Return the names of data that can be used with functions in the taxa
#' package that use [non-standard evaluation](http://adv-r.had.co.nz/Computing-on-the-language.html),
#' like [filter_taxa()].
#' \preformatted{
#' obj$all_names(tables = TRUE, funcs = TRUE,
#'   others = TRUE, warn = FALSE)
#' all_names(obj, tables = TRUE, funcs = TRUE,
#'   others = TRUE, warn = FALSE)}
#'
#' @param obj ([taxonomy()] or [taxmap()]) The object containing
#'   taxon information to be queried.
#' @param tables This option only applies to [taxmap()] objects. If `TRUE`,
#'   include the names of columns of tables in `obj$data`
#' @param funcs This option only applies to [taxmap()] objects. If `TRUE`,
#'   include the names of user-definable functions in `obj$funcs`.
#' @param others This option only applies to [taxmap()] objects. If `TRUE`,
#'   include the names of data in `obj$data` besides tables.
#' @param builtin_funcs This option only applies to [taxmap()] objects. If
#'   `TRUE`, include functions like [n_supertaxa()] that provide information for
#'   each taxon.
#' @param warn If `TRUE`, warn if there are duplicate names.
#'
#' @return `character`
#'
#' @examples
#' # Get the names of all data accesible by non-standard evaluation
#' all_names(ex_taxmap)
#'
#' # Dont include the names of automatically included functions.
#' all_names(ex_taxmap, builtin_funcs = FALSE)
#'
#' @family accessors
#'
#' @name all_names
NULL


#' Get names of data used in expressions
#'
#' Get names of available data used in expressions.
#' Expressions are not evaluated and do not need to make sense.
#' \preformatted{
#' obj$names_used(...)
#' names_used(obj,...)}
#'
#' @param obj a [taxonomy()] or [taxmap()] object
#' @param ... One or more expressions
#'
#' @return Named `character`
#'
#' @examples
#' taxa:::names_used(ex_taxmap, n_legs + dangerous == invalid_expression)
#'
#' @family accessors
#'
#' @name names_used
#' @keywords internal
NULL


#' Get data in a taxmap object by name
#'
#' Given a vector of names, return a list of data (usually lists/vectors)
#' contained in a [taxonomy()] or [taxmap()] object. Each item will be named by
#' taxon ids when possible.
#' \preformatted{
#' obj$get_data(name = NULL, ...)
#' get_data(obj, name = NULL, ...)}
#'
#' @param obj A [taxonomy()] or [taxmap()]  object
#' @param name (`character`) Names of data to return. If not supplied, return
#'   all data listed in [all_names()].
#' @param ... Passed to [all_names()]. Used to filter what kind of data is
#'   returned (e.g. columns in tables or function output?) if `name` is not
#'   supplied or what kinds are allowed if `name` is supplied.
#'
#' @return `list` of vectors or lists. Each vector or list will be named by
#'   associated taxon ids if possible.
#'
#' @examples
#' # Get specific values
#' get_data(ex_taxmap, c("reaction", "n_legs", "taxon_ranks"))
#'
#' # Get all values
#' get_data(ex_taxmap)
#'
#' @family accessors
#'
#' @name get_data
NULL


#' Get data in a taxonomy or taxmap object by name
#'
#' Given a vector of names, return a list of data (usually lists/vectors)
#' contained in a [taxonomy()] or [taxmap()] object. Each item will be named by
#' taxon ids when possible.
#' \preformatted{
#' obj$get_data_frame(name = NULL, ...)
#' get_data_frame(obj, name = NULL, ...)}
#'
#' @param obj A [taxonomy()] or [taxmap()]  object
#' @param name (`character`) Names of data to return. If not supplied, return
#'   all data listed in [all_names()].
#' @param ... Passed to [all_names()]. Used to filter what kind of data is
#'   returned (e.g. columns in tables or function output?) if `name` is not
#'   supplied or what kinds are allowed if `name` is supplied.
#'
#' @return `data.frame`
#'
#' @examples
#' # Get specific values
#' get_data_frame(ex_taxonomy, c("taxon_names", "taxon_indexes", "is_stem"))
#'
#' # Get all values
#' get_data_frame(ex_taxonomy)
#'
#' @family accessors
#'
#' @name get_data_frame
NULL


#' Get values of data used in expressions
#'
#' Get values available for
#' [non-standard evaluation](http://adv-r.had.co.nz/Computing-on-the-language.html)
#' in a [taxonomy()] or [taxmap()] object used in expressions. Expressions are
#' not evaluated and do not need to make sense.
#' \preformatted{
#' obj$data_used(...)
#' data_used(obj, ...)}
#'
#' @param obj a [taxonomy()] or [taxmap()] object
#' @param ... One or more expressions
#'
#' @return `list`
#'
#' @examples
#' taxa:::data_used(ex_taxmap, n_legs + dangerous == invalid_expression)
#'
#' @family accessors
#'
#' @name data_used
#' @keywords internal
NULL


#' Filter taxa with a list of conditions
#'
#' Filter taxa in a [taxonomy()] or [taxmap()] object with a series of
#' conditions. Any variable name that appears in [all_names()] can be used
#' as if it was a vector on its own. See [dplyr::filter()] for the inspiration
#' for this function and more information. Calling the function using the
#' `obj$filter_taxa(...)` style edits "obj" in place, unlike most R functions.
#' However, calling the function using the `filter_taxa(obj, ...)` imitates R's
#' traditional copy-on-modify semantics, so "obj" would not be changed; instead
#' a changed version would be returned, like most R functions.
#' \preformatted{
#' filter_taxa(obj, ..., subtaxa = FALSE, supertaxa = FALSE,
#'   drop_obs = TRUE, reassign_obs = TRUE, reassign_taxa = TRUE,
#'   invert = FALSE)
#' obj$filter_taxa(..., subtaxa = FALSE, supertaxa = FALSE,
#'   drop_obs = TRUE, reassign_obs = TRUE, reassign_taxa = TRUE,
#'   invert = FALSE)}
#'
#' @param obj An object of class [taxonomy()] or [taxmap()]
#' @param ... One or more filtering conditions. Any variable name that appears
#'   in [all_names()] can be used as if it was a vector on its own. Each
#'   filtering condition must resolve to one of three things:
#'   * `character`: One or more taxon IDs contained in `obj$edge_list$to`
#'   * `integer`: One or more row indexes of `obj$edge_list`
#'   * `logical`: A `TRUE`/`FALSE` vector of length equal to the number of rows
#'   in `obj$edge_list`
#' @param subtaxa (`logical` or `numeric` of length 1) If `TRUE`, include
#'   subtaxa of taxa passing the filter. Positive numbers indicate the number of
#'   ranks below the target taxa to return. `0` is equivalent to `FALSE`.
#'   Negative numbers are equivalent to `TRUE`.
#' @param supertaxa (`logical`  or `numeric` of length 1) If `TRUE`, include
#'   supertaxa of taxa passing the filter. Positive numbers indicate the number
#'   of ranks above the target taxa to return. `0` is equivalent to `FALSE`.
#'   Negative numbers are equivalent to `TRUE`.
#' @param drop_obs (`logical`)  This option only applies to [taxmap()] objects.
#'   If `FALSE`, include observations even if the taxon they are assigned to is
#'   filtered out. Observations assigned to removed taxa will be assigned to
#'   \code{NA}. This option can be either simply `TRUE`/`FALSE`, meaning that
#'   all data sets will be treated the same, or a logical vector can be supplied
#'   with names corresponding one or more data sets in `obj$data`. For example,
#'   `c(abundance = FALSE, stats = TRUE)` would include observations whose taxon
#'   was filtered out in `obj$data$abundance`, but not in `obj$data$stats`. See
#'   the `reassign_obs` option below for further complications.
#' @param reassign_obs (`logical` of length 1) This option only applies to
#'   [taxmap()] objects. If `TRUE`, observations assigned to removed taxa will
#'   be reassigned to the closest supertaxon that passed the filter. If there
#'   are no supertaxa of such an observation that passed the filter, they will
#'   be filtered out if `drop_obs` is `TRUE`. This option can be either simply
#'   `TRUE`/`FALSE`, meaning that all data sets will be treated the same, or a
#'   logical vector can be supplied with names corresponding one or more data
#'   sets in `obj$data`. For example, `c(abundance = TRUE, stats = FALSE)` would
#'   reassign observations in `obj$data$abundance`, but not in `obj$data$stats`.
#' @param reassign_taxa (`logical` of length 1) If `TRUE`, subtaxa of removed
#'   taxa will be reassigned to the closest supertaxon that passed the filter.
#'   This is useful for removing intermediate levels of a taxonomy.
#' @param invert (`logical` of length 1) If `TRUE`, do NOT include the
#'   selection. This is different than just replacing a `==` with a `!=` because
#'   this option negates the selection after taking into account the `subtaxa`
#'   and `supertaxa` options. This is useful for removing a taxon and all its
#'   subtaxa for example.
#'
#' @return An object of type [taxonomy()] or [taxmap()]
#'
#' @examples
#' # Filter by index
#' filter_taxa(ex_taxmap, 1:3)
#'
#' # Filter by taxon ID
#' filter_taxa(ex_taxmap, c("b", "c", "d"))
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
#' filter_taxa(ex_taxmap, 12, supertaxa = 2)
#'
#' # Include subtaxa
#' filter_taxa(ex_taxmap, 1, subtaxa = TRUE)
#' filter_taxa(ex_taxmap, 1, subtaxa = 2)
#'
#' # Dont remove rows in data corresponding to removed taxa
#' filter_taxa(ex_taxmap, 2, drop_obs = c(info = FALSE))
#'
#' # Remove a taxon and it subtaxa
#' filter_taxa(ex_taxmap, 1, subtaxa = TRUE, invert = TRUE)
#'
#' @family taxmap manipulation functions
#'
#' @name filter_taxa
NULL


#' Sort the edge list of [taxmap()] objects
#'
#' Sort the edge list and taxon list in [taxonomy()] or [taxmap()] objects. See
#' [dplyr::arrange()] for the inspiration for this function and more
#' information.
#' \preformatted{
#' obj$arrange_taxa(...)
#' arrange_taxa(obj, ...)}
#'
#' @param obj [taxonomy()] or [taxmap()]
#' @param ... One or more expressions (e.g. column names) to sort on. Any
#'   variable name that appears in [all_names()] can be used as if it was a
#'   vector on its own.
#'
#' @return An object of type [taxonomy()] or [taxmap()]
#'
#' @examples
#' # Sort taxa in ascending order
#' arrange_taxa(ex_taxmap, taxon_names)
#'
#' # Sort taxa in decending order
#' arrange_taxa(ex_taxmap, desc(taxon_names))
#'
#' # Sort using an expression. List genera first.
#' arrange_taxa(ex_taxmap, taxon_ranks != "genus")
#'
#' @family taxmap manipulation functions
#'
#' @name arrange_taxa
NULL

#' Sample n taxa from [taxonomy()] or [taxmap()]
#'
#' Randomly sample some number of taxa from a [taxonomy()] or [taxmap()] object.
#' Weights can be specified for taxa or the observations assigned to them. See
#' [dplyr::sample_n()] for the inspiration for this function.
#' \preformatted{
#' obj$sample_n_taxa(size, taxon_weight = NULL,
#'   obs_weight = NULL, obs_target = NULL,
#'   use_subtaxa = TRUE, collapse_func = mean, ...)
#' sample_n_taxa(obj, size, taxon_weight = NULL,
#'   obs_weight = NULL, obs_target = NULL,
#'   use_subtaxa = TRUE, collapse_func = mean, ...)}
#'
#' @param obj ([taxonomy()] or [taxmap()]) The object to sample from.
#' @param size (`numeric` of length 1) The number of taxa to sample.
#' @param taxon_weight (`numeric`) Non-negative sampling weights of each
#'   taxon. If `obs_weight` is also specified, the two weights are
#' multiplied (after `obs_weight` for each taxon is calculated).
#' @param obs_weight (`numeric`)  This option only applies to [taxmap()]
#'   objects. Sampling weights of each observation. The weights for each
#'   observation assigned to a given taxon are supplied to `collapse_func` to
#'   get the taxon weight. If `use_subtaxa` is `TRUE` then the observations
#'   assigned to every subtaxa are also used. Any variable name that appears in
#'   [all_names()] can be used as if it was a vector on its own. If
#'   `taxon_weight` is also specified, the two weights are multiplied (after
#'   `obs_weight` for each observation is calculated). `obs_target` must be used
#'   with this option.
#' @param obs_target (`character` of length 1)  This option only applies to
#'   [taxmap()] objects. The name of the data set in `obj$data` that values in
#'   `obs_weight` corresponds to. Must be used when `obs_weight` is used.
#' @param use_subtaxa (`logical` or `numeric` of length 1) Affects how the
#'   `obs_weight` option is used. If `TRUE`, the weights for each taxon in an
#'   observation's classification are multiplied to get the observation weight.
#'   If `FALSE` just the taxonomic level the observation is assign to it
#'   considered. Positive numbers indicate the number of ranks below the each
#'   taxon to use. `0` is equivalent to `FALSE`. Negative numbers are equivalent
#'   to `TRUE`.
#' @param collapse_func (`function` of length 1) If `taxon_weight` is used and
#'   `supertaxa` is `TRUE`, the weights for each taxon in an observation's
#'   classification are supplied to `collapse_func` to get the observation
#'   weight. This function should take  numeric vector and return a single
#'   number.
#' @param ... Additional options are passed to [filter_taxa()].
#'
#' @return An object of type [taxonomy()] or [taxmap()]
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
#' sample_n_taxa(ex_taxmap, 3, supertaxa = TRUE,
#'               obs_weight = n_legs, obs_target = "info")
#'
#' @family taxmap manipulation functions
#'
#' @name sample_n_taxa
NULL


#' Sample a proportion of taxa from [taxonomy()] or [taxmap()]
#'
#' Randomly sample some proportion of taxa from a [taxonomy()] or [taxmap()]
#' object. Weights can be specified for taxa or the observations assigned to
#' them. See
#' [dplyr::sample_frac()] for the inspiration for this function.
#' \preformatted{
#' obj$sample_frac_taxa(size, taxon_weight = NULL,
#'   obs_weight = NULL, obs_target = NULL,
#'   use_subtaxa = TRUE, collapse_func = mean, ...)
#' sample_frac_taxa(obj, size, taxon_weight = NULL,
#'   obs_weight = NULL, obs_target = NULL,
#'   use_subtaxa = TRUE, collapse_func = mean, ...)}
#'
#' @param obj ([taxonomy()] or [taxmap()]) The object to sample from.
#' @param size (`numeric` of length 1) The proportion of taxa to sample.
#' @param taxon_weight (`numeric`) Non-negative sampling weights of each
#'   taxon. If `obs_weight` is also specified, the two weights are
#' multiplied (after `obs_weight` for each taxon is calculated).
#' @param obs_weight (`numeric`)  This option only applies to [taxmap()]
#'   objects. Sampling weights of each observation. The weights for each
#'   observation assigned to a given taxon are supplied to `collapse_func` to
#'   get the taxon weight. If `use_subtaxa` is `TRUE` then the observations
#'   assigned to every subtaxa are also used. Any variable name that appears in
#'   [all_names()] can be used as if it was a vector on its own. If
#'   `taxon_weight` is also specified, the two weights are multiplied (after
#'   `obs_weight` for each observation is calculated). `obs_target` must be used
#'   with this option.
#' @param obs_target (`character` of length 1)  This option only applies to
#'   [taxmap()] objects. The name of the data set in `obj$data` that values in
#'   `obs_weight` corresponds to. Must be used when `obs_weight` is used.
#' @param use_subtaxa (`logical` or `numeric` of length 1) Affects how the
#'   `obs_weight` option is used. If `TRUE`, the weights for each taxon in an
#'   observation's classification are multiplied to get the observation weight.
#'   If `TRUE` just the taxonomic level the observation is assign to it
#'   considered. Positive numbers indicate the number of ranks below the target
#'   taxa to return. `0` is equivalent to `FALSE`. Negative numbers are
#'   equivalent to `TRUE`.
#' @param collapse_func (`function` of length 1) If `taxon_weight` is
#'   used and `supertaxa` is `TRUE`, the weights for each taxon in an
#'   observation's classification are supplied to `collapse_func` to get
#'   the observation weight. This function should take  numeric vector and
#'   return a single number.
#' @param ... Additional options are passed to [filter_taxa()].
#'
#' @return An object of type [taxonomy()] or [taxmap()]
#'
#'
#' @examples
#' # sample half of the taxa
#' sample_frac_taxa(ex_taxmap, 0.5, supertaxa = TRUE)
#'
#' @family taxmap manipulation functions
#'
#' @name sample_frac_taxa
NULL


#' Test if taxa are roots
#'
#' Test if taxa are roots in a [taxonomy()] or [taxmap()] object. Roots are taxa
#' without supertaxa, typically things like "Bacteria", or "Life".
#' \preformatted{
#' obj$is_root()
#' is_root(obj)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#'
#' @return A `logical` of length equal to the number of taxa.
#'
#' @family taxonomy data functions
#'
#' @examples
#' is_root(ex_taxmap)
#'
#' @name is_root
NULL


#' Test if taxa are "internodes"
#'
#' Test if taxa are "internodes" in a [taxonomy()] or [taxmap()] object.  An
#' internode is any taxon with a single immediate supertaxon and a single immediate
#' subtaxon. They can be removed from a tree without any loss of information on
#' the relative relationship between remaining taxa.
#' \preformatted{
#' obj$is_internode()
#' is_internode(obj)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#'
#' @return A `logical` of length equal to the number of taxa.
#'
#' @family taxonomy data functions
#'
#' @examples
#' is_internode(ex_taxmap)
#'
#' @name is_internode
NULL


#' Test if taxa are stems
#'
#' Test if taxa are stems in a [taxonomy()] or [taxmap()] object. Stems are taxa
#' from the [roots()] taxa to the first taxon with more than one subtaxon. These
#' can usually be filtered out of the taxonomy without removing any information
#' on how the remaining taxa are related.
#' \preformatted{
#' obj$is_stem()
#' is_stem(obj)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#'
#' @return A `logical` of length equal to the number of taxa.
#'
#' @family taxonomy data functions
#'
#' @examples
#' is_stem(ex_taxmap)
#'
#' @name is_stem
NULL


#' Test if taxa are branches
#'
#' Test if taxa are branches in a [taxonomy()] or [taxmap()] object. Branches
#' are taxa in the interior of the tree that are not [roots()], [stems()], or
#' [leaves()].
#' \preformatted{
#' obj$is_branch()
#' is_branch(obj)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#'
#' @return A `logical` of length equal to the number of taxa.
#'
#' @family taxonomy data functions
#'
#' @examples
#' is_branch(ex_taxmap)
#'
#' @name is_branch
NULL


#' Test if taxa are leaves
#'
#' Test if taxa are leaves in a [taxonomy()] or [taxmap()] object. Leaves are taxa
#' without subtaxa, typically species.
#' \preformatted{
#' obj$is_leaf()
#' is_leaf(obj)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#'
#' @return A `logical` of length equal to the number of taxa.
#'
#' @family taxonomy data functions
#'
#' @examples
#' is_leaf(ex_taxmap)
#'
#' @name is_leaf
NULL


#' Create a mapping between two variables
#'
#' Creates a named vector that maps the values of two variables associated with
#' taxa in a [taxonomy()] or [taxmap()] object. Both values must be named by
#' taxon ids.
#' \preformatted{
#' obj$map_data(from, to, warn = TRUE)
#' map_data(obj, from, to, warn = TRUE)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#' @param from The value used to name the output. There will be one output value
#'   for each value in `from`. Any variable that appears in [all_names()] can be
#'   used as if it was a variable on its own.
#' @param to The value returned in the output. Any variable that appears in
#'   [all_names()] can be used as if it was a variable on its own.
#' @param warn If `TRUE`, issue a warning if there are multiple unique values of
#'   `to` for each value of `from`.
#'
#' @return A vector of `to` values named by values in `from`.
#'
#' @family taxonomy data functions
#'
#' @examples
#' # Mapping between two variables in `all_names(ex_taxmap)`
#' map_data(ex_taxmap, from = taxon_names, to = n_legs > 0)
#'
#' # Mapping with external variables
#' x = c("d" = "looks like a cat", "h" = "big scary cats",
#'       "i" = "smaller cats", "m" = "might eat you", "n" = "Meow! (Feed me!)")
#' map_data(ex_taxmap, from = taxon_names, to = x)
#'
#' @name map_data
NULL


#' Create a mapping without NSE
#'
#' Creates a named vector that maps the values of two variables associated with
#' taxa in a [taxonomy()] or [taxmap()] object without using Non-Standard
#' Evaluation (NSE). Both values must be named by taxon ids. This is the same as
#' [map_data()] without NSE and can be useful in some odd cases where NSE fails
#' to work as expected.
#' \preformatted{
#' obj$map_data(from, to)
#' map_data(obj, from, to)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#' @param from The value used to name the output. There will be one output value
#'   for each value in `from`.
#' @param to The value returned in the output.
#'
#' @return A vector of `to` values named by values in `from`.
#'
#' @family taxonomy data functions
#'
#' @examples
#' x = c("d" = "looks like a cat", "h" = "big scary cats",
#'       "i" = "smaller cats", "m" = "might eat you", "n" = "Meow! (Feed me!)")
#' map_data_(ex_taxmap, from = ex_taxmap$taxon_names(), to = x)
#'
#' @name map_data_
NULL


#' Replace taxon ids
#'
#' Replace taxon ids in a [taxmap()] or [taxonomy()] object.
#' \preformatted{
#' obj$replace_taxon_ids(new_ids)
#' replace_taxon_ids(obj, new_ids)}
#'
#' @param obj The [taxonomy()] or [taxmap()] object.
#' @param new_ids A vector of new ids, one per taxon. They must be unique and in
#'   the same order as the corresponding ids in `obj$taxon_ids()`.
#'
#' @return A [taxonomy()] or [taxmap()] object with new taxon ids
#' @name replace_taxon_ids
#' @examples \dontrun{
#'
#' replace_taxon_ids(ex_taxmap, seq_len(length(ex_taxmap$taxa)))
#' }
NULL


#' Remove redundant parts of taxon names
#'
#' Remove the names of parent taxa in the begining of their children's names in a \code{taxonomy} or \code{taxmap} object.
#' This is useful for removing genus names in species binomials.
#' \preformatted{
#' obj$remove_redundant_names()
#' remove_redundant_names(obj)}
#'
#' @param obj A \code{taxonomy} or \code{taxmap} object
#'
#' @return A \code{taxonomy} or \code{taxmap} object
#' @name remove_redundant_names
NULL
