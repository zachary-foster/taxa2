#' Get taxon IDs
#'
#' Return the taxon IDs in a [taxonomy()] or [taxmap()] object.
#' They are in the order they appear in the edge list.
#'
#' \preformatted{
#' obj$taxon_ids()
#' taxon_ids(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @family taxonomy data functions
#'
#' @name taxon_ids
NULL

#' Get taxon indexes
#'
#' Return the taxon indexes in a [taxonomy()] or [taxmap()] object.
#' They are the indexes of the edge list rows.
#'
#' \preformatted{
#' obj$taxon_indexes()
#' taxon_indexes(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @family taxonomy data functions
#'
#' @name taxon_indexes
NULL


#' Get taxon names
#'
#' Return the taxon names in a [taxonomy()] or [taxmap()] object.
#' They are in the order they appear in the edge list.
#'
#' \preformatted{
#' obj$taxon_names()
#' taxon_names(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @family taxonomy data functions
#'
#' @name taxon_names
NULL

#' Get taxon ranks
#'
#' Return the taxon ranks in a [taxonomy()] or [taxmap()] object.
#' They are in the order taxa appear in the edge list.
#'
#' \preformatted{
#' obj$taxon_ranks()
#' taxon_ranks(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @family taxonomy data functions
#'
#' @name taxon_ranks
NULL



#' Get all supertaxa of a taxon
#'
#' Return the taxon IDs or indexes of all supertaxa (i.e. all taxa the target
#' taxa are a part of) in an object of type [taxonomy()] or
#' [taxmap()]. \preformatted{ obj$supertaxa(subset = NULL, recursive =
#' TRUE, simplify = FALSE, include_input = FALSE, value = NULL, na =
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
#'   recursions (i.e. number of ranks above the target taxon to return). `1` is
#'   equivalent to `FALSE`. Negative numbers are equivalent to `TRUE`.
#' @param simplify (`logical`) If `TRUE`, then combine all the results into a
#'   single vector of unique values.
#' @param include_input (`logical`) If `TRUE`, the input taxa are included in
#'   the output
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of `all_names(obj)` can be used, but it
#'   usually only makes sense to data that corresponds to taxa 1:1, such as
#'   [taxon_ranks()]. By default, taxon indexes are returned.
#' @param na (`logical`) If `TRUE`, return `NA` where information
#'   is not available.
#' @param ... U Used by the S3 method to pass the parameters to the R6 method of
#'   [taxonomy()]
#'
#' @return If `simplify = FALSE`, then a list of vectors are returned
#'   corresponding to the `subset` argument. If `simplify = TRUE`,
#'   then unique values are returned in a single vector.
#'
#' @family taxonomy indexing functions
#'
#' @name supertaxa
NULL


#' Get root taxa
#'
#' Return the root taxa for a [taxonomy()] or [taxmap()] object. Can also be used to
#' get the roots of a subset of taxa. \preformatted{ obj$roots(subset = NULL,
#' value = NULL) roots(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object containing taxon
#'   information to be queried.
#' @param subset (`character`) Taxon IDs for which root taxa will be
#'   returned. Default: All taxon in `obj` will be used.
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of `all_names(obj)` can be used, but it
#'   usually only makes sense to data that corresponds to taxa 1:1, such as
#'   [taxon_ranks()]. By default, taxon indexes are returned.
#' @param ... Used by the S3 method to pass the parameters to the R6 method of
#'   [taxonomy()]
#'
#' @family taxonomy indexing functions
#'
#' @return `character`
#'
#' @name roots
NULL


#' Get subtaxa
#'
#' Return the taxon IDs or `taxon_data` indexes of all subtaxa in an object
#' of type `taxmap` \preformatted{ obj$subtaxa(subset = NULL, recursive =
#' TRUE, simplify = FALSE, include_input = FALSE, value = NULL, na =
#' FALSE) subtaxa(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object containing taxon
#'   information to be queried.
#' @param subset (`character`) `taxon_ids` or indexes of
#'   `taxon_data` for which supertaxa will be returned. Default: All taxa
#'   in `obj` will be used.
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
#'   table in `obj$data`. Any result of `all_names(obj)` can be used, but it
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
NULL


#' Get stem taxa
#'
#' Return the stem taxa for a [taxonomy()] or a [taxmap()]
#' object. Stem taxa are all those from the roots to the first taxon with more
#' than one subtaxon. \preformatted{ obj$stems(subset = NULL, simplify = FALSE,
#' value = NULL, exclude_leaves = FALSE) stems(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object containing taxon
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
#' @param ... Used by the S3 method to pass the parameters to the R6 method of
#'   [taxonomy()]
#'
#' @return `character`
#'
#' @family taxonomy indexing functions
#'
#' @name stems
NULL


#' Get leaf taxa
#'
#' Return the leaf taxa for a [taxonomy()] or [taxmap()] object. Leaf taxa are taxa
#' with no subtaxa. \preformatted{ obj$leaves(subset = NULL, value = NULL)
#' leaves(obj, ...)}
#'
#' @param obj The `taxonomy` or `taxmap` object containing taxon
#'   information to be queried.
#' @param subset (`character`) Taxon IDs for which leaf taxa will be
#'   returned. Default: All taxon in `obj` will be used.
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of `all_names(obj)` can be used, but it
#'   usually only makes sense to data that corresponds to taxa 1:1, such as
#'   [taxon_ranks()]. By default, taxon indexes are returned.
#' @param ... Used by the S3 method to pass the parameters to the R6 method of
#'   [taxonomy()]
#'
#' @return `character`
#'
#' @family taxonomy indexing functions
#'
#' @name leaves
NULL


#' Get classifications of taxa
#'
#' Get classification strings of taxa in an object of type [taxonomy()] or [taxmap()]
#' composed of taxon IDs. Each classification is constructed by concatenating
#' the taxon ids of the given taxon and its supertaxa. \preformatted{
#' obj$id_classifications(sep = ";") id_classifications(obj, sep = ";")}
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


#' Get classifications of taxa
#'
#' Get classification strings of taxa in an object of type [taxonomy()] or [taxmap()]
#' composed of taxon names. Each classification is constructed by concatenating
#' the taxon names of the given taxon and its supertaxa. \preformatted{
#' obj$name_classifications(sep = ";") name_classifications(obj, sep = ";")}
#'
#' @param obj ([taxonomy()] or [taxmap()])
#' @param sep (`character` of length 1) The character(s) to place between
#'   taxon names
#'
#' @return `character`
#'
#' @examples
#' name_classifications(ex_taxmap)
#'
#' @family taxonomy data functions
#'
#' @name name_classifications
NULL


#' Get number of supertaxa
#'
#' Get number of supertaxa for each taxon in an object of type
#' [taxonomy()] or [taxmap()] \preformatted{ obj$n_supertaxa() n_supertaxa(obj)}
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


#' Get number of subtaxa
#'
#' Get number of subtaxa for each taxon in an object of type
#' [taxonomy()] or [taxmap()] \preformatted{ obj$n_subtaxa() n_subtaxa(obj)}
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
#' include subtaxa assigned to subtaxa. \preformatted{ obj$n_subtaxa_1()
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


#' Return names of data in a [taxonomy()] or [taxmap()]
#'
#' Return all the valid names that can be used with non-standard evalulation in
#' manipulation functions like `filter_taxa`.
#' \preformatted{
#' obj$all_names(tables = TRUE, funcs = TRUE, others = TRUE, warn = FALSE)
#' all_names(obj, tables = TRUE, funcs = TRUE, others = TRUE, warn = FALSE)}
#'
#' @param obj ([taxonomy()] or [taxmap()]) The object containing
#'   taxon information to be queried.
#' @param tables This option only applies to [taxmap()] objects. If `TRUE`,
#'   include the names of columns of tables in `obj$data`
#' @param funcs This option only applies to [taxmap()] objects. If `TRUE`,
#'   include the names of user-definable functionsin `obj$funcs`.
#' @param others This option only applies to [taxmap()] objects. If `TRUE`,
#'   include the names of data in `obj$data` besides tables.
#' @param builtin_funcs This option only applies to [taxmap()] objects. If
#'   `TRUE`, include functions like [n_supertaxa()] that provide information for
#'   each taxon.
#' @param warn This option only applies to [taxmap()] objects. If `TRUE`, warn
#'   if there are duplicate names.
#'
#' @return `character`
#'
#' @examples
#' # Get the names of all data accesible by non-standard evaluation
#' ex_taxmap$all_names()
#'
#' # Dont include the names of functions
#' ex_taxmap$all_names(funcs = FALSE)
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
#' ex_taxmap$names_used(n_legs + dangerous == invalid_expression)
#'
#' @family accessors
#'
#' @name names_used
NULL


#' Get data in a taxmap object by name
#'
#' Given a vector of names, return a list of data contained in a [taxonomy()] or
#' [taxmap()] object. \preformatted{ obj$get_data(name) get_data(obj, name)}
#'
#' @param obj A [taxonomy()] or [taxmap()]  object
#' @param name (`character`) Names of data to return.
#' @param ... Passed to [all_names()]. Used to filter what kind of data is
#'   returned (e.g. columns in tables or function output?) if `name` is not
#'   supplied or what kinds are allowed if `name` is supplied.
#'
#' @return `list`
#'
#' @examples
#' ex_taxmap$get_data("reaction")
#'
#' @family accessors
#'
#' @name get_data
NULL


#' Get values of data used in expressions
#'
#' Get values of data in a [taxonomy()] or [taxmap()] used in expressions.
#' Expressions are not evaluated and do not need to make sense.
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
#' ex_taxmap$data_used(n_legs + dangerous == invalid_expression)
#'
#' @family accessors
#'
#' @name data_used
NULL


#' Filter taxa with a list of conditions
#'
#' Filter taxa in a [taxonomy()] or [taxmap()] object with a series of
#' conditions. Any variable name that appears in `obj$all_names()` can be used
#' as if it was a vector on its own. See [dplyr::filter()] for the inspiration
#' for this function and more information. Calling the function using the
#' `obj$filter_taxa(...)` style edits "obj" in place, unlike most R functions.
#' However, calling the function using the `filter_taxa(obj, ...)` immitates R's
#' traditional copy-on-modify semantics, so "obj" would not be changed; instead
#' a changed version would be returned, like most R functions.
#' \preformatted{filter_taxa(obj, ..., subtaxa = FALSE, supertaxa = FALSE,
#' taxonless = FALSE, reassign_obs = TRUE, reassign_taxa = TRUE, invert = FALSE)
#' obj$filter_taxa(..., subtaxa = FALSE, supertaxa = FALSE, taxonless =
#' FALSE, reassign_obs = TRUE, reassign_taxa = TRUE, invert = FALSE)}
#'
#' @param obj An object of class [taxonomy()] or [taxmap()]
#' @param ... One or more filtering conditions. Each filtering condition must
#'   resolve to one of three things: \describe{ \item{`character`}{One or more
#'   taxon IDs contained in `obj$edge_list$to`} \item{`integer`}{One or more row
#'   indexes of `obj$edge_list`} \item{`logical`}{A `TRUE`/`FALSE` vector of
#'   length equal to the number of rows in `obj$edge_list`} } Any variable name
#'   that appears in `obj$all_names()` can be used as if it was a vector on its
#'   own.
#' @param subtaxa (`logical` or `numeric` of length 1) If `TRUE`, include
#'   subtaxa of taxa passing the filter. Positive numbers indicate the number of
#'   ranks below the target taxa to return. `0` is equivalent to `FALSE`.
#'   Negative numbers are equivalent to `TRUE`.
#' @param supertaxa (`logical`  or `numeric` of length 1) If `TRUE`, include
#'   supertaxa of taxa passing the filter. Positive numbers indicate the number
#'   of ranks above the target taxa to return. `0` is equivalent to `FALSE`.
#'   Negative numbers are equivalent to `TRUE`.
#' @param taxonless (`logical`)  This option only applies to [taxmap()] objects.
#'   If `TRUE`, include observations even if the taxon they are assigned to is
#'   filtered out. Observations assigned to removed taxa will be assigned to
#'   \code{NA}. This option can be either simply `TRUE`/`FALSE`, meaning that
#'   all data sets will be treated the same, or a logical vector can be supplied
#'   with names corresponding one or more data sets in `obj$data`. For example,
#'   `c(abundance = TRUE, stats = FALSE)` would inlcude observations whose taxon
#'   was filtered out in `obj$data$abundance`, but not in `obj$data$stats`. See
#'   the `reassign_obs` option below for further complications.
#' @param reassign_obs (`logical` of length 1) This option only applies to
#'   [taxmap()] objects. If `TRUE`, observations assigned to removed taxa will
#'   be reassigned to the closest supertaxon that passed the filter. If there
#'   are no supertaxa of such an observation that passed the filter, they will
#'   be filtered out if `taxonless` is `TRUE`. This option can be either simply
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
#' filter_taxa(ex_taxmap, c("1", "2", "3"))
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
#' # Remove rows in data corresponding to removed taxa
#' filter_taxa(ex_taxmap, 2, taxonless = c(info = FALSE))
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
#' Sort the edge list in [taxonomy()] or [taxmap()] objects. Any variable name that
#' appears in `obj$all_names()` can be used as if it was a vector on its
#' own. See [dplyr::arrange()] for the inspiration for this function
#' and more information.
#' \preformatted{
#' obj$arrange_taxa(...)
#' arrange_taxa(obj, ...)}
#'
#' @param obj [taxonomy()] or [taxmap()]
#' @param ... One or more column names to sort on.
#'
#' @return An object of type [taxonomy()] or [taxmap()]
#'
#' @examples
#' arrange_taxa(ex_taxmap, desc(ex_taxmap$taxon_names()))
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
#' obj$sample_n_taxa(size, taxon_weight = NULL, obs_weight = NULL,
#' obs_target = NULL, use_subtaxa = TRUE, collapse_func = mean, ...)
#' sample_n_taxa(obj, size, taxon_weight = NULL, obs_weight = NULL,
#' obs_target = NULL, use_subtaxa = TRUE, collapse_func = mean, ...)}
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
#'   `obj$all_names()` can be used as if it was a vector on its own. If
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
#' obj$sample_frac_taxa(size, taxon_weight = NULL, obs_weight = NULL,
#' obs_target = NULL, use_subtaxa = TRUE, collapse_func = mean, ...)
#' sample_frac_taxa(obj, size, taxon_weight = NULL, obs_weight = NULL,
#' obs_target = NULL, use_subtaxa = TRUE, collapse_func = mean, ...)}
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
#'   `obj$all_names()` can be used as if it was a vector on its own. If
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
#' \preformatted{obj$is_root()
#' is_root(obj)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @return A `logical` of length equal to the number of taxa.
#'
#' @family taxonomy data functions
#'
#' @name is_root
NULL


#' Test if taxa are stems
#'
#' Test if taxa are stems in a [taxonomy()] or [taxmap()] object. Stems are taxa
#' from the [roots()] taxa to the first taxon with more than one subtaxon. These
#' can usually be filtered out of the taxonomy without removing any information
#' on how the reminaing taxa are related.
#' \preformatted{obj$is_stem()
#' is_stem(obj)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @return A `logical` of length equal to the number of taxa.
#'
#' @family taxonomy data functions
#'
#' @name is_stem
NULL


#' Test if taxa are branches
#'
#' Test if taxa are branches in a [taxonomy()] or [taxmap()] object. Branches
#' are taxa in the interior of the tree that are not [roots()], [stems()], or
#' [leaves()].
#' \preformatted{obj$is_branch()
#' is_branch(obj)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @return A `logical` of length equal to the number of taxa.
#'
#' @family taxonomy data functions
#'
#' @name is_branch
NULL


#' Test if taxa are leaves
#'
#' Test if taxa are leaves in a [taxonomy()] or [taxmap()] object. Leaves are taxa
#' without subtaxa, typically species.
#' \preformatted{obj$is_leaf()
#' is_leaf(obj)}
#'
#' @param obj The `taxonomy` or `taxmap` object.
#'
#' @return A `logical` of length equal to the number of taxa.
#'
#' @family taxonomy data functions
#'
#' @name is_leaf
NULL

