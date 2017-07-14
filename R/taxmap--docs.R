#' Get data indexes associated with taxa
#'
#' Given a [taxmap()] object, return data associated with each taxon in a
#' given table included in that [taxmap()] object.
#' \preformatted{obj$obs(data, value = NULL, subset = NULL, recursive = TRUE, simplify = FALSE)
#' obs(obj, data, value = NULL, subset = NULL, recursive = TRUE, simplify = FALSE)}
#'
#' @param obj ([taxmap()]) The [taxmap()] object containing taxon information to
#'   be queried.
#' @param data Either the name of something in `obj$data` that has taxon
#'   information or a an external object with taxon information. For tables,
#'   there must be a column named "taxon_id" and lists/vectors must be named by
#'   taxon ID.
#' @param value What data to return. This is usually the name of column in a
#'   table in `obj$data`. Any result of `all_names(obj)` can be used. If the
#'   value used has names, it is assumed that the names are taxon ids and the
#'   taxon ids are used to look up the correct values.
#' @param subset (`character`) Taxon IDs or indexes for which observation
#'   indexes will be returned. Default: All taxa in `obj` will be used.
#' @param recursive (`logical` or `numeric`) If `FALSE`, only return the
#'   observation assigned to the specified input taxa, not subtaxa. If `TRUE`,
#'   return all the observations of every subtaxa, etc. Positive numbers
#'   indicate the number of ranks below the each taxon to get observations for
#'   `0` is equivalent to `FALSE`. Negative numbers are equivalent to `TRUE`.
#' @param simplify (`logical`) If `TRUE`, then combine all the results into a
#'   single vector of unique observation indexes.
#'
#' @return If `simplify = FALSE`, then a list of vectors of observation indexes
#'   are returned corresponding to the `target` argument. If `simplify = TRUE`,
#'   then the observation indexes for all `target` taxa are returned in a single
#'   vector.
#'
#' @name obs
#'
#' @examples
#' # Get indexes of rows corresponding to each taxon
#' obs(ex_taxmap, "info")
#'
#' # Get only a subset of taxon indexes
#' obs(ex_taxmap, "info", subset = 1:2)
#'
#' # Get only a subset of taxon IDs
#' obs(ex_taxmap, "info", subset = c("1", "2"))
#'
#' # Get only a subset of taxa using logical tests
#' obs(ex_taxmap, "info", subset = taxon_ranks == "genus")
#'
#' # Only return indexes of rows assinged to each taxon explicitly
#' obs(ex_taxmap, "info", recursive = FALSE)
#'
#' # Lump all row indexes in a single vector
#' obs(ex_taxmap, "info", simplify = TRUE)
#'
#' # Return values from a dataset instead of indexes
#' obs(ex_taxmap, "info", value = "name")
#'
NULL

#' Apply function to observations per taxon
#'
#' Apply a function to data for the observations for each taxon. This is similar
#' to using [obs()] with [lapply()] or [sapply()].
#' \preformatted{obj$obs_apply(data, func, simplify = FALSE, value = NULL,
#'               subset = NULL, recursive = TRUE, ...)
#' obs(obj, data, func, simplify = FALSE,
#'     value = NULL, subset = NULL, recursive = TRUE, ...)}
#'
#' @param obj The [taxmap()] object containing taxon information to
#'   be queried.
#' @param data Either the name of something in `obj$data` that has taxon
#'   information or a an external object with taxon information. For tables,
#'   there must be a column named "taxon_id" and lists/vectors must be named by
#'   taxon ID.
#' @param func (`function`) The function to apply.
#' @param simplify (`logical`) If `TRUE`, convert lists to vectors.
#' @param value What data to give to the function. This is usually the name of
#'   column in a table in `obj$data`. Any result of `all_names(obj)` can be
#'   used, but it usually only makes sense to use columns in the dataset
#'   specified by the `data` option. By default, the indexes of observation in
#'   `data` are returned.
#' @param subset (`character`) Taxon IDs or indexes for which observation
#'   indexes will be returned. Default: All taxa in `obj` will be used.
#' @param recursive (`logical` or `numeric`) If `FALSE`, only return the
#'   observation assigned to the specified input taxa, not subtaxa. If `TRUE`,
#'   return all the observations of every subtaxa, etc. Positive numbers
#'   indicate the number of ranks below the each taxon to get observations for
#'   `0` is equivalent to `FALSE`. Negative numbers are equivalent to `TRUE`.
#' @param ... Extra arguments are passed to the function.
#'
#' @name obs_apply
#'
#' @examples
#' # Find the average number of legs in each taxon
#' obs_apply(ex_taxmap, "info", mean, value = "n_legs", simplify = TRUE)
#'
#' # One way to implement `n_obs` and find the number of observations per taxon
#' obs_apply(ex_taxmap, "info", length, simplify = TRUE)
#'
NULL


#' Filter observations with a list of conditions
#'
#' Filter data in a [taxmap()] object (in `obj$data`) with a
#' set of conditions.  See
#' [dplyr::filter()] for the inspiration for this function and more
#' information. Calling the function using the `obj$filter_obs(...)` style
#' edits "obj" in place, unlike most R functions. However, calling the function
#' using the `filter_obs(obj, ...)` imitates R's traditional copy-on-modify
#' semantics, so "obj" would not be changed; instead a changed version would be
#' returned, like most R functions.
#' \preformatted{
#' obj$filter_obs(target, ..., drop_taxa = FALSE)
#' filter_obs(obj, target, ...)}
#'
#' @param obj An object of type [taxmap()]
#' @param target The name of the list/vector/table in `obj$data` to filter
#' @param ... One or more filtering conditions. Any variable name that appears
#'   in [all_names()] can be used as if it was a vector on its own. Each
#'   filtering condition can be one of three things:
#'   * `integer`: One or more row indexes of `obj[target]`
#'   * `logical`: A `TRUE`/`FALSE` vector of length equal to the number of
#'   rows in `obj[target]`
#' @param drop_taxa (`logical` of length 1) If `FALSE`, preserve taxa
#'   even if all of their observations are filtered out. If `TRUE`, remove
#'   taxa for which all observations were filtered out. Note that only taxa that
#'   are unobserved due to this filtering will be removed; there might be other
#'   taxa without observations to begin with that will not be removed.
#'
#' @return An object of type [taxmap()]
#'
#' @examples
#' # Filter by row index
#' filter_obs(ex_taxmap, "info", 1:2)
#'
#' # Filter by TRUE/FALSE
#' filter_obs(ex_taxmap, "info", dangerous == FALSE)
#'
#' # Remove taxa whose obserservation were filtered out
#' filter_obs(ex_taxmap, "info", dangerous == FALSE, drop_taxa = TRUE)
#'
#' @family taxmap manipulation functions
#'
#' @name filter_obs
NULL


#' Subset columns in a [taxmap()] object
#'
#' Subsets columns in a [taxmap()] object. Takes and returns a
#' [taxmap()] object. Any variable name that appears in
#' [all_names()] can be used as if it was a vector on its own. See
#' [dplyr::select()] for the inspiration for this function and more
#' information. Calling the function using the `obj$select_obs(...)` style
#' edits "obj" in place, unlike most R functions. However, calling the function
#' using the `select_obs(obj, ...)` imitates R's traditional copy-on-modify
#' semantics, so "obj" would not be changed; instead a changed version would be
#' returned, like most R functions.
#' \preformatted{
#' obj$select_obs(target, ...)
#' select_obs(obj, target, ...)}
#'
#' @param obj An object of type [taxmap()]
#' @param target The name of the list/vector/table in `obj$data` to filter
#' @param ... One or more column names to return in the new object. Each can be
#'   one of two things: \describe{ \item{expression with unquoted column
#'   name}{The name of a column in `obj$data[[target]]` typed as if it was
#'   a variable on its own.} \item{`numeric`}{Indexes of columns in
#'   `obj$data[[target]]`} } To match column names with a character vector,
#'   use `matches("my_col_name")`. To match a logical vector, convert it to
#'   a column index using `which`.
#'
#' @return An object of type [taxmap()]
#'
#' @family taxmap manipulation functions
#'
#' @examples
#' # Selecting a column by name
#' select_obs(ex_taxmap, "info", dangerous)
#'
#' # Selecting a column by index
#' select_obs(ex_taxmap, "info", 3)
#'
#' # Selecting a column by regular expressions
#' select_obs(ex_taxmap, "info", matches("^n"))
#'
#' @name select_obs
NULL


#' Add columns to [taxmap()] objects
#'
#' Add columns to tables in `obj$data` in [taxmap()] objects.  See [dplyr::mutate()] for the inspiration for
#' this function and more information. Calling the function using the
#' `obj$mutate_obs(...)` style edits "obj" in place, unlike most R
#' functions. However, calling the function using the `mutate_obs(obj,
#' ...)` imitates R's traditional copy-on-modify semantics, so "obj" would not be
#' changed; instead a changed version would be returned, like most R functions.
#' \preformatted{
#' obj$mutate_obs(target, ...)
#' mutate_obs(obj, target, ...)}
#'
#' @param obj An object of type [taxmap()]
#' @param target The name of the table in `obj$data` to filter
#' @param ... One or more named columns to add. Newly created columns can be
#'   referenced in the same function call. Any variable name that appears in
#'   [all_names()] can be used as if it was a vector on its own.
#'
#' @return An object of type [taxmap()]
#'
#' @examples
#' mutate_obs(ex_taxmap, "info",
#'            new_col = "Im new",
#'            newer_col = paste0(new_col, "er!"))
#'
#' @family taxmap manipulation functions
#' @name mutate_obs
NULL


#' Replace columns in [taxmap()] objects
#'
#' Replace columns of tables in `obj$data` in [taxmap()] objects. See
#' [dplyr::transmute()] for the inspiration for this function and more
#' information.
#' \preformatted{
#' obj$transmute_obs(target, ...)
#' transmute_obs(obj, target, ...)}
#'
#' @param obj An object of type [taxmap()]
#' @param target The name of the table in `obj$data` to filter
#' @param ... One or more named columns to add. Newly created columns can be
#'   referenced in the same function call. Any variable name that appears in
#'   [all_names()] can be used as if it was a vector on its own.
#'
#' @return An object of type [taxmap()]
#' @examples
#' transmute_obs(ex_taxmap, "info", new_col = paste0(name, "!!!"))
#'
#' @family taxmap manipulation functions
#'
#' @name transmute_obs
NULL


#' Sort columns of [taxmap()] objects
#'
#' Sort columns of tables in `obj$data` in [taxmap()] objects.
#' Any variable name that appears in [all_names()] can be used as if it
#' was a vector on its own. See [dplyr::arrange()] for the inspiration
#' for this function and more information.
#' \preformatted{
#' obj$arrange_obs(target, ...)
#' arrange_obs(obj, target, ...)}
#'
#' @param obj An object of type [taxmap()].
#' @param target The name of the table in `obj$data` to filter.
#' @param ... One or more expressions (e.g. column names) to sort on.
#'
#' @return An object of type [taxmap()]
#'
#' @examples
#' # Sort in ascending order
#' arrange_obs(ex_taxmap, "info", n_legs)
#' arrange_obs(ex_taxmap, "foods", name)
#'
#' # Sort in decending order
#' arrange_obs(ex_taxmap, "info", desc(n_legs))
#'
#' @family taxmap manipulation functions
#'
#' @name arrange_obs
NULL


#' Sample n observations from [taxmap()]
#'
#' Randomly sample some number of observations from a [taxmap()]
#' object. Weights can be specified for observations or the taxa they are classified
#' by. Any variable name that appears in [all_names()] can be used as
#' if it was a vector on its own. See [dplyr::sample_n()] for the inspiration
#' for this function.
#' \preformatted{
#' obj$sample_n_obs(target, size, replace = FALSE, taxon_weight = NULL,
#' obs_weight = NULL, use_supertaxa = TRUE, collapse_func = mean, ...)
#' sample_n_obs(obj, target, size, replace = FALSE, taxon_weight = NULL,
#' obs_weight = NULL, use_supertaxa = TRUE, collapse_func = mean, ...)}
#'
#' @param obj ([taxmap()]) The object to sample from.
#' @param target The name of the table in `obj$data` to filter
#' @param size (`numeric` of length 1) The number of observations to
#'   sample.
#' @param replace (`logical` of length 1) If `TRUE`, sample with
#'   replacement.
#' @param taxon_weight (`numeric`) Non-negative sampling weights of each
#'   taxon. If `use_supertaxa` is `TRUE`, the weights for each taxon
#'   in an observation's classification are supplied to `collapse_func` to
#'   get the observation weight. If `obs_weight` is also specified, the two
#'   weights are multiplied (after `taxon_weight` for each observation is
#' calculated).
#' @param obs_weight (`numeric`) Sampling weights of each observation.  If
#'   `taxon_weight` is also specified, the two weights are multiplied (after
#'   `taxon_weight` for each observation is calculated).
#' @param use_supertaxa (`logical` or `numeric` of length 1) Affects how the
#'   `taxon_weight` is used. If `TRUE`, the weights for each taxon in an
#'   observation's classification are multiplied to get the observation weight.
#'   Otherwise, just the taxonomic level the observation is assign to it
#'   considered. If `TRUE`, use all supertaxa. Positive numbers indicate the
#'   number of ranks above each taxon to use. `0` is equivalent to `FALSE`.
#'   Negative numbers are equivalent to `TRUE`.
#' @param collapse_func (`function` of length 1) If `taxon_weight` option is
#'   used and `supertaxa` is `TRUE`, the weights for each
#'   taxon in an observation's classification are supplied to
#'   `collapse_func` to get the observation weight. This function should
#'   take  numeric vector and return a single number.
#' @param ... Additional options are passed to [filter_obs()].
#'
#' @return An object of type [taxmap()]
#'
#' @examples
#' # Sample 2 rows without replacement
#' sample_n_obs(ex_taxmap, "info", 2)
#' sample_n_obs(ex_taxmap, "foods", 2)
#'
#' # Sample with replacement
#' sample_n_obs(ex_taxmap, "info", 10, replace = TRUE)
#'
#' # Sample some rows for often then others
#' sample_n_obs(ex_taxmap, "info", 3, obs_weight = n_legs)
#'
#' @family taxmap manipulation functions
#'
#' @name sample_n_obs
NULL


#' Sample a proportion of observations from [taxmap()]
#'
#' Randomly sample some proportion of observations from a [taxmap()]
#' object. Weights can be specified for observations or their taxa. See
#' [dplyr::sample_frac()] for the inspiration for this function.
#' \preformatted{
#' obj$sample_frac_obs(target, size, replace = FALSE, taxon_weight = NULL,
#' obs_weight = NULL, use_supertaxa = TRUE, collapse_func = mean, ...)
#' sample_frac_obs(obj, target, size, replace = FALSE, taxon_weight =
#' NULL, obs_weight = NULL, use_supertaxa = TRUE, collapse_func = mean, ...)}
#'
#' @param obj ([taxmap()]) The object to sample from.
#' @param target The name of the table in `obj$data` to filter
#' @param size (`numeric` of length 1) The proportion of observations to
#'   sample.
#' @param replace (`logical` of length 1) If `TRUE`, sample with
#'   replacement.
#' @param taxon_weight (`numeric`) Non-negative sampling weights of each
#'   taxon. If `use_supertaxa` is `TRUE`, the weights for each taxon
#'   in an observation's classification are supplied to `collapse_func` to
#'   get the observation weight. If `obs_weight` is also specified, the two
#'   weights are multiplied (after `taxon_weight` for each observation is
#'   calculated).
#' @param obs_weight (`numeric`) Sampling weights of each observation.  If
#'   `taxon_weight` is also specified, the two weights are multiplied
#'   (after `taxon_weight` for each observation is calculated).
#' @param use_supertaxa (`logical` or `numeric` of length 1) Affects how the
#'   `taxon_weight` is used. If `TRUE`, the weights for each taxon in
#'   an observation's classification are multiplied to get the observation
#'   weight. If `FALSE` just the taxonomic level the observation is assign to it
#'   considered. Positive numbers indicate the number of ranks above the
#'   each taxon to use. `0` is equivalent to `FALSE`. Negative numbers
#'   are equivalent to `TRUE`.
#' @param collapse_func (`function` of length 1) If `taxon_weight`
#'   option is used and `supertaxa` is `TRUE`, the weights for each
#'   taxon in an observation's classification are supplied to
#'   `collapse_func` to get the observation weight. This function should
#'   take  numeric vector and return a single number.
#' @param ... Additional options are passed to [filter_obs()].
#'
#' @return An object of type [taxmap()]
#'
#' @examples
#' sample_frac_obs(ex_taxmap, "info", 0.5)
#'
#' @family taxmap manipulation functions
#'
#' @name sample_frac_obs
NULL


#' Count observations in [taxmap()]
#'
#' Count observations for each taxon in a [taxmap()] object.
#' This includes observations for the specific taxon and its subtaxa.
#' \preformatted{
#' obj$n_obs(target)
#' n_obs(obj, target)}
#'
#' @param obj ([taxmap()])
#' @param target The name of the list/vector/table in `obj$data`
#'
#' @return `numeric`
#'
#' @examples
#' n_obs(ex_taxmap, "info")
#'
#' @family taxmap data functions
#'
#' @name n_obs
NULL


#' Count observation assigned in [taxmap()]
#'
#' Count observations assigned to a specific taxon in an [taxmap()].
#' This does not include observations assigned to subtaxa.
#' \preformatted{
#' obj$n_obs_1(target)
#' n_obs_1(obj, target)}
#'
#' @param obj ([taxmap()])
#' @param target The name of the list/vector/table in `obj$data`
#'
#' @return `numeric`
#'
#' @examples
#' n_obs_1(ex_taxmap, "info")
#'
#' @family taxmap data functions
#'
#' @name n_obs_1
NULL
