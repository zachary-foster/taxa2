#' @export
taxon_ids <- function(obj, ...) {
  UseMethod("taxon_ids")
}

#' @export
taxon_ids.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
taxon_ids.Taxonomy <- function(obj, ...) {
  obj$taxon_ids(...)
}


#' @export
taxon_indexes <- function(obj, ...) {
  UseMethod("taxon_indexes")
}

#' @export
taxon_indexes.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
taxon_indexes.Taxonomy <- function(obj, ...) {
  obj$taxon_indexes(...)
}


#' @export
taxon_names <- function(obj, ...) {
  UseMethod("taxon_names")
}

#' @export
taxon_names.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
taxon_ranks.Taxonomy <- function(obj, ...) {
  obj$taxon_ranks(...)
}

#' @export
taxon_ranks <- function(obj, ...) {
  UseMethod("taxon_ranks")
}

#' @export
taxon_ranks.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
taxon_names.Taxonomy <- function(obj, ...) {
  obj$taxon_names(...)
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


#' @export
supertaxa_apply <- function(obj, ...) {
  UseMethod("supertaxa_apply")
}

#' @export
supertaxa_apply.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
supertaxa_apply.Taxonomy <- function(obj, ...) {
  obj$supertaxa_apply(...)
}


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


#' @export
subtaxa_apply <- function(obj, ...) {
  UseMethod("subtaxa_apply")
}

#' @export
subtaxa_apply.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
subtaxa_apply.Taxonomy <- function(obj, ...) {
  obj$subtaxa_apply(...)
}


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


#' @export
leaves <- function(obj, ...) {
  UseMethod("leaves")
}

#' @export
leaves.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
leaves.Taxonomy <- function(obj, ...) {
  obj$leaves(...)
}


#' @export
id_classifications <- function(obj, ...) {
  UseMethod("id_classifications")
}

#' @export
id_classifications.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
id_classifications.Taxonomy <- function(obj, ...) {
  obj$id_classifications(...)
}


#' @export
name_classifications <- function(obj, ...) {
  UseMethod("name_classifications")
}

#' @export
name_classifications.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
name_classifications.Taxonomy <- function(obj, ...) {
  obj$name_classifications(...)
}


#' @export
n_supertaxa <- function(obj) {
  UseMethod("n_supertaxa")
}

#' @export
n_supertaxa.default <- function(obj) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
n_supertaxa.Taxonomy <- function(obj) {
  obj$n_supertaxa()
}


#' @export
n_subtaxa <- function(obj) {
  UseMethod("n_subtaxa")
}

#' @export
n_subtaxa.default <- function(obj) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
n_subtaxa.Taxonomy <- function(obj) {
  obj$n_subtaxa()
}


#' @export
n_subtaxa_1 <- function(obj) {
  UseMethod("n_subtaxa_1")
}

#' @export
n_subtaxa_1.default <- function(obj) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
n_subtaxa_1.Taxonomy <- function(obj) {
  obj$n_subtaxa_1()
}


#' @export
all_names <- function(obj, ...) {
  UseMethod("all_names")
}

#' @export
all_names.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
all_names.Taxonomy <- function(obj, ...) {
  obj$all_names(...)
}


#' @export
names_used <- function(obj, ...) {
  UseMethod("names_used")
}

#' @export
names_used.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
names_used.Taxonomy <- function(obj, ...) {
  obj$names_used(...)
}


#' @export
get_data <- function(obj, ...) {
  UseMethod("get_data")
}

#' @export
get_data.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
get_data.Taxonomy <- function(obj, ...) {
  obj$get_data(...)
}


#' @export
data_used <- function(obj, ...) {
  UseMethod("data_used")
}

#' @export
data_used.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
data_used.Taxonomy <- function(obj, ...) {
  obj$data_used(...)
}


#' @export
filter_taxa <- function(obj, ...) {
  UseMethod("filter_taxa")
}

#' @export
filter_taxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
filter_taxa.Taxonomy <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$filter_taxa(...)
}


#' @export
arrange_taxa <- function(obj, ...) {
  UseMethod("arrange_taxa")
}

#' @export
arrange_taxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
arrange_taxa.Taxonomy <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$arrange_taxa(...)
}


#' @export
sample_n_taxa <- function(obj, ...) {
  UseMethod("sample_n_taxa")
}

#' @export
sample_n_taxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
sample_n_taxa.Taxonomy <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$sample_n_taxa(...)
}


#' @export
sample_frac_taxa <- function(obj, ...) {
  UseMethod("sample_frac_taxa")
}

#' @export
sample_frac_taxa.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
sample_frac_taxa.Taxonomy <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$sample_frac_taxa(...)
}


#' @export
is_root <- function(obj, ...) {
  UseMethod("is_root")
}

#' @export
is_root.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
is_root.Taxonomy <- function(obj, ...) {
  obj$is_root(...)
}


#' @export
is_stem <- function(obj, ...) {
  UseMethod("is_root")
}

#' @export
is_stem.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
is_stem.Taxonomy <- function(obj, ...) {
  obj$is_stem(...)
}


#' @export
is_branch <- function(obj, ...) {
  UseMethod("is_branch")
}

#' @export
is_branch.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
is_branch.Taxonomy <- function(obj, ...) {
  obj$is_branch(...)
}


#' @export
is_leaf <- function(obj, ...) {
  UseMethod("is_leaf")
}

#' @export
is_leaf.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
is_leaf.Taxonomy <- function(obj, ...) {
  obj$is_leaf(...)
}
