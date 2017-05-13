#' @export
obs <- function(obj, ...) {
  UseMethod("obs")
}

#' @export
obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
obs.Taxmap <- function(obj, ...) {
  obj$obs(...)
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
all_names.Taxmap <- function(obj, ...) {
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
names_used.Taxmap <- function(obj, ...) {
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
get_data.Taxmap <- function(obj, ...) {
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
data_used.Taxmap <- function(obj, ...) {
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
filter_taxa.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$filter_taxa(...)
}


#' @export
filter_obs <- function(obj, ...) {
  UseMethod("filter_obs")
}

#' @export
filter_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
filter_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$filter_obs(...)
}


#' @export
select_obs <- function(obj, ...) {
  UseMethod("select_obs")
}

#' @export
select_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
select_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$select_obs(...)
}


#' @export
mutate_obs <- function(obj, ...) {
  UseMethod("mutate_obs")
}

#' @export
mutate_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
mutate_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$mutate_obs(...)
}


#' @export
transmute_obs <- function(obj, ...) {
  UseMethod("transmute_obs")
}

#' @export
transmute_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
transmute_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$transmute_obs(...)
}


#' @export
arrange_obs <- function(obj, ...) {
  UseMethod("arrange_obs")
}

#' @export
arrange_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
arrange_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$arrange_obs(...)
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
arrange_taxa.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$arrange_taxa(...)
}


#' @export
sample_n_obs <- function(obj, ...) {
  UseMethod("sample_n_obs")
}

#' @export
sample_n_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
sample_n_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$sample_n_obs(...)
}


#' @export
sample_frac_obs <- function(obj, ...) {
  UseMethod("sample_frac_obs")
}

#' @export
sample_frac_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
sample_frac_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$sample_frac_obs(...)
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
sample_n_taxa.Taxmap <- function(obj, ...) {
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
sample_frac_taxa.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$sample_frac_taxa(...)
}


#' @export
n_obs <- function(obj, ...) {
  UseMethod("n_obs")
}

#' @export
n_obs.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
n_obs.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$n_obs(...)
}


#'@export
n_obs_1 <- function(obj, ...) {
  UseMethod("n_obs_1")
}

#' @export
n_obs_1.default <- function(obj, ...) {
  stop("Unsupported class: ", class(obj)[[1L]], call. = FALSE, domain = NA)
}

#' @export
n_obs_1.Taxmap <- function(obj, ...) {
  obj <- obj$clone(deep = TRUE)
  obj$n_obs_1(...)
}