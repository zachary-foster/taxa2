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