#' Lazy
#'
#' @name lazy-helpers
#' @param ... unquoted variables
#' @section How do these functions work?:
#' Each function is a light wrapper around [dplyr::vars()], to use lazy
#' evaluation to be able to pass in unquote variables to save some typing,
#' though you can still quote and get the same result
#'
#' These are not used by themselves but inside of [pop()], [pick()], [span()]
#'
#' @section Symbols:
#' \itemize{
#'  \item `:`: items between x and y, inclusive
#'  \item `::`: items between x and y, exclusive
#'  \item `. <`: all items below rank of x
#'  \item `. >`: all items above rank of x
#' }
#'
#' @section ranks:
#' Ranks can be any character string in the set of acceptable rank
#' names.
#'
#' @section nms:
#' `nms` is named to avoid using `names` which would collide with the
#' fxn [base::names()] in Base R. Can pass in any character taxonomic names.
#'
#' @section ids:
#' Ids are any alphanumeric taxonomic identifier. Some database providers
#' use all digits, but some use a combination of digits and characters.
#'
#' @examples
#' ranks(order)
#' ranks(order, genus)
#' ranks(order:genus)
#' ranks(. > genus)
#'
#' nms(Poaceae)
#' nms(Poaceae, Poa)
#' nms(Poaceae:Poa)
#' nms(. < Poaceae)
#'
#' ids(4544)
#' ids(4544, 4479)
#' ids(4544:4479)
#' ids(. < 4479)
NULL

#' @export
#' @rdname lazy-helpers
ranks <- function(...) {
  helpers_fxn("ranks", ...)
}

#' @export
#' @rdname lazy-helpers
nms <- function(...) {
  helpers_fxn("names", ...)
}

#' @export
#' @rdname lazy-helpers
ids <- function(...) {
  helpers_fxn("ids", ...)
}

###### ---------------------
helpers_fxn <- function(name, ...) {
  rks <- rlang::quos(...)
  clzzs <- vapply(rks, function(z) class(rlang::quo_expr(z)), "",
                  USE.NAMES = FALSE)
  if (any(clzzs == "call")) {
    rlang::quo_expr(rks[[1]])
    tmp <- as.character(rlang::quo_expr(rks[[1]]))
    ops <- paste0(rev(strex(tmp, "^>$|^<$|^\\.$|^:$|^::$")), collapse = " ")
    structure(list(strex(tmp, "[A-Za-z0-9]+")),
              operator = ops, names = name,
              class = 'taxapicker', type = name)
  } else {
    tmp <- vapply(rks, function(z) rlang::quo_name(z), "", USE.NAMES = FALSE)
    structure(list(tmp), names = name, class = 'taxapicker', type = name)
  }
}

print.taxapicker <- function(x, ...) {
  cat("<taxapicker>", sep = "\n")
  cat(sprintf(
    "  (%s) (operator: `%s`): %s", attr(x, "type"), attr(x, "operator") %||% "",
    paste0(unclass(x[[1]]), collapse = ", ")
  ), sep = "\n")
}

# pickers <- function(...) {
#   xx <- Filter(function(x) inherits(x, "taxapicker"), list(...))
#   structure(xx, class = "taxapickers")
# }

Taxapickers <- R6::R6Class(
  "Taxapickers",
  lock_objects = TRUE,
  public = list(
    x = NULL,

    initialize = function(...) {
      self$x <- Filter(function(x) inherits(x, "taxapicker"), list(...))
    },

    print = function() {
      cat(paste0("<Taxapickers> n=", length(self$x)), sep = "\n")
      for (i in seq_along(self$x)) {
        cat(sprintf(
          "  (%s): %s", attr(self$x[[i]], "type"),
          paste0(unclass(self$x[[i]][[1]]), collapse = ", ")
        ), sep = "\n")
      }
    },

    ranks = function() private$pluck("ranks"),
    names = function() private$pluck("names"),
    ids = function() private$pluck("ids")
  ),

  private = list(
    pluck = function(z) {
      self$x[vapply(self$x, attr, "", which = "type") == z]
    }
  )
)

# print.taxapickers <- function(x, ...) {
#   cat(paste0("<taxapickers> n=", length(x)), sep = "\n")
#   for (i in seq_along(x)) {
#     cat(sprintf(
#       "  (%s): %s", attr(x[[i]], "type"),
#       paste0(unclass(x[[i]][[1]]), collapse = ", ")
#     ), sep = "\n")
#   }
# }
