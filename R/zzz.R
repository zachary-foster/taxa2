ct <- function(l) Filter(Negate(is.null), l)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

add_names <- function(...) {
  tt <- sapply(match.call(), deparse)[-1]
  nms <- unname(sapply(tt, function(x) strsplit(x, split = "\\$")[[1]][2]))
  stats::setNames(list(...), nms)
}
