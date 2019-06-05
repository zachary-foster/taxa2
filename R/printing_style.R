#' Font for secondary data
#'
#' A wrapper to make changing the formatting of text printed easier.
#' This is used for print data associated with other data.
#'
#' @param text What to print.
#'
#' @family printer fonts
#'
#' @keywords internal
font_secondary <- function(text) {
  crayon::silver(text)
}
