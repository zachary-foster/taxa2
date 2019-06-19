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


#' Punctuation formatting in print methods
#'
#' A simple wrapper to make changing the formatting of text printed easier.
#' This is used for non-data, formatting characters
#'
#' @param text What to print
#'
#' @family printer fonts
#'
#' @keywords internal
font_punct <- function(text) {
  crayon::silver(text)
}



#' Print that works with color
#'
#' The same as the `print` function, but can print colored text. Its a bit of a hack, but the only
#' way I found to replicate the behavior of `print` without rewriting the entire `print` function.
#'
#' @param x What to print, typically a character vector
#' @param ... Passed to `print`
#'
#' @keywords internal
print_with_color <- function(x, ...) {

  # Apply maximum printing limits
  original_length <- length(x)
  original_input <- x
  if (original_length > options()$max.print) {
    x <- x[seq_len(options()$max.print)]
  }
  formatted_intput <- format(x)

  # Print fake data with same length as uncolored text
  dummy <- vapply(nchar(crayon::strip_style(formatted_intput)), FUN.VALUE = character(1),
                  function(n) paste0(rep("@", n), collapse = ""))
  dummy <- capture.output(print(dummy, ...))
  dummy <- paste0(dummy, collapse = "\n")

  # Replace fake data with colored data
  split_dummy <- strsplit(dummy, "@+")[[1]]
  output <- paste0(interleave(c(split_dummy, ""), c(formatted_intput, "")), collapse = "")

  # Replicate maximum print truncation message
  if (original_length > length(x)) {
    output <- paste0(output, '\n',
                     '[ reached getOption("max.print") -- omitted ',
                     original_length - length(x), ' entries ]')
  }

  # Print output and return input
  output <- paste0(output, '\n')
  cat(output)
  invisible(original_input)
}

#' Interleves two vectors
#'
#' Taken from "http://r.789695.n4.nabble.com/Interleaving-elements-of-two-vectors-td795123.html"
#'
#' @keywords internal
interleave <- function(v1, v2)
{
  ord1 <- 2*(1:length(v1))-1
  ord2 <- 2*(1:length(v2))
  c(v1,v2)[order(c(ord1,ord2))]
}