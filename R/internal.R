#' Print a subset of a character vector
#'
#' Prints the start and end values for a character vector. The number of values
#' printed depend on the width of the screen by default.
#'
#' @param chars (`character`) What to print.
#' @param prefix (`character` of length 1) What to print before
#'   `chars`, on the same line.
#' @param sep What to put between consecutive values
#' @param mid What is used to indicate omitted values
#' @param trunc What is appended onto truncated values
#' @param max_chars (`numeric` of length 1) The maximum number of
#'   characters to print.
#' @param type (`"error"`, `"warning"`, `"message"`, `"cat"`, `"print"`, `"silent"`, `"plain"`)
#'
#' @return `NULL`
#'
#' @examples
#' taxa:::limited_print(1:100)
#' taxa:::limited_print(1:10000)
#' taxa:::limited_print(1:10000, prefix = "stuff:")
#'
#' @keywords internal
limited_print <- function(chars, prefix = "", sep = ", ", mid = " ... ",
                          trunc_char = "[truncated]",
                          max_chars = getOption("width") - nchar(prefix) - 5,
                          type = "message") {

  # https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
  interleave <- function(v1,v2) {
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1,v2)[order(c(ord1,ord2))]
  }

  truncate <- function(x, max_chars = 30) {
    if (nchar(x) > max_chars) {
      x <- paste0(substr(x, 0, max_chars - nchar(crayon::strip_style(trunc_char))), trunc_char)
    }
    return(x)
  }

  # Remove colsole fonts
  raw_chars <- chars
  chars <- crayon::strip_style(chars)

  # Convert NA to "NA"
  chars[is.na(chars)] <- "NA"

  #
  if (length(chars) == 0) {
    output <- prefix
    return(invisible(NULL))
  }

  #
  q = "'"
  interleaved <- interleave(chars[1:(length(chars) / 2)],
                            rev(chars[(length(chars) / 2 + 1):length(chars)]))
  is_greater_than_max <- cumsum(nchar(interleaved) + nchar(crayon::strip_style(sep))) + 10 > max_chars
  if (all(! is_greater_than_max)) {
    max_printed <- length(chars)
  } else {
    max_printed <- which.max(is_greater_than_max) - 1
  }
  if (max_printed < length(chars)) {
    if (max_printed < 2) {
      first_part <- truncate(chars[1])
      second_part <- truncate(chars[length(chars)])
    } else {
      first_part <-  raw_chars[1:ceiling(max_printed / 2)]
      second_part <- raw_chars[(length(chars) - floor(max_printed / 2) + 1):length(chars)]
    }
    if (length(chars) > 1) {
      output <- paste0(paste0(collapse = sep, first_part),
                       mid,
                       paste0(collapse = sep, second_part),
                       "\n")
    } else {
      output <- paste0(paste0(collapse = sep, first_part),
                       "\n")

    }
  } else {
    output <- paste0(paste0(collapse = sep, raw_chars), "\n")
  }
  output <- paste(prefix, output, collapse = "")

  if (type == "error") {
    stop(output)
  } else if (type == "warning") {
    warning(output)
  } else if (type == "message") {
    message(output)
  } else if (type == "cat") {
    cat(output)
  } else if (type == "print") {
    print(output)
  } else if (type == "plain") {
    output <- crayon::strip_style(output)
  } else if (type != "silent") {
    stop("invalid type option")
  }
  return(invisible(output))
}


#' Return name of database
#'
#' This is meant to return the name of a database when it is not known if the
#' input is a `TaxonDatabase` object or a simple character vector.
#'
#' @param input Either a character vector or `TaxonDatabase` class
#'
#' @return The name of the database
#'
#' @keywords internal
get_database_name <- function(input) {
  if ("TaxonDatabase" %in% class(input)) {
    database_name <- input$name
  } else {
    database_name <- input
  }
  return(database_name)
}


#' Like `strsplit`, but with multiple separators
#'
#' Splits items in a vector by multiple separators.
#'
#' @param input A character vector
#' @param split One or more separators to use to split `input`
#' @param ... Passed to [base::strsplit()]
#'
#' @keywords internal
multi_sep_split <- function(input, split, ...) {
  lapply(input, function(x) {
    for (sep in split) {
      x <- unlist(strsplit(x, split = sep, ...))
    }
    return(x)
  })
}


#' Get indexes of a unique set of the input
#'
#' @keywords internal
unique_mapping <- function(input) {
  unique_input <- unique(input)
  vapply(input, function(x) {if (is.na(x)) which(is.na(unique_input)) else which(x == unique_input)}, numeric(1))
}


#' Run a function on unique values of a iterable
#'
#' Runs a function on unique values of a list/vector and then reformats the
#' output so there is a one-to-one relationship with the input.
#'
#' @param input What to pass to \code{func}
#' @param func (\code{function})
#' @param ... passed to \code{func}
#'
#' @keywords internal
map_unique <- function(input, func, ...) {
  input_class <- class(input)
  unique_input <- unique(input)
  class(unique_input) <- input_class
  func(unique_input, ...)[unique_mapping(input)]
}


#' Converts decimal numbers to other bases
#'
#' Converts from base 10 to other bases represented by a given set of symbols.
#'
#' @param numbers One or more numbers to convert.
#' @param symbols The set of symbols to use for the new base.
#' @param base The base to convert to.
#' @param min_length The minimum number of symbols in each result.
#'
#' @return character vector
#'
#' @keywords internal
convert_base <- function(numbers, symbols = letters, base = length(symbols),
                         min_length = 0) {

  # A modification of the `dec2base` function in the `oro.dicom` package
  #    Copyright (c) 2015, Brandon Whitcher
  convert_one <- function (n)  {
    if (is.na(n)) {
      return(NA_character_)
    }
    max_length <- max(trunc(log(max(n, 1))/log(base)) + 1, min_length)
    power <- rep(1, length(n)) * base^((max_length - 1):0)
    n <- n * rep(1, max_length)
    digits <- floor((n%%(base * power))/power)
    paste(symbols[digits + 1], collapse = "")
  }

  vapply(as.integer(numbers), convert_one, character(1))

}


#' check for packages
#'
#' check for packages, and stop if not installed
#'
#' @param package The name of the package
#'
#' @return `TRUE` if package is present
#'
#' @keywords internal
check_for_pkg <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Please install ", package, call. = FALSE)
  } else {
    invisible(TRUE)
  }
}


#' Get input from dots or list
#'
#' Get input from dots or list, but not both.
#' Throws an error if both are supplied.
#'
#' @param ... Dots input
#' @param .list List input
#'
#' @return A list of inputs
#'
#' @keywords internal
get_dots_or_list <- function(..., .list = NULL) {
  dots_input <- list(...)
  list_input <- .list
  if (length(dots_input) > 0 && length(list_input) == 0) {
    return(dots_input)
  } else if (length(dots_input) == 0 && length(list_input) > 0) {
    return(list_input)
  } else if (length(dots_input) > 0 && length(list_input) > 0) {
    stop("Both `...` and `.list` were supplied. Only one can be used at a time.",
         call. = FALSE)
  } else {
    return(list())
  }
}

#' Format a proportion as a printed percent
#'
#' Format a proportion as a printed percent
#'
#' @param prop The proportion
#' @param ... passed to `format`
#' @inheritParams base::format
#'
#' @return character
#'
#' @keywords internal
to_percent <- function(prop, digits = 3, ...) {
  if (prop < .00001) {
    return("< 0.001%")
  } else {
    return(paste0(format(prop * 100, digits = digits, ...), '%'))
  }
}


#' Check length of thing
#'
#' Check the length of an object, be it list, vector, or table.
#'
#' @param obj
#'
#' @return \code{numeric} of length 1.
#'
#' @keywords internal
length_of_thing <- function(obj) {
  if (is.data.frame(obj)) {
    return(nrow(obj))
  } else {
    return(length(obj))
  }
}

#' Check argument types
#'
#' Check that an argument is one of an accepted set of classes and throw and error if it is not.
#'
#' @param value The value of the argument
#' @param valid_classes A character vector of valid classes. It must inherit at least one of these.
#' @param arg_name The name of the argument used in error messages. By defualt the name of the
#'   variable passed to "value" is used.
#'
#' @keywords internal
check_arg_class <- function(value, valid_classes, arg_name = deparse(substitute(x))) {
  if (!is.null(value)) {
    if (!any(class(value) %in% valid_classes)) {
      stop('', arg_name, ' must be a class that is or inherits one of the following classes: ',
           paste0(valid_classes, collapse = ", "), '. An object of class "', class(value), '" was given.', call. = FALSE)
    }
  }
}


#' Convert to character or a placeholder
#'
#' The placeholder is used with the thing to be printed is NULL or of zero length.
#'
#' @param thing The thing to convert to a character
#' @param placeholder What is returned when "thing" is NULL or of zero length.
#'
#' @keywords internal
char_or_placeholder <- function(thing, placeholder = "[none]") {
  if (is.null(thing) || length(thing) == 0) {
    return(placeholder)
  } else {
    return(as.character(thing))
  }
}


#' Clone R6 objects
#'
#' Check if the object is an R6 class and return a deep clone if so, otherwise return the object
#'
#' @param input the object to clone
#'
#' @keywords internal
clone_if_r6 <- function(input) {
  if (length(input) > 0 && "R6" %in% class(input)) {
    return(input$clone(deep = TRUE))
  } else {
    return(input)
  }
}

#' @keywords internal
ct <- function(l) Filter(Negate(is.null), l)

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @keywords internal
add_names <- function(...) {
  tt <- sapply(match.call(), deparse)[-1]
  nms <- unname(sapply(tt, function(x) strsplit(x, split = "\\$")[[1]][2]))
  stats::setNames(list(...), nms)
}


#' @keywords internal
assert <- function(x, y) {
  if (!is.null(x)) {
    if (!any(class(x) %in% y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

#' @keywords internal
csep2vec <- function(z, uniq = TRUE) {
  tmp <- unlist(lapply(z, function(w) strsplit(w, split = ",")[[1]]), FALSE)
  if (uniq) return(unique(tmp))
  return(tmp)
}

#' @keywords internal
strex <- function(str, pattern) regmatches(str, regexpr(pattern, str))

#' @keywords internal
named_field <- function(x, i) {
  out <- vctrs::field(x, i)
  if (! is.null(names(x))) {
    names(out) <- names(x)
  }
  return(out)
}


#' Remove names from fields in a vctrs rcrd
#'
#' Remove names from fields in a vctrs rcrd
#'
#' @param x a vctrs rcrd
#'
#' @keywords internal
unname_fields <- function(x) {
  for (f in vctrs::fields(x)) {
    vctrs::field(x, f) <- unname(vctrs::field(x, f))
  }
  return(x)
}