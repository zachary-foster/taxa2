#' Print a item
#'
#' Used to print each item in the `taxmap` print method.
#'
#' @param data The item to be printed
#' @param max_rows (`numeric` of length 1) The maximum number of rows in
#'   tables to print.
#' @param max_items (`numeric` of length 1) The maximum number of list
#'   items to print.
#' @param max_width (`numeric` of length 1) The maximum number of
#'   characters to print.
#' @param prefix (`numeric` of length 1) What to print in front of each
#'   line.
#'
#' @examples
#' taxa:::print_item(ex_taxmap$data$info)
#' taxa:::print_item(1:100)
#'
#' @keywords internal
print_item <- function(data, name = NULL, max_rows = 3, max_items = 3,
                       max_width = getOption("width") - 10, prefix = "") {

  # Find best print method
  print_methdods <- c(paste0("print__", class(data)),
                      "print__default_")
  applicable_methods <- print_methdods[vapply(print_methdods, function(x) exists(x), logical(1))]
  best_method <- applicable_methods[1]

  # Call print method
  get(best_method)(data, name = name, prefix = prefix, max_width = max_width,
                   max_rows = max_rows)

  invisible(data)
}


#' Print a object with a prefix
#'
#' Print a object with a prefix. Uses the standard print method of the object.
#'
#' @param x What to print.
#'
#' @keywords internal
prefixed_print <- function(x, prefix, ...) {
  output <- paste0(prefix, utils::capture.output(print(x, ...)))
  cat(paste0(paste0(output, collapse = "\n"), "\n"))
}



#' Print a tibble
#'
#' Print a table for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__tbl_df <- function(obj, name, prefix, max_width, max_rows) {
  loadNamespace("dplyr") # used for tibble print methods
  if (length(name) > 0 && ! is.na(name)) {
    cat(paste0(prefix, crayon::bold(name), ":\n"))
  }
  prefixed_print(obj, prefix = paste0(prefix, "  "), n = max_rows,
                 width = max_width)
}


#' Print a data.frame
#'
#' Print a data.frame for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__data.frame <- function(obj, name, prefix, max_width, max_rows) {
  cat(paste0(prefix, crayon::bold(name), ":\n"))
  if (nrow(obj) > max_rows) {
    cat(paste0(prefix, "  A ", nrow(obj), " by ", ncol(obj), " data.frame (first ",
               max_rows, " rows shown)\n"))
    obj <- obj[1:max_rows, , drop = FALSE]
  } else {
    cat(paste0(prefix, "  A ", nrow(obj), " by ", ncol(obj), " data.frame\n"))
  }
  prefixed_print(obj, prefix = paste0(prefix, "  "))
}


#' Print a list
#'
#' Print a list for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__list <- function(obj, name, prefix, max_width, max_rows) {
  if (length(obj) < 1) {
    prefixed_print(list(), prefix = prefix)
  } else {
    cat(paste0(prefix, crayon::bold(name), ": a list with ", length(obj),
               ifelse(length(obj) == 1, " item", " items")))
    if (is.null(names(obj))) {
      cat("\n")
    } else {
      cat(paste0(" with names:\n  ",
                 limited_print(names(obj), prefix = prefix, type = "silent")))
    }
  }
}


#' Generic vector printer
#'
#' Print a vector for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#' @param type The name of the type of vector to print (e.g. numeric).
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__vector <- function(obj, name, prefix, max_width, max_rows, type = class(obj)[1]) {
  cat(paste0(prefix, crayon::bold(name), ": ", ifelse(is.null(names(obj)), "a ", "a named "), type, " with ", length(obj),
             " item", ifelse(length(obj) == 1, "", "s"), "\n  ", prefix))
  if (is.null(names(obj))) {
    limited_print(obj, max_chars = max_width, type = "cat")
  } else {
    limited_print(paste0(names(obj), ". ", obj), max_chars = max_width, type = "cat")
  }
}


#' Print an integer
#'
#' Print an integer for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__integer <- function(obj, name, prefix, max_width, max_rows) {
  print__vector(obj, name, prefix, max_width, max_rows)
}



#' Print a numeric
#'
#' Print a numeric vector for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__numeric <- function(obj, name, prefix, max_width, max_rows) {
  print__vector(obj, name, prefix, max_width, max_rows)
}


#' Print a character
#'
#' Print a character for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__character <- function(obj, name, prefix, max_width, max_rows) {
  print__vector(obj, name, prefix, max_width, max_rows)
}


#' Print a logical
#'
#' Print a logical for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__logical <- function(obj, name, prefix, max_width, max_rows) {
  print__vector(obj, name, prefix, max_width, max_rows)
}


#' Print a factor
#'
#' Print a factor for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__factor <- function(obj, name, prefix, max_width, max_rows) {
  print__vector(obj, name, prefix, max_width, max_rows)
}


#' Print a ordered factor
#'
#' Print a ordered factor for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__ordered <- function(obj, name, prefix, max_width, max_rows) {
  print__vector(obj, name, prefix, max_width, max_rows, type = "ordered factor")
}


#' Print a matrix
#'
#' Print a matrix for the print method of taxmap objects.
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__matrix <- function(obj, name, prefix, max_width, max_rows) {
  cat(paste0(prefix, crayon::bold(name), ":\n"))
  if (nrow(obj) > max_rows) {
    cat(paste0(prefix, "  A ", nrow(obj), " by ", ncol(obj), " matrix (first ",
               max_rows, " rows shown)\n"))
    obj <- obj[1:max_rows, , drop = FALSE]
  } else {
    cat(paste0(prefix, "  A ", nrow(obj), " by ", ncol(obj), " matrix\n"))
  }
  prefixed_print(obj, prefix = paste0(prefix, "  "))
}


#' Print method for unsupported
#'
#' Print method for unsupported classes for taxmap objects
#'
#' Which print method is called is determined by its name, so changing the name
#' of this function will change when it is called.
#'
#' @param obj Something to print
#' @param name The name of the thing to print
#' @param prefix What to put before the thing printed. Typically a space.
#' @param max_width Maximum width in number of characters to print
#' @param max_rows Maximum number of rows to print
#'
#' @family taxmap print methods
#'
#' @keywords internal
print__default_ <- function(obj, name, prefix, max_width, max_rows) {
  cat(paste0(prefix, crayon::bold(name), ":\n"))
  prefixed_print(obj, prefix = paste0(prefix, "  "))
}
