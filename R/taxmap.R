#' Taxmap class
#'
#' @export
#' @param ... Any number of object of class \code{\link{hierarchy}} or character vectors.
#' @param data A list of tables with data associated with the taxa.
#' @return An \code{R6Class} object of class \code{Taxmap}
#'
#' @details on initialize, function sorts the taxon list, see
#' \code{\link{ranks_ref}} for the reference rank names and orders
taxmap <- function(...) {
  Taxmap$new(...)
}

Taxmap <- R6::R6Class(
  "Taxmap",
  inherit = Taxonomy,
  public = list(
    data = list(),

    initialize = function(..., data = list()) {

      # Call `taxonomy` constructor
      super$initialize(...)

      # Convert tables in `data` to class with "function" columns
      # NOT IMPLEMENTED

      # Add data tables
      self$data <- data
    },

    print = function(indent = "", max_rows = 3, max_table = 3, max_width = getOption("width") - 10) {

      # Call `taxonomy` print method
      super$print(indent = indent)

      # Print first few rows of each table, up to a maximum number of rows/tables
      cat("<data>\n")
      if (length(self$data) > 0) {
        for (i in 1:min(c(max_table, length(self$data)))) {
          print_table(self$data[[i]], name = names(self$data[i]),
                      max_rows = max_rows, max_width = max_width)
        }
        if (length(self$data) < max_table) {
          cat(paste0("And ", length(self$data) - max_table, " more tables:\n"))
          limited_print(names(self$data)[(max_table + 1):length(self$data)])
        }
      } else {
        cat("No associated data.\n")
      }

      invisible(self)
    }
  ),

  private = list(

  )
)



#' Print a table
#'
#' Used to print each table in the `taxmap` print method.
#'
#' @param data The table to be printed
#' @param max_rows (\code{numeric} of length 1) The maximum number of row to print
#' @param max_width (\code{numeric} of length 1) The maximum number of characters to print.
#'
#' @keywords internal
print_table <- function(data, name = NULL, max_rows = 3, max_width = getOption("width") - 10) {
  loadNamespace("dplyr") # used for print methods
  if (length(name) > 0 && ! is.na(name)) {
    cat(paste0(name, ":\n"))
  }
  print(data, n = max_rows, width = max_width)
  invisible(data)
}
