#' Parse options specifying datasets
#'
#' Parse options specifying datasets in taxmap objects
#'
#' @param obj The taxmap object.
#' @param dataset The name/index of datasets in a taxmap object to use. Can also be a logical vector
#'   of length equal to the number of datasets.
#' @param must_be_valid If TRUE, all datasets specified must be valid or an error occurs.
#' @param needed If TRUE, at least one dataset must be specified or an error occurs.
#' @param rm_na If TRUE, then invalid datasets do result in NAs in the output.
#'
#' @return The indexes for the datasets selected
parse_dataset = function(obj, dataset, must_be_valid = TRUE, needed = TRUE, rm_na = TRUE) {

  output <- NULL

  # Convert logicals to numerics
  if (is.logical(dataset)) {
    if (length(dataset) != length(obj$data)) {
      stop("When using a TRUE/FALSE vector to specify the data set, it must be the same length as the number of data sets",
           call. = FALSE)
    } else {
      output <- which(dataset)
    }
  }

  # Check numerical inputs
  if (is.numeric(dataset)) {
    output <- vapply(dataset, FUN.VALUE = numeric(1),
                     function(one) {
                       if (one <= length(obj$data)) {
                         return(one)
                       } else {
                         return(NA_integer_)
                       }
                     })
  }

  # Check character inputs
  if (is.character(dataset)) {
    output <- vapply(dataset, FUN.VALUE = numeric(1),
                     function(one) {
                       if (one %in% names(obj$data)) {
                         return(which(names(obj$data) == one))
                       } else {
                         return(NA_integer_)
                       }
                     })
  }

  # Stop if none of the above could create an output
  if (is.null(output)) {
    stop(call. = FALSE,
         'Invalid input given to "dataset". Must be a character (name of dataset), index, or TRUE/FALSE vector.')

  }

  # Check that all dataset are valid
  if (must_be_valid && any(is.na(output))) {
    stop(call. = FALSE,
         paste0("The input does not correspond to a valid dataset.",
                " Valid datasets include:\n",
                limited_print(type = "silent",
                              paste0("[",seq_along(obj$data), "] ", names(obj$data)))))
  }

  # Remove invlaid datasets from inputs
  if (rm_na) {
    output <- output[!is.na(output)]
  }

  # Check that at least one dataset is specified
  if (needed && length(output) < 1) {
    stop(call. = FALSE, "At least one dataset must be specified.")
  }

  return(output)
}
