#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_info constructor
#'
#' Minimal taxon_info constructor for internal use. Only use when the input is
#' known to be valid since few validity checks are done.
#'
#' @param info A list of arbitrary, user-defined attributes associated with each taxon. Each element
#'   in the list, one per taxon, should be a named list of zero or more items with unique names.
#'   Values in this list can be accessed with the [taxon_info] function. All elements in the list do
#'   not need to contain the same attributes.
#'
#' @return An `S3` object of class `taxa_taxon_info`
#'
#' @keywords internal
new_taxon_info <- function(info = list()) {

  # Check that values are the correct type
  vctrs::vec_assert(info, ptype = list())

  # Create new object
  vctrs::new_list_of(info, ptype = list(), class = "taxa_taxon_info")
}


#' Set and get taxon info
#'
#' Set and get taxon info in [taxon()] objects.
#'
#' @param ... Used to pass arguments to methods and allow methods to used additional arguments.
#'
#' @importFrom vctrs %<-%
#'
#' @examples
#'
#' # Create taxon vector
#' x <- taxon(name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
#'            info = list(list(n = 1), list(n = 3), list(n = 2), list(n = 9)))
#'
#' # Get parts of the taxon vector
#' taxon_info(x)
#' taxon_info(x, 'n')
#'
#' # Set parts of the taxon vector
#' taxon_info(x)[3:4] <- list(list(count = NA))
#' taxon_info(x, 'n')[1:2] <- c(12, 30)
#'
#' @export
taxon_info <- function(...) {
  UseMethod("taxon_info")
}


#' @keywords internal
taxon_info.default <- function(info = list(), ...) {
  # Cast inputs to correct values
  info <- lapply(info, vctrs::vec_cast, list())

  # Check that all elements are named
  is_named <- vapply(info, FUN.VALUE = logical(1), function(x) {
    length(info) > 0 || (! is.null(names(x)) && all(! is.na(names(x))) && all(names(x) != ''))
  })
  if (any(! is_named)) {
    stop(call. = FALSE,
         'All elements in taxon info must be named lists. ',
         'The following indexes have unnamed elements:\n',
         limited_print(type = 'silent', prefix = '  ', which(! is_named)))
  }

  # Check that all names are unique
  is_named_unique <- vapply(info, FUN.VALUE = logical(1), function(x) {
    length(info) == 0 || length(unique(names(x))) == length(names(x))
  })
  if (any(! is_named_unique)) {
    stop(call. = FALSE,
         'All elements in taxon info must be uniquely named lists. ',
         'The following indexes non-uniquely named elements:\n',
         limited_print(type = 'silent', prefix = '  ', which(! is_named_unique)))
  }

  # Create taxon_info object
  new_taxon_info(info = info)
}


#' @rdname taxon_info
#'
#' @param x An object with taxa.
#' @param value The taxa to set. Inputs will be coerced into a list of uniquely named list.
#'
#' @export
`taxon_info<-` <- function(x, key = NULL, value) {
  UseMethod('taxon_info<-')
}


setOldClass(c("taxa_taxon_info", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @rdname taxa_coercion_funcs
#' @method vec_type2 taxa_taxon_info
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon_info
#' @keywords internal
vec_type2.taxa_taxon_info <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon_info", y)


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon_info default
#' @export
vec_type2.taxa_taxon_info.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon_info vctrs_unspecified
#' @export
vec_type2.taxa_taxon_info.vctrs_unspecified <- function(x, y, ...) x


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon_info taxa_taxon_info
#' @export
vec_type2.taxa_taxon_info.taxa_taxon_info <- function(x, y, ...) new_taxon_info()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon_info list
#' @export
vec_type2.taxa_taxon_info.list <- function(x, y, ...) list()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.list taxa_taxon_info
#' @importFrom vctrs vec_type2.list
#' @export
vec_type2.list.taxa_taxon_info <- function(x, y, ...) list()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @rdname taxa_casting_funcs
#' @method vec_cast taxa_taxon_info
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_info
#' @keywords internal
vec_cast.taxa_taxon_info <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon_info")


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_info default
#' @export
vec_cast.taxa_taxon_info.default <- function(x, to, x_arg, to_arg) {
  vctrs::vec_default_cast(x, to, x_arg, to_arg)
}


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_info taxa_taxon_info
#' @export
vec_cast.taxa_taxon_info.taxa_taxon_info <- function(x, to, x_arg, to_arg) x


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon_info list
#' @export
vec_cast.taxa_taxon_info.list <- function(x, to, x_arg, to_arg) taxon_info(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.list taxa_taxon_info
#' @importFrom vctrs vec_cast.list
#' @export
vec_cast.list.taxa_taxon_info <- function(x, to, x_arg, to_arg) vctrs::field(x, "info")



#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' Check if is a taxon info object
#'
#' Check if an object is the taxon info class
#'
#' @param x An object to test
#'
#' @keywords internal
is_taxon_info <- function(x) {
  inherits(x, "taxa_taxon_info")
}

