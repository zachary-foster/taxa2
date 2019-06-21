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


#' Taxon info class
#'
#' Used to store arbitrary user-defined data assocaited with data. This is typically used to
#' store taxon info in [taxon()] objects.
#'
#' @export
#' @inheritParams new_taxon_info
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon_info`
#' @family classes
#'
#' @examples
#'
taxon_info <- function(info = list()) {
  # Cast inputs to correct values
  info <- lapply(info, vctrs::vec_cast, list())

  # Create taxon_info object
  new_taxon_info(info = info)
}


#' @importFrom methods setOldClass
methods::setOldClass(c("taxa_taxon_info", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' @export
#' @keywords internal
obj_print_data.taxa_taxon_info <- function(x) {
  print(as.list(x))
}



#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_info <- function(x) {
  "tax_info"
}


#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_info <- function(x) {
  "taxon_info"
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @method vec_type2 taxa_taxon_info
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon_info
#' @keywords internal
vec_type2.taxa_taxon_info <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon_info", y)


#' @method vec_type2.taxa_taxon_info default
#' @export
vec_type2.taxa_taxon_info.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.taxa_taxon_info vctrs_unspecified
#' @export
vec_type2.taxa_taxon_info.vctrs_unspecified <- function(x, y, ...) x


#' @method vec_type2.taxa_taxon_info taxa_taxon_info
#' @export
vec_type2.taxa_taxon_info.taxa_taxon_info <- function(x, y, ...) new_taxon_info()


#' @method vec_type2.taxa_taxon_info list
#' @export
vec_type2.taxa_taxon_info.list <- function(x, y, ...) list()


#' @method vec_type2.list taxa_taxon_info
#' @importFrom vctrs vec_type2.list
#' @export
vec_type2.list.taxa_taxon_info <- function(x, y, ...) list()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @method vec_cast taxa_taxon_info
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_info
#' @keywords internal
vec_cast.taxa_taxon_info <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon_info")


#' @method vec_cast.taxa_taxon_info default
#' @export
vec_cast.taxa_taxon_info.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @method vec_cast.taxa_taxon_info taxa_taxon_info
#' @export
vec_cast.taxa_taxon_info.taxa_taxon_info <- function(x, to, x_arg, to_arg) x


#' @method vec_cast.taxa_taxon_info list
#' @export
vec_cast.taxa_taxon_info.list <- function(x, to, x_arg, to_arg) taxon_info(x)


#' @method vec_cast.list taxa_taxon_info
#' @importFrom vctrs vec_cast.list
#' @export
vec_cast.list.taxa_taxon_info <- function(x, to, x_arg, to_arg) vctrs::field(x, "info")



#--------------------------------------------------------------------------------
# S3 equality and comparison functions
#--------------------------------------------------------------------------------




#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' Check if is a taxon info object
#'
#' Check if an object is the taxon info class
#'
#' @param x An object to test
#'
#' @export
is_taxon_info <- function(x) {
  inherits(x, "taxa_taxon_info")
}



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

