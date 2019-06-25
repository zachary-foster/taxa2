#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon_auth constructor
#'
#' Minimal taxon_auth constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param auth Zero or more taxon authorities. Inputs will be transformed to a `character` vector.
#'
#' @return An `S3` object of class `taxa_taxon_auth`
#'
#' @keywords internal
new_taxon_auth <- function(auth = character()) {
  vctrs::vec_assert(auth, ptype = character())
  vctrs::new_rcrd(list(auth = auth), class = "taxa_taxon_auth")
}


#' Taxon authority class
#'
#' Used to store taxon authorities. This is typically used to store taxon IDs in [taxon()] objects.
#'
#' @export
#' @inheritParams new_taxon_auth
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon_auth`
#' @family classes
taxon_auth <- function(auth = character()) {
  UseMethod("taxon_auth")
}


#' @export
taxon_auth.default <- function(auth = character()) {
  auth <- vctrs::vec_cast(auth, character())
  new_taxon_auth(auth)
}


#' @export
`taxon_auth<-` <- function(x, value) {
  UseMethod('taxon_auth<-')
}


#' @importFrom methods setOldClass
methods::setOldClass(c("taxa_taxon_auth", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' Prepare taxon_auth for printing
#'
#' Prepare taxon_auth for printing. Makes color optional.
#'
#' @param color Use color?
#'
#' @return character
#'
#' @keywords internal
printed_taxon_auth <- function(x, color = FALSE) {
  out <- vctrs::field(x, 'auth')
  out <- font_na(out)
  if (! color) {
    out <- crayon::strip_style(out)
  }
  return(out)
}


#' @export
#' @keywords internal
format.taxa_taxon_auth <- function(x, ...) {
  printed_taxon_auth(x, color = FALSE)
}


#' @export
#' @keywords internal
obj_print_data.taxa_taxon_auth <- function(x) {
  if (length(x) == 0) {
    return()
  }
  out <- printed_taxon_auth(x, color = TRUE)
  print_with_color(out, quote = TRUE)
}


#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon_auth <- function(x) {
  "tax_auth"
}


#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon_auth <- function(x) {
  paste0("taxon_auth")
}


#' @importFrom pillar pillar_shaft
#' @export
#' @keywords internal
pillar_shaft.taxa_taxon_auth <- function(x, ...) {
  out <- printed_taxon_auth(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "left")
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @method vec_type2 taxa_taxon_auth
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon_auth
#' @keywords internal
vec_type2.taxa_taxon_auth <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon_auth", y)


#' @method vec_type2.taxa_taxon_auth default
#' @export
vec_type2.taxa_taxon_auth.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.taxa_taxon_auth vctrs_unspecified
#' @export
vec_type2.taxa_taxon_auth.vctrs_unspecified <- function(x, y, ...) x


#' @method vec_type2.taxa_taxon_auth taxa_taxon_auth
#' @export
vec_type2.taxa_taxon_auth.taxa_taxon_auth <- function(x, y, ...) new_taxon_auth()


#' @method vec_type2.taxa_taxon_auth character
#' @export
vec_type2.taxa_taxon_auth.character <- function(x, y, ...) character()


#' @method vec_type2.character taxa_taxon_auth
#' @importFrom vctrs vec_type2.character
#' @export
vec_type2.character.taxa_taxon_auth <- function(x, y, ...) character()


#' @method vec_type2.taxa_taxon_auth factor
#' @export
vec_type2.taxa_taxon_auth.factor <- function(x, y, ...) factor()


#' @method vec_type2.factor taxa_taxon_auth
#' @importFrom vctrs vec_type2.factor
#' @export
vec_type2.factor.taxa_taxon_auth <- function(x, y, ...) factor()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @method vec_cast taxa_taxon_auth
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon_auth
#' @keywords internal
vec_cast.taxa_taxon_auth <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon_auth")


#' @method vec_cast.taxa_taxon_auth default
#' @export
vec_cast.taxa_taxon_auth.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @method vec_cast.taxa_taxon_auth taxa_taxon_auth
#' @export
vec_cast.taxa_taxon_auth.taxa_taxon_auth <- function(x, to, x_arg, to_arg) x


#' @method vec_cast.taxa_taxon_auth character
#' @export
vec_cast.taxa_taxon_auth.character <- function(x, to, x_arg, to_arg) taxon_auth(x)


#' @method vec_cast.character taxa_taxon_auth
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon_auth <- function(x, to, x_arg, to_arg) vctrs::field(x, "auth")


#' @method vec_cast.taxa_taxon_auth factor
#' @export
vec_cast.taxa_taxon_auth.factor <- function(x, to, x_arg, to_arg) taxon_auth(x)


#' @method vec_cast.factor taxa_taxon_auth
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_taxon_auth <- function(x, to, x_arg, to_arg) factor(vctrs::field(x, "auth"))



#--------------------------------------------------------------------------------
# S3 equality and comparison functions
#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' Check if is a taxon authority
#'
#' Check if an object is the taxon authority class
#'
#' @param x An object to test
#'
#' @export
is_taxon_auth <- function(x) {
  inherits(x, "taxa_taxon_auth")
}


#' @export
is.na.taxa_taxon_auth <- function(x) {
  is.na(vctrs::vec_cast(x, character()))
}



#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

