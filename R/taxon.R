#--------------------------------------------------------------------------------
# S3 constructors
#--------------------------------------------------------------------------------

#' Minimal taxon constructor
#'
#' Minimal taxon constructor for internal use. Only use when the input is known to be valid since
#' few validity checks are done.
#'
#' @param name The names of taxa. Inputs with be coerced into a [taxon_name] vector if anything else
#'   is given.
#' @param rank The ranks of taxa. Inputs with be coerced into a [taxon_rank] vector if anything else
#'   is given.
#' @param id The ids of taxa. These should be unique identifiers. A given ID with a defined database
#'   can only appear more than once if all other information (e.g. name, rank) are the same, but
#'   differnt IDs can have the same information. Inputs with be coerced into a [taxon_id] vector if
#'   anything else is given.
#' @param auth The authority of the taxon. Inputs with be coerced into a [character] vector if
#'   anything else is given.
#' @param info A list of arbitrary, user-defined attributes associated with each taxon. Each element
#'   in the list, one per taxon, should be a named list of zero or more items with unique names.
#'   Values in this list can be accessed with the [taxon_info] function. All elements in the list do
#'   not need to contain the same attributes.
#'
#' @return An `S3` object of class `taxa_taxon`
#'
#' @keywords internal
new_taxon <- function(name = taxon_name(), rank = taxon_rank(), id = taxon_id(),
                      auth = character(), info = taxon_info()) {

  # Check that values are the correct type
  vctrs::vec_assert(name, ptype = taxon_name())
  # vctrs::vec_assert(rank, ptype = taxon_rank())
  vctrs::vec_assert(id, ptype = taxon_id())
  vctrs::vec_assert(auth, ptype = character())
  vctrs::vec_assert(info, ptype = taxon_info())

  # Create new object
  vctrs::new_rcrd(list(name = name, rank = rank, id = id, auth = auth, info = info),
                  class = "taxa_taxon")
}


#' Taxon rank class
#'
#' Used to store information about taxa, such as names, ranks, id, and other arbitrary data.
#'
#' @export
#' @inheritParams new_taxon
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon`
#' @family classes
#'
#' @examples
#'
taxon <- function(name = taxon_name(), rank = NA, id = NA, auth = NA, info = list(NULL)) {

  # Cast inputs to correct values
  name <- vctrs::vec_cast(name, taxon_name())
  rank <- vctrs::vec_cast(rank, taxon_rank())
  id <- vctrs::vec_cast(id, taxon_id())
  auth <- vctrs::vec_cast(auth, character())
  info <- vctrs::vec_cast(info, taxon_info())

  # Recycle ranks and databases to common length
  c(name, rank, id, auth, info) %<-% vctrs::vec_recycle_common(name, rank, id, auth, info)

  # Create taxon object
  new_taxon(name = name, rank = rank, id = id, auth = auth, info = info)
}


#' @importFrom methods setOldClass
methods::setOldClass(c("taxa_taxon", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------


#' @export
`levels<-.taxa_taxon` <- function(x, value) {
  levels(vctrs::field(x, 'rank')) <- value
  return(x)
}


#' @export
levels.taxa_taxon <- function(x) {
  levels(vctrs::field(x, 'rank'))
}


#' @export
`taxon_id<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_id())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "id") <- value
  return(x)
}


#' @export
taxon_id.taxa_taxon <- function(id = character()) {
  vctrs::field(id, "id")
}


#' @export
`taxon_name<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_name())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "name") <- value
  return(x)
}


#' @export
taxon_name.taxa_taxon <- function(name = character()) {
  vctrs::field(name, "name")
}


#' @export
`taxon_rank<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_rank())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "rank") <- value
  return(x)
}


#' @export
taxon_rank.taxa_taxon <- function(rank = character()) {
  vctrs::field(rank, "rank")
}


#--------------------------------------------------------------------------------
# S3 printing functions
#--------------------------------------------------------------------------------

#' Prepare taxon for printing
#'
#' Prepare taxon for printing. Makes color optional.
#'
#' @param color Use color?
#'
#' @return character
#'
#' @keywords internal
printed_taxon <- function(x, color = FALSE) {
  id <- vctrs::field(x, 'id')
  rank <- vctrs::field(x, 'rank')
  auth <- vctrs::field(x, 'auth')
  out <- font_tax_name(x)
  # out <- paste0(out, ifelse(is.na(auth), '', paste0(' ', font_tax_name(auth))))
  out <- paste0(ifelse(is.na(id), '', paste0(font_secondary(id), font_punct('|'))),
                out)
  out <- paste0(out, ifelse(is.na(rank), '', paste0(font_punct('|'), font_secondary(rank))))
  if (! color) {
    out <- crayon::strip_style(out)
  }
  return(out)
}


#' @export
#' @keywords internal
format.taxa_taxon <- function(x, ...) {
  printed_taxon(x, color = FALSE)
}


#' @export
#' @keywords internal
obj_print_data.taxa_taxon <- function(x) {
  if (length(x) == 0) {
    return()
  }
  out <- printed_taxon(x, color = TRUE)
  print_with_color(out, quote = FALSE)
}


#' @export
#' @keywords internal
obj_print_footer.taxa_taxon <- function(x) {
  # print taxon rank levels
  vctrs::obj_print_footer(vctrs::field(x, 'rank'))

  # print databases used in names, ranks, and ids
  # if (any(! is.na(taxon_db())))
}


#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon <- function(x) {
  "taxon"
}


#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon <- function(x) {
  "taxon"
}


#' @importFrom pillar pillar_shaft
#' @export
#' @keywords internal
pillar_shaft.taxa_taxon <- function(x, ...) {
  out <- printed_taxon(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "left")
}



#--------------------------------------------------------------------------------
# S3 coercion functions
#--------------------------------------------------------------------------------

#' @method vec_type2 taxa_taxon
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon
#' @keywords internal
vec_type2.taxa_taxon <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon", y)


#' @method vec_type2.taxa_taxon default
#' @export
vec_type2.taxa_taxon.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.taxa_taxon vctrs_unspecified
#' @export
vec_type2.taxa_taxon.vctrs_unspecified <- function(x, y, ...) x


#' @method vec_type2.taxa_taxon taxa_taxon
#' @export
vec_type2.taxa_taxon.taxa_taxon <- function(x, y, ...) new_taxon()


#' @method vec_type2.taxa_taxon character
#' @export
vec_type2.taxa_taxon.character <- function(x, y, ...) character()


#' @method vec_type2.character taxa_taxon
#' @importFrom vctrs vec_type2.character
#' @export
vec_type2.character.taxa_taxon <- function(x, y, ...) character()


#' @method vec_type2.taxa_taxon factor
#' @export
vec_type2.taxa_taxon.factor <- function(x, y, ...) factor()


#' @method vec_type2.factor taxa_taxon
#' @importFrom vctrs vec_type2.factor
#' @export
vec_type2.factor.taxa_taxon <- function(x, y, ...) factor()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @method vec_cast taxa_taxon
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon
#' @keywords internal
vec_cast.taxa_taxon <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon")


#' @method vec_cast.taxa_taxon default
#' @export
vec_cast.taxa_taxon.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @method vec_cast.taxa_taxon taxa_taxon
#' @export
vec_cast.taxa_taxon.taxa_taxon <- function(x, to, x_arg, to_arg) x


#' @method vec_cast.taxa_taxon character
#' @export
vec_cast.taxa_taxon.character <- function(x, to, x_arg, to_arg) taxon(x)


#' @method vec_cast.character taxa_taxon
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon <- function(x, to, x_arg, to_arg) as.character(vctrs::field(x, "name"))


#' @method vec_cast.taxa_taxon factor
#' @export
vec_cast.taxa_taxon.factor <- function(x, to, x_arg, to_arg) taxon(x)


#' @method vec_cast.factor taxa_taxon
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_taxon <- function(x, to, x_arg, to_arg) as.factor(vctrs::field(x, "name"))



#--------------------------------------------------------------------------------
# S3 equality and comparison functions
#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
# Exported utility functions
#--------------------------------------------------------------------------------

#' Check if is a taxon rank
#'
#' Check if an object is the taxon rank class
#'
#' @param x An object to test
#'
#' @export
is_taxon <- function(x) {
  inherits(x, "taxa_taxon")
}

#' @export
is.na.taxa_taxon <- function(x) {
  is.na(vctrs::vec_cast(x, character()))
}


#--------------------------------------------------------------------------------
# Internal utility functions
#--------------------------------------------------------------------------------

