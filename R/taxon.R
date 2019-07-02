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
#' @param id The ids of taxa. These should be unique identifier and are usually associated with a
#'   database. Inputs with be coerced into a [taxon_id] vector if anything else is given.
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


#' Taxon class
#'
#' \Sexpr[results=rd, stage=render]{taxa:::lifecycle("experimental")}
#' Used to store information about taxa, such as names, ranks, id, and other arbitrary data.
#'
#' @param ... Used to pass arguments to methods and allow methods to used additional arguments.
#'
#' @importFrom vctrs %<-%
#'
#' @return An `S3` object of class `taxa_taxon`
#' @family classes
#'
#' @examples
#'
#' # Create taxon vector
#' x <- taxon(c('A', 'B', 'C'))
#' x <- taxon(name = c('Homo sapiens', 'Bacillus', 'Ascomycota', 'Ericaceae'),
#'            rank = c('species', 'genus', 'phylum', 'family'),
#'            id = taxon_id(c('9606', '1386', '4890', '4345'), db = 'ncbi'),
#'            auth = c('Linnaeus, 1758', 'Cohn 1872', NA, 'Juss., 1789'),
#'            info = list(list(n = 1), list(n = 3), list(n = 2), list(n = 9)))
#'
#' # Get parts of the taxon vector
#' taxon_name(x)
#' taxon_rank(x)
#' taxon_id(x)
#' taxon_auth(x)
#' taxon_info(x)
#' taxon_info(x, 'n')
#'
#' # Set parts of the taxon vector
#' taxon_name(x) <- tolower(taxon_name(x))
#' taxon_rank(x)[1] <- NA
#' taxon_db(taxon_id(x)) <- 'itis'
#' taxon_auth(x) <- NA
#' taxon_info(x)[3:4] <- list(list(count = NA))
#' taxon_info(x, 'n')[1:2] <- c(12, 30)
#'
#' @export
taxon <- function(...) {
  UseMethod("taxon")
}


#' @rdname taxon
#'
#' @param name The names of taxa. Inputs with be coerced into a [taxon_name] vector if anything else
#'   is given.
#' @param rank The ranks of taxa. Inputs with be coerced into a [taxon_rank] vector if anything else
#'   is given.
#' @param id The ids of taxa. These should be unique identifier and are usually associated with a
#'   database. Inputs with be coerced into a [taxon_id] vector if anything else is given.
#' @param auth The authority of the taxon. Inputs with be coerced into a [character] vector if
#'   anything else is given.
#' @param info A list of arbitrary, user-defined attributes associated with each taxon. Each element
#'   in the list, one per taxon, should be a named list of zero or more items with unique names.
#'   Values in this list can be accessed with the [taxon_info] function. All elements in the list do
#'   not need to contain the same attributes.
#'
#' @export
taxon.default <- function(name = taxon_name(), rank = NA, id = NA, auth = NA, info = list(NULL), ...) {
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


#' @rdname taxon
#'
#' @param x An object to with taxa.
#' @param value The taxa to set. Inputs will be coerced into a [taxon()] vector.
#'
#' @export
`taxon<-` <- function(x, value) {
  UseMethod('taxon<-')
}


setOldClass(c("taxa_taxon", "vctrs_vctr"))



#--------------------------------------------------------------------------------
# S3 getters/setters
#--------------------------------------------------------------------------------


#' @rdname taxon_rank
#' @export
`levels<-.taxa_taxon` <- function(x, value) {
  levels(vctrs::field(x, 'rank')) <- value
  return(x)
}


#' @rdname taxon_rank
#' @export
levels.taxa_taxon <- function(x) {
  levels(vctrs::field(x, 'rank'))
}


#' @rdname taxon_id
#' @export
`taxon_id<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_id())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "id") <- value
  return(x)
}


#' @rdname taxon_id
#' @export
taxon_id.taxa_taxon <- function(x, ...) {
  vctrs::field(x, "id")
}


#' @rdname taxon_name
#' @export
`taxon_name<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_name())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "name") <- value
  return(x)
}


#' @rdname taxon_name
#' @export
taxon_name.taxa_taxon <- function(x, ...) {
  vctrs::field(x, "name")
}


#' @rdname taxon_rank
#' @export
`taxon_rank<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, taxon_rank())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "rank") <- value
  return(x)
}


#' @rdname taxon_rank
#' @export
taxon_rank.taxa_taxon <- function(x, ...) {
  vctrs::field(x, "rank")
}


#' Set and get taxon authorities
#'
#' Set and get taxon authorities in objects that have them, such as [taxon()] objects.
#'
#' @param x An object with taxon authorities.
#'
#' @return A [character()] vector
#'
#' @export
taxon_auth <- function(x) {
  UseMethod('taxon_auth')
}


#' @rdname taxon_auth
#' @export
taxon_auth.taxa_taxon <- function(x) {
  vctrs::field(x, "auth")
}


#' @rdname taxon_auth
#'
#' @param value The taxon authorities to set. Inputs will be coerced into a [character()] vector.
#'
#' @export
`taxon_auth<-` <- function(x, value) {
  UseMethod('taxon_auth<-')
}


#' @rdname taxon_auth
#' @export
`taxon_auth<-.taxa_taxon` <- function(x, value) {
  value <- vctrs::vec_cast(value, character())
  value <- vctrs::vec_recycle(value, length(x))
  vctrs::field(x, "auth") <- value
  return(x)
}


#' @rdname taxon_info
#' @export
`taxon_info<-.taxa_taxon` <- function(x, key = NULL, value) {
  if (is.null(key)) {
    value <- vctrs::vec_cast(value, taxon_info())
    value <- vctrs::vec_recycle(value, length(x))
    vctrs::field(x, "info") <- value
  } else {
    value <- vctrs::vec_recycle(value, length(x))
    vctrs::field(x, "info") <- mapply(vctrs::field(x, "info"), value,
                                      SIMPLIFY = FALSE,
                                      FUN = function(l, v) {
                                        l[key] <- v
                                        return(l)
                                      })
  }
  return(x)
}


#' @rdname taxon_info
#'
#' @param key The name of the item to get from the info for each taxon. If the element with the name
#'   specified is present, `NULL` will be returned/set for those taxa. If a key is not supplied
#'   (default), then all info for each taxon will be returned/set.
#'
#' @export
taxon_info.taxa_taxon <- function(x, key = NULL, ...) {
  out <- as.list(vctrs::field(x, "info"))
  if (is.null(key)) {
    return(out)
  } else {
    return(lapply(out, function(l) {
      l[[key]]
    }))
  }
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
  out <- paste0(out, ifelse(is.na(auth), '', paste0(' ', font_secondary(auth))))
  out <- paste0(ifelse(is.na(id), '', paste0(font_secondary(id), font_punct('|'))),
                out)
  out <- paste0(out, ifelse(is.na(rank), '',
                            paste0(font_punct('|'),
                                   printed_taxon_rank(rank, color = TRUE, add_db = FALSE))))
  if (! color) {
    out <- crayon::strip_style(out)
  }
  return(out)
}


#' @rdname taxa_printing_funcs
#' @rdname taxon
#' @export
#' @keywords internal
format.taxa_taxon <- function(x, ...) {
  printed_taxon(x, color = FALSE)
}


#' @rdname taxa_printing_funcs
#' @rdname taxon
#' @export
#' @keywords internal
obj_print_data.taxa_taxon <- function(x) {
  if (length(x) == 0) {
    return()
  }
  out <- printed_taxon(x, color = TRUE)
  print_with_color(out, quote = FALSE)
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
obj_print_footer.taxa_taxon <- function(x) {
  # print taxon rank levels
  vctrs::obj_print_footer(vctrs::field(x, 'rank'))

  # print databases used in names, ranks, and ids
  db_types <- c(name = 'name', rank = 'rank', id = 'id')
  dbs <- lapply(db_types, function(db) {
    as.character(vctrs::field(vctrs::field(x, db), 'db'))
  })
  db_per_type <- unique(utils::stack(dbs))
  db_per_type <- db_per_type[!is.na(db_per_type$values), ]
  if (nrow(db_per_type) > 0) {
    type_per_db <- split(db_per_type$ind, db_per_type$values)
    db_printed <- vapply(seq_len(length(type_per_db)), FUN.VALUE = character(1), function(i) {
      db_name <- names(type_per_db)[i]
      db_types <- paste0('(', paste0(type_per_db[[i]], collapse = ','), ')')
      db_types <- font_secondary(db_types)
      paste0(db_name, db_types)
    })
    cat(paste0('Databases: ', paste0(db_printed, collapse = ' ')))
  }
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_abbr.taxa_taxon <- function(x) {
  "taxon"
}


#' @rdname taxa_printing_funcs
#' @export
#' @keywords internal
vec_ptype_full.taxa_taxon <- function(x) {
  "taxon"
}


#' @rdname taxa_printing_funcs
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

#' @rdname taxa_coercion_funcs
#' @method vec_type2 taxa_taxon
#' @importFrom vctrs vec_type2
#' @export
#' @export vec_type2.taxa_taxon
#' @keywords internal
vec_type2.taxa_taxon <- function(x, y, ...) UseMethod("vec_type2.taxa_taxon", y)


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon default
#' @export
vec_type2.taxa_taxon.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon vctrs_unspecified
#' @export
vec_type2.taxa_taxon.vctrs_unspecified <- function(x, y, ...) x


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon taxa_taxon
#' @export
vec_type2.taxa_taxon.taxa_taxon <- function(x, y, ...) new_taxon()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon character
#' @export
vec_type2.taxa_taxon.character <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.character taxa_taxon
#' @importFrom vctrs vec_type2.character
#' @export
vec_type2.character.taxa_taxon <- function(x, y, ...) character()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.taxa_taxon factor
#' @export
vec_type2.taxa_taxon.factor <- function(x, y, ...) factor()


#' @rdname taxa_coercion_funcs
#' @method vec_type2.factor taxa_taxon
#' @importFrom vctrs vec_type2.factor
#' @export
vec_type2.factor.taxa_taxon <- function(x, y, ...) factor()



#--------------------------------------------------------------------------------
# S3 casting functions
#--------------------------------------------------------------------------------

#' @rdname taxa_casting_funcs
#' @method vec_cast taxa_taxon
#' @importFrom vctrs vec_cast
#' @export
#' @export vec_cast.taxa_taxon
#' @keywords internal
vec_cast.taxa_taxon <- function(x, to, x_arg, to_arg) UseMethod("vec_cast.taxa_taxon")


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon default
#' @export
vec_cast.taxa_taxon.default <- function(x, to, x_arg, to_arg) vctrs::vec_default_cast(x, to, x_arg, to_arg)


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon taxa_taxon
#' @export
vec_cast.taxa_taxon.taxa_taxon <- function(x, to, x_arg, to_arg) x


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon character
#' @export
vec_cast.taxa_taxon.character <- function(x, to, x_arg, to_arg) taxon(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.character taxa_taxon
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.character.taxa_taxon <- function(x, to, x_arg, to_arg) as.character(vctrs::field(x, "name"))


#' @rdname taxa_casting_funcs
#' @method vec_cast.taxa_taxon factor
#' @export
vec_cast.taxa_taxon.factor <- function(x, to, x_arg, to_arg) taxon(x)


#' @rdname taxa_casting_funcs
#' @method vec_cast.factor taxa_taxon
#' @importFrom vctrs vec_cast.factor
#' @export
vec_cast.factor.taxa_taxon <- function(x, to, x_arg, to_arg) as.factor(vctrs::field(x, "name"))



#--------------------------------------------------------------------------------
# S3 equality and comparison functions
#--------------------------------------------------------------------------------

#' @rdname taxa_comparison_funcs
#' @export
#' @keywords internal
vec_proxy_compare.taxa_taxon <- function(x, ...) {
  data.frame(stringsAsFactors = FALSE,
             rank = as.character(taxon_rank(x)),
             name = as.character(taxon_name(x)),
             id   = as.character(taxon_id(x)))
}


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

