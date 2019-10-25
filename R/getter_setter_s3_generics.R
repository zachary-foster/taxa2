#' Set and get taxon authors
#'
#' Set and get taxon authors in objects that have them, such as [taxon_authority()] objects.
#'
#' @param x An object with taxon authors.
#'
#' @export
tax_author <- function(x) {
  UseMethod('tax_author')
}

#' @rdname tax_author
#'
#' @param value The taxon authors to set. Inputs will be coerced into a [character()] vector.
#'
#' @export
`tax_author<-` <- function(x, value) {
  UseMethod('tax_author<-')
}



#' Set and get taxon authority dates
#'
#' Set and get the taxon authority dates in objects that have them, such as [taxon_authority()] objects.
#'
#' @param x An object with taxon authority dates.
#'
#' @export
tax_date <- function(x) {
  UseMethod('tax_date')
}

#' @rdname tax_date
#'
#' @param value The taxon authority dates to set. Inputs will be coerced into a [character()] vector.
#'
#' @export
`tax_date<-` <- function(x, value) {
  UseMethod('tax_date<-')
}



#' Set and get taxon authority citations
#'
#' Set and get the taxon authority citations in objects that have them, such as [taxon_authority()] objects.
#'
#' @param x An object with taxon authority dates.
#'
#' @export
tax_cite <- function(x) {
  UseMethod('tax_cite')
}

#' @rdname tax_cite
#'
#' @param value The taxon citations to set. Inputs will be coerced into a [taxon_authority()] vector.
#'
#' @export
`tax_cite<-` <- function(x, value) {
  UseMethod('tax_cite<-')
}



#' Set and get taxon ID databases
#'
#' Set and get the taxon ID databases in objects that have them, such as [taxon_id()] objects.
#'
#' @param x An object with taxon authority dates.
#'
#' @export
tax_db <- function(x) {
  UseMethod('tax_db')
}

#' @rdname tax_db
#'
#' @param value The taxon citations to set. Inputs will be coerced into a [taxon_db()] vector.
#'
#' @export
`tax_db<-` <- function(x, value) {
  UseMethod('tax_db<-')
}



#' Set and get taxon names
#'
#' Set and get the taxon names in objects that have them, such as [taxon_name()] objects.
#' Note that this is not the same as adding vector names with [names()].
#'
#' @param x An object with taxon names.
#'
#' @export
tax_name <- function(x) {
  UseMethod('tax_name')
}

#' @rdname tax_name
#'
#' @param value The taxon names to set. Inputs will be coerced into a [character()] vector.
#'
#' @export
`tax_name<-` <- function(x, value) {
  UseMethod('tax_name<-')
}



#' Set and get taxon IDs
#'
#' Set and get the taxon IDs in objects that have them, such as [taxon_name()] objects.
#'
#' @param x An object with taxon IDs.
#'
#' @export
tax_id <- function(x) {
  UseMethod('tax_id')
}

#' @rdname tax_id
#'
#' @param value The taxon IDs to set. Inputs will be coerced into a [taxon_id()] vector.
#'
#' @export
`tax_id<-` <- function(x, value) {
  UseMethod('tax_id<-')
}



#' Set and get taxon authorities
#'
#' Set and get the taxon authorities in objects that have them, such as [taxon_name()] objects.
#' Note that this sets all the authority information, such as author name, date, and citations.
#' To set or get just one of part of the authorities, use [tax_author()], [tax_date()], or [tax_cite()] instead.
#'
#' @param x An object with taxon authorities.
#'
#' @export
tax_auth <- function(x) {
  UseMethod('tax_auth')
}

#' @rdname tax_auth
#'
#' @param value The taxon IDs to set. Inputs will be coerced into a [taxon_id()] vector.
#'
#' @export
`tax_auth<-` <- function(x, value) {
  UseMethod('tax_auth<-')
}



#' Set and get taxon ranks
#'
#' Set and get the taxon ranks in objects that have them, such as [taxon_name()] objects.
#'
#' @param x An object with taxon ranks.
#'
#' @export
tax_rank <- function(x) {
  UseMethod('tax_rank')
}

#' @rdname tax_rank
#'
#' @param value The taxon ranks to set. Inputs will be coerced into a [taxon_rank()] vector.
#'
#' @export
`tax_rank<-` <- function(x, value) {
  UseMethod('tax_rank<-')
}
