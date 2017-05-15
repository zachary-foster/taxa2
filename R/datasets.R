#' A data.frame representing a taxonomic classification.
#'
#' Data is from GBIF for the taxon *Bison bison*
#'
#' It has the following columns
#'
#' \itemize{
#'   \item name. taxon name
#'   \item rank. taxon rank
#'   \item id. taxon id
#' }
#'
#' @name bison_bison
#' @docType data
#' @keywords data
NULL

#' Lookup-table for IDs of taxonomic ranks
#'
#' Composed of two columns:
#' \itemize{
#'  \item rankid - the ordered identifer value. lower values mean higher rank
#'  \item ranks - all the rank names that belong to the same level, with
#'  different variants that mean essentially the same thing
#' }
#'
#' @name ranks_ref
#' @docType data
#' @keywords data
NULL
