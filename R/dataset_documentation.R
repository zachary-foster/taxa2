#' Example of UNITE fungal ITS data
#'
#' A dataset containing information from 500 sequences from the UNITE reference database.
#'
#' @examples
#' \dontrun{
#'
#' file_path <- system.file("extdata", "unite_general_release.fasta", package = "metacoder")
#' sequences <- ape::read.FASTA(file_path)
#' unite_ex_data <- extract_taxonomy(sequences,
#'                                   regex = "^(.*)\\|(.*)\\|(.*)\\|.*\\|(.*)$",
#'                                   key = c(seq_name = "obs_info", seq_id = "obs_info",
#'                                           other_id = "obs_info", "class"),
#'                                   class_regex = "^(.*)__(.*)$",
#'                                   class_key = c(unite_rank = "taxon_info", "name"),
#'                                   class_sep = ";")
#' }
#'
#' @format An object of type \code{\link{taxmap}}
#' @source \url{https://unite.ut.ee/}
"unite_ex_data"
