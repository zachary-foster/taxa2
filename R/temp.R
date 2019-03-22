
# file_path <- system.file("extdata", "unite_general_release.fasta", package = "metacoder")
# sequences <- ape::read.FASTA(file_path)
# library(taxa) # The parsers in taxa are used
# unite_ex_data_2 <- extract_tax_data(names(sequences)[1],
#                                     regex = "^(.*)\\|(.*)\\|(.*)\\|.*\\|(.*)$",
#                                     key = c(seq_name = "info", seq_id = "info",
#                                             other_id = "info", my_class = "class"),
#                                     class_regex = "^(.*)__(.*)$",
#                                     class_key = c(unite_rank = "info", my_name = "taxon_name"),
#                                     class_sep = ";",
#                                     database = "ncbi")


#' @importFrom pillar type_sum
#' @export
type_sum.Taxa <- function(x) {
  "Taxa"
}


#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.Taxa <- function(x, ...) {
  out <- as.character(x)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @importFrom pillar is_vector_s3
#' @export
is_vector_s3.Taxa <- function(x) FALSE

#' @importFrom pillar obj_sum
#' @export
obj_sum.Taxa <- function(x) {
  rep("Taxa", length(x))
}