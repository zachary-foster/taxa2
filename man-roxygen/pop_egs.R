#' @examples
#' # With Hierarchy class object
#' ex_hierarchy1
#' ## ranks
#' pop(ex_hierarchy1, ranks(family))
#' ex_hierarchy1 %>% pop(ranks(family))
#' ex_hierarchy1 %>% pop(ranks(family, genus))
#' ## taxon names
#' ex_hierarchy1 %>% pop(nms(Poa))
#' ex_hierarchy1 %>% pop(nms(Poaceae, Poa))
#' ## taxon ids
#' ex_hierarchy1 %>% pop(ids(4479))
#' ex_hierarchy1 %>% pop(ids(4479, 4544))
#' ## mixed: ids and names
#' ex_hierarchy1 %>% pop(ranks(family), ids(4544))
#'
#' # With hierarchies class object
#' # single taxonomic group
#' invisible(lapply(ex_hierarchies, print))
#' ex_hierarchies %>% pop(family) %>% lapply(., print) %>% invisible
#' ## more than one taxonomic group
#' invisible(lapply(ex_hierarchies, print))
#' ex_hierarchies %>% pop(ranks(family, genus)) %>% lapply(., print) %>%
#'   invisible
#'
#' # Taxonomy class
#' # ex_taxonomy$pop(ranks(family), NULL, NULL)
#' # pop(ex_taxonomy, ranks(family))
#' # pop(ex_taxonomy, ranks(family, genus))
#' # taxon names
#' # pop(ex_taxonomy, nms(Poa))
#' # pop(ex_taxonomy, nms(Poaceae, Poa))
#' # taxon ids
#' # pop(ex_taxonomy, ids(4479))
#' # pop(ex_taxonomy, ids(4479, 4544))
#' # mixed: ids and names
#' # pop(ex_taxonomy, ranks(family), ids(4544))
