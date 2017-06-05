#' @examples
#' # Hierarchy class
#' ex_hierarchy1
#'
#' ## ranks
#' ### keep all taxa between order and genus
#' span(ex_hierarchy1, ranks(order:genus))
#' ## taxon names
#' ### keep all taxa between Poaceae and Poa
#' ### - matches to ranks first
#' ex_hierarchy1 %>% span(nms(Poaceae, Poa))
#' ex_hierarchy1 %>% span(nms(Poaceae:Poa))
#' ## taxon ids
#' ### keep all taxa between 4479 and 4544 taxonomic IDs
#' ### - matches to ranks first
#' ex_hierarchy1 %>% span(ids(4479, 4544))
#' ex_hierarchy1 %>% span(ids(4479:4544))
#'
#'
#' # hierarchies class
#' # single taxonomic group
#' invisible(lapply(hiers, print))
#' hiers %>% span(family) %>% lapply(., print) %>% invisible
#'
#' ## more than one taxonomic group
#' hier1 <- hierarchy(z, y, x)
#' hier2 <- hierarchy(c, b, a)
#' hiers <- hierarchies(hier1, hier2)
#' invisible(lapply(hiers, print))
#' hiers %>% span(family, genus) %>% lapply(., print) %>% invisible
