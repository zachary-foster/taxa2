#' @examples
#' # hierarchy
#' (x <- taxon(
#'   name = taxon_name("Poaceae"),
#'   rank = taxon_rank("family"),
#'   id = taxon_id(4479)
#' ))
#'
#' (y <- taxon(
#'   name = taxon_name("Poa"),
#'   rank = taxon_rank("genus"),
#'   id = taxon_id(4544)
#' ))
#'
#' (z <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' ))
#'
#' (res <- hierarchy(z, y, x))
#'
#' ## single taxonomic group
#' res %>% pick(family)
#' ### more than 1 - remake res object above first
#' res %>% pick(family, genus)
#'
#'
#' # hierarchies
#' x <- taxon(
#'   name = taxon_name("Poaceae"),
#'   rank = taxon_rank("family"),
#'   id = taxon_id(4479)
#' )
#' y <- taxon(
#'   name = taxon_name("Poa"),
#'   rank = taxon_rank("genus"),
#'   id = taxon_id(4544)
#' )
#' z <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' )
#' hier1 <- hierarchy(z, y, x)
#'
#' a <- taxon(
#'   name = taxon_name("Felidae"),
#'   rank = taxon_rank("family"),
#'   id = taxon_id(9681)
#' )
#' b <- taxon(
#'   name = taxon_name("Puma"),
#'   rank = taxon_rank("genus"),
#'   id = taxon_id(146712)
#' )
#' c <- taxon(
#'   name = taxon_name("Puma concolor"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(9696)
#' )
#' hier2 <- hierarchy(c, b, a)
#' hiers <- hierarchies(hier1, hier2)
#'
#' # single taxonomic group
#' invisible(lapply(hiers, print))
#' hiers %>% pick(family) %>% lapply(., print) %>% invisible
#'
#' ## more than one taxonomic group
#' hier1 <- hierarchy(z, y, x)
#' hier2 <- hierarchy(c, b, a)
#' hiers <- hierarchies(hier1, hier2)
#' invisible(lapply(hiers, print))
#' hiers %>% pick(family, genus) %>% lapply(., print) %>% invisible
