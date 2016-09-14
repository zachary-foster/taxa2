#' @examples
#' # Mammalia
#' ## Notoryctidae
#' x <- taxon(
#'   name = taxon_name("Notoryctidae"),
#'   rank = taxon_rank("family"),
#'   id = taxon_id(4479)
#' )
#' y <- taxon(
#'   name = taxon_name("Notoryctes"),
#'   rank = taxon_rank("genus"),
#'   id = taxon_id(4544)
#' )
#' z <- taxon(
#'   name = taxon_name("Notoryctes typhlops"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' )
#'
#' # Mammalia
#' ## Felidae
#' a <- taxon(
#'   name = taxon_name("Mammalia"),
#'   rank = taxon_rank("class"),
#'   id = taxon_id(9681)
#' )
#' b <- taxon(
#'   name = taxon_name("Felidae"),
#'   rank = taxon_rank("family"),
#'   id = taxon_id(9681)
#' )
#'
#' c <- taxon(
#'   name = taxon_name("Puma"),
#'   rank = taxon_rank("genus"),
#'   id = taxon_id(146712)
#' )
#' d <- taxon(
#'   name = taxon_name("Puma concolor"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(9696)
#' )
#'
#' m <- taxon(
#'   name = taxon_name("Panthera"),
#'   rank = taxon_rank("genus"),
#'   id = taxon_id(146712)
#' )
#' n <- taxon(
#'   name = taxon_name("Panthera tigris"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(9696)
#' )
#'
#' # make hierarchies
#' (hier1 <- hierarchy(z, y, x, a))
#' (hier2 <- hierarchy(c, b, a, d))
#' (hier3 <- hierarchy(n, m, b, a))
#'
#' (hrs <- hierarchies(hier1, hier2, hier3))
#'
#' # make taxonomy
#' taxonomy(hier1, hier2, hier3)
#'
#'
#'
