% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxonomy.R
\name{roots}
\alias{roots}
\title{Get root taxa}
\usage{
roots(x, subset = NULL)
}
\arguments{
\item{x}{An object containing taxonomic relationships, such as \link{taxonomy} objects.}

\item{subset}{The subset of the tree to search for roots to that subset. Can be indexes or names.}
}
\description{
Get the indexes of root taxa in a taxonomy.
}
\examples{
x <- taxonomy(c('Carnivora', 'Felidae', 'Panthera', 'Panthera leo',
                'Panthera tigris', 'Ursidae', 'Ursus', 'Ursus arctos'),
              supertaxa = c(NA, 1, 2, 3, 3, 1, 6, 7))
roots(x)
roots(x, subset = 2:8)

}
\seealso{
Other taxonomy functions: 
\code{\link{internodes}()},
\code{\link{leaves}()},
\code{\link{stems}()},
\code{\link{subtaxa}()},
\code{\link{supertaxa}()}

Other root functions: 
\code{\link{is_root}()}
}
\concept{root functions}
\concept{taxonomy functions}
