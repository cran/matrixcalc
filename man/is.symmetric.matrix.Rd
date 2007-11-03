\name{is.symmetric.matrix}
\alias{is.symmetric.matrix}
\title{ Test for symmetric numeric matrix }
\description{
  This function returns TRUE of the argument is a numeric symmetric square matrix and FALSE otherwise.
}
\usage{
is.symmetric.matrix(x)
}
\arguments{
  \item{x}{ an R object }
}
\value{
  TRUE or FALSE.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{ ~~further notes~~ 
  If the argument is not a numeric matrix, the function displays an error message and stops.
  If the argument is not a square matrix, the function displays an error message and stops.
}
\seealso{
  \code{\link{is.numeric.matrix}},
  \code{\link{is.square.matrix}}
}
\examples{
A <- matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
is.symmetric.matrix( A )
B <- matrix( c( 1, 2, 2, 1 ), nrow=2, byrow=TRUE )
is.symmetric.matrix( B )
}
\keyword{ math }
