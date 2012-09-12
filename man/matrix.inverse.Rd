\name{matrix.inverse}
\alias{matrix.inverse}
\title{ Inverse of a square matrix }
\description{
  This function returns the inverse of a square matrix computed using the R function solve.
}
\usage{
matrix.inverse(x)
}
\arguments{
  \item{x}{ a square numeric matrix }
}
\value{
  A matrix.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
A <- matrix( c ( 1, 2, 2, 1 ), nrow=2, byrow=TRUE)
print( A )
invA <- matrix.inverse( A )
print( invA )
print( A \%*\% invA )
print( invA \%*\% A )
}
\keyword{ math }
