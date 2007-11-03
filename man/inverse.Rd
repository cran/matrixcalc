\name{inverse}
\alias{inverse}
\title{ Inverse of a square matrix }
\description{
  This function returns the inverse of a square matrix computed using the R function solve.
}
\usage{
inverse(x)
}
\arguments{
  \item{x}{ a square numeric matrix }
}
\value{
  A matrix.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
A <- matrix( c ( 1, 2, 2, 1 ), nrow=2, byrow=TRUE)
invA <- inverse( A )
}
\keyword{ math }
