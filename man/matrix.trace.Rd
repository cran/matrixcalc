\name{matrix.trace}
\alias{matrix.trace}
\title{ The trace of a matrix }
\description{
  This function returns the trace of a given square numeric matrix.
}
\usage{
matrix.trace(x)
}
\arguments{
  \item{x}{ a matrix }
}
\value{
  A numeric value which is the sum of the values on the diagonal.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument A is not a square matrix, then the function presents an error message and stops.
}
\seealso{
  \code{\link{is.numeric.matrix}}
}
\examples{
A <- matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE )
matrix.trace( A )
}
\keyword{ math }
