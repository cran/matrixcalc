\name{shift.right}
\alias{shift.right}
\title{Shift matrix n columns to the right }
\description{
  This function returns a matrix that has been shifted to the right n columns
  filling the previous columns with the given fill value.
}
\usage{
shift.right(A, cols = 1, fill = 0)
}
\arguments{
  \item{A}{ a matrix }
  \item{cols}{ integer number of columns to be shifted to the right }
  \item{fill}{ the fill which as default value zero }
}
\value{
  A matrix.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument A is not a numeric matrix, then the function presents an error message and stops.
  If the argument rows is not a positive integer, then the function presents an error message and stops.
}
\examples{
A <- matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE )
shift.right( A, 1 )
shift.right( A, 2 )
}
\keyword{ math }
