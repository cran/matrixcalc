\name{shift.left}
\alias{shift.left}
\title{ Shift a matrix n columns to the left }
\description{
  This function returns a matrix that has been shifted n columns to the left
  filling the subsqeuent columns with the given fill value
}
\usage{
shift.left(A, cols = 1, fill = 0)
}
\arguments{
  \item{A}{ a matrix }
  \item{cols}{ integer number of columns to be shifted to the left }
  \item{fill}{ the fill value which as as a default zero }
}
\value{
  A matrix.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument A is not a numeric matrix, then the function presents an error message and stops.
  If the argument cols is not a positive integer, then the function presents an error message and stops.
}
\examples{
A <- matrix( seq( 1, 12, 1 ), nrow=3, byrow=TRUE )
shift.left( A, 1 )
shift.left( A, 2 )
}
\keyword{ math }
