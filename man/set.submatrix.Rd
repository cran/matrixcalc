\name{set.submatrix}
\alias{set.submatrix}
\title{ Store matrix inside another matrix }
\description{
  This function returns a matrix which is a copy of matrix x into which the contents of matrix y
  have been inserted at the given row and column.
}
\usage{
set.submatrix(x, y, row, col)
}
\arguments{
  \item{x}{ a matrix }
  \item{y}{ a matrix }
  \item{row}{ an integer row number }
  \item{col}{ an integer column number }
}
\value{
  A matrix.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument x is not a numeric matrix, then the function presents an error message and stops.
  If the argument y is not a numeric matrix, then the function presents an error message and stops.
  If the argument row is not a positive integer, then the function presents an error message and stops.
  If the argument col is not a positive integer, then the function presents an error message and stops.
  If the target row range does not overlap with the row range of argument x, then the function presents an error message and stops.
  If the target col range does not overlap with the col range of argument x, then the function presents an error message and stops.
}
\examples{
x <- matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE )
y <- matrix( seq( 1, 4, 1 ), nrow=2, byrow=TRUE )
z <- set.submatrix( x, y, 3, 3 )
}
\keyword{ math }
