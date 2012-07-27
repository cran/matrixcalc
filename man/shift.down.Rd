\name{shift.down}
\alias{shift.down}
\title{ Shift matrix m rows down }
\description{
  This function returns a matrix that has had its rows shifted downwards filling the above rows
  with the given fill value.
}
\usage{
shift.down(A, rows = 1, fill = 0)
}
\arguments{
  \item{A}{ a matrix }
  \item{rows}{ the number of rows to be shifted }
  \item{fill}{ the fill value which as a default is zero }
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
shift.down( A, 1 )
shift.down( A, 3 )
}
\keyword{ math }
