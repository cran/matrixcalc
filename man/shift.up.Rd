\name{shift.up}
\alias{shift.up}
\title{ Shift matrix m rows up }
\description{
  This function returns a matrix where the argument as been shifted up the given number of rows filling
  the bottom rows with the given fill value.
}
\usage{
shift.up(A, rows = 1, fill = 0)
}
\arguments{
  \item{A}{ a matrix }
  \item{rows}{ integer number of rows}
  \item{fill}{ fill value which as the default value of zero }
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
shift.up( A, 1 )
shift.up( A, 3 )
}
\keyword{ math }
