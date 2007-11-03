\name{hilbert.matrix}
\alias{hilbert.matrix}
\title{ Hilbert matrices }
\description{
  This function returns an n by n Hilbert matrix.
}
\usage{
hilbert.matrix(n)
}
\arguments{
  \item{n}{ Order of the Hilbert matrix }
}
\value{
  A matrix.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) Matrix Differential Calculus with Applications in Statistics and Econometrics,
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument is less than or equal to zero, the function displays an error message and stops.
  If the argument is not an integer, the function displays an error message and stops.
}
\examples{
H <- hilbert.matrix( 4 )
}
\keyword{ math }
