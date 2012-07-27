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
\details{
  A Hilbert matrix is an order \eqn{n} square matrix of unit fractions with elements 
  defined as \eqn{{H_{i,j}} = \frac{1}{{i + j - 1}}}.
}
\value{
  A matrix.
}
\references{
  Hilbert, David (1894). Ein Beitrag zur Theorie des Legendre schen Polynoms,
  \emph{Acta Mathematica}, Springer, Netherlands, 18, 155-159.
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
