\name{hankel.matrix}
\alias{hankel.matrix}
\title{ Hankel Matrix }
\description{
  This function constructs an order n Hankel matrix from the values in
  the order n vector x.  Each row of the matrix is a circular shift of
  the values in the previous row.
}
\usage{
hankel.matrix(n, x)
}
\arguments{
  \item{n}{ a positive integer value for order of matrix greater than 1 }
  \item{x}{ a vector of values used to construct the matrix }
}
\details{
  A Hankel matrix is a square matrix with constant skew diagonals.
  The determinant of a Hankel matrix is called a catalecticant.
  Hankel matrices are formed when the hidden Mark model is sought
  from a given sequence of data.
}
\value{
  An order \eqn{n} matrix.
}
\references{
  Power, S. C. (1982). \emph{Hankel Operators on Hilbert Spaces}, Research
  notes in mathematics, Series 64, Pitman Publishing.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument n is not a positive integer, the function presents an error message and stops.
  If the length of x is less than n, the function presents an error message and stops.
}
\examples{
H <- hankel.matrix( 4, seq( 1, 7 ) )
print( H )
}
\keyword{ math }
