\name{toeplitz.matrix}
\alias{toeplitz.matrix}
\title{ Toeplitz Matrix }
\description{
  This function constructs an order n Toeplitz matrix from the values in
  the order 2 * n - 1 vector x.
}
\usage{
toeplitz.matrix(n, x)
}
\arguments{
  \item{n}{ a positive integer value for order of matrix greater than 1 }
  \item{x}{ a vector of values used to construct the matrix }
}
\details{
  The element \code{T[i,j]} in the Toeplitz matrix is \code{x[i-j+n]}.
}
\value{
  An order n matrix.
}
\references{
  Monahan, J. F. (2011). \emph{Numerical Methods of Statistics}, Cambridge
  University Press.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument n is not a positive integer, the function presents an error message and stops.
  If the length of x is not equal to 2 * n - 1, the function presents an error message and stops.
}
\examples{
T <- toeplitz.matrix( 4, seq( 1, 7 ) )
print( T )
}
\keyword{ math }
