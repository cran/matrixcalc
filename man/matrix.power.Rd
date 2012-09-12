\name{matrix.power}
\alias{matrix.power}
\title{ Matrix Raised to a Power }
\description{
  This function computes the k-th power of order n square matrix x
  If k is zero, the order n identity matrix is returned.  argument k
  must be an integer.
}
\usage{
matrix.power(x, k)
}
\arguments{
  \item{x}{ a numeric square matrix }
  \item{k}{ a numeric exponent }
}
\details{
  The matrix power is computed by successive matrix multiplications.  If the
  exponent is zero, the order n identity matrix is returned.  If the exponent
  is negative, the inverse of the matrix is raised to the given power.
}
\value{
  An order \eqn{n} matrix.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
A <- matrix( c ( 1, 2, 2, 1 ), nrow=2, byrow=TRUE)
matrix.power( A, -2 )
matrix.power( A, -1 )
matrix.power( A, 0 )
matrix.power( A, 1 )
matrix.power( A, 2 )
}
\keyword{ math }
