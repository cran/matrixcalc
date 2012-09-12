\name{is.skew.symmetric.matrix}
\alias{is.skew.symmetric.matrix}
\title{ Test for a skew-symmetric matrix }
\description{
  This function returns \code{TRUE} if the matrix argument x is
  a skew symmetric matrix, i.e., the transpose of the matrix is
  the negative of the matrix.  Otherwise, \code{FALSE} is returned.
}
\usage{
is.skew.symmetric.matrix(x, tol = 1e-08)
}
\arguments{
  \item{x}{ a numeric square matrix }
  \item{tol}{ a numeric tolerance level usually left out }
}
\details{
  Let \eqn{{\bf{x}}} be an order \eqn{n} matrix.  If every element
  of the matrix \eqn{{\bf{x}} + {\bf{x'}}} in absolute value is less
  than the given tolerance, then the matrix argument is declared to be
  skew symmetric.
}
\value{
  A TRUE or FALSE value.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.

  Horn, R. A. and C. R. Johnson (1990). \emph{Matrix Analysis}, Cambridge University Press.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
A <- diag( 1, 3 )
is.skew.symmetric.matrix( A )
B <- matrix( c( 0, -2, -1, -2, 0, -4, 1, 4, 0 ), nrow=3, byrow=TRUE )
is.skew.symmetric.matrix( B )
C <- matrix( c( 0, 2, 1, 2, 0, 4, 1, 4, 0 ), nrow=3, byrow=TRUE )
is.skew.symmetric.matrix( C )
}
\keyword{ math }
