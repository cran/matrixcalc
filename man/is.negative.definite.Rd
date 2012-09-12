\name{is.negative.definite}
\alias{is.negative.definite}
\title{ Test matrix for negative definiteness }
\description{
  This function returns TRUE if the argument, a square symmetric real matrix x, is negative definite.  
}
\usage{
is.negative.definite(x, tol=1e-8)
}
\arguments{
  \item{x}{ a matrix }
  \item{tol}{ a numeric tolerance level }
}
\details{
  For a negative definite matrix, the eigenvalues should be negative. The R function \code{eigen}
  is used to compute the eigenvalues. If any of the eigenvalues in absolute value is less than
  the given tolerance, that eigenvalue is replaced with zero. If any of the eigenvalues is greater than or equal to zero,
  then the matrix is not negative definite.   Otherwise, the matrix is declared to be negative definite.
}
\value{
  TRUE or FALSE.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{is.positive.definite}}, 
  \code{\link{is.positive.semi.definite}}, 
  \code{\link{is.negative.semi.definite}}, 
  \code{\link{is.indefinite}}
}
\examples{
###
### identity matrix is always positive definite
I <- diag( 1, 3 )
is.negative.definite( I )
###
### positive definite matrix
### eigenvalues are 3.4142136 2.0000000 0.585786
###
A <- matrix( c( 2, -1, 0, -1, 2, -1, 0, -1, 2 ), nrow=3, byrow=TRUE )
is.negative.definite( A )
###
### positive semi-defnite matrix
### eigenvalues are 4.732051 1.267949 8.881784e-16
###
B <- matrix( c( 2, -1, 2, -1, 2, -1, 2, -1, 2 ), nrow=3, byrow=TRUE )
is.negative.definite( B )
###
### negative definite matrix
### eigenvalues are -0.5857864 -2.0000000 -3.4142136
###
C <- matrix( c( -2, 1, 0, 1, -2, 1, 0, 1, -2 ), nrow=3, byrow=TRUE )
is.negative.definite( C )
###
### negative semi-definite matrix
### eigenvalues are 1.894210e-16 -1.267949 -4.732051
###
D <- matrix( c( -2, 1, -2, 1, -2, 1, -2, 1, -2 ), nrow=3, byrow=TRUE )
is.negative.definite( D )
###
### indefinite matrix
### eigenvalues are 3.828427  1.000000 -1.828427
###
E <- matrix( c( 1, 2, 0, 2, 1, 2, 0, 2, 1 ), nrow=3, byrow=TRUE )
is.negative.definite( E )
}
\keyword{ math }
