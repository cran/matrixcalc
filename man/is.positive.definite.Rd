\name{is.positive.definite}
\alias{is.positive.definite}
\title{ Test matrix for positive definiteness }
\description{
  This function returns TRUE if the argument, a square matrix x, is positive definite.  The user
  chooses between a test of the eigenvalues of x or a Choleski decomposition of x.
}
\usage{
is.positive.definite(x, tol, method = c("eigen", "chol"))
}
\arguments{
  \item{x}{ a matrix }
  \item{tol}{ a numeric tolerance level usually left out }
  \item{method}{ a character string that specifies the method used }
}
\details{
  For the eigenvalue approach, the eigenvalues should be positive.  The Choleski decomposition only works
  for positive definite symmetric matrices.  The function works only for real valued matrices.
}
\value{
  TRUE or FALSE.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
A <- diag( 1, 3 )
is.positive.definite( A )
B <- matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
is.positive.definite( B )
}
\keyword{ math }
