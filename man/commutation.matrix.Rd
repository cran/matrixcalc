\name{commutation.matrix}
\alias{commutation.matrix}
\title{ Commutation matrix for m by n numeric matrices }
\description{
  This function returns a square matrix of order p = m * n that, for an m by n matrix A, transforms
  vec(A) to vec(A') where prime denotes transpose.
}
\usage{
commutation.matrix(m, n)
}
\arguments{
  \item{m}{ integer row dimension }
  \item{n}{ integer column dimension }
}
\value{
  A numeric matrix.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) Matrix Differential Calculus with Applications in Statistics and Econometrics,
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If either argument is negative, then the function stops and displays an appropriate error mesage.
  If either argument is not an integer, then the function stops and displays an appropriate error mesage
}
\seealso{
  \code{\link{vec}},
  \code{\link{vech}}
}
\examples{
K <- commutation.matrix( 3, 4 )
A <- matrix( seq( 1, 12, 1 ), nrow=3, byrow=TRUE )
vecA <- vec( A )
vecAt <- vec( t( A ) )
y <- K %*% vecA
print( y )
}
\keyword{ math }
