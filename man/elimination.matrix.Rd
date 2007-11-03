\name{elimination.matrix}
\alias{elimination.matrix}
\title{ Elimination matrix for lower triangular matrices }
\description{
  This function returns a matrix with n * ( n + 1 ) / 2 rows and N * n columns which
  for any lower triangular matrix A transforms vec( A ) into vech(A)
}
\usage{
elimination.matrix(n)
}
\arguments{
  \item{n}{ row or column dimension }
}
\value{
  A matrix.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) Matrix Differential Calculus with Applications in Statistics and Econometrics,
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{ ~~further notes~~ 
  If the argument is less than or equal to zero, the function displays an error message and stops.
  If the argument is not an integer, the function displays an error message and stops.
}
\seealso{
  \code{\link{lower.triangle}},
  \code{\link{vec}},
  \code{\link{vech}}
}
\examples{
L <- elimination.matrix( 4 )
A <- lower.triangle( matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE ) )
vecA <- vec( A )
vechA <- vech( A )
y <- L %*% vecA
print( y )
print( vechA )
}
\keyword{ math }
