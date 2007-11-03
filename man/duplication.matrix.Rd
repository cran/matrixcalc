\name{duplication.matrix}
\alias{duplication.matrix}
\title{ Duplication matrix for n by n matrices }
\description{
  This function returns a matrix with n * n rows and n * ( n + 1 ) / 2 columns
  that transforms vech(A) to vec(A) where A is a symmetric n by n matrix.
}
\usage{
duplication.matrix(n)
}
\arguments{
  \item{n}{ Row and column dimension }
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
\seealso{
  \code{\link{vec}},
  \code{\link{vech}}
}
\examples{
D <- duplication.matrix( 4 )
A <- matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE )
vecA <- vec( A )
vechA<- vech( A )
y <- D %*% vechA
print( y )
print( vecA )
}
\keyword{ math }
