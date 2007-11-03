\name{frobenius.prod}
\alias{frobenius.prod}
\title{ Frobenius innter product of matrices }
\description{
  This function returns the Fronbenius inner product of two matrices, x and y, with the same row and column dimensions.
}
\usage{
frobenius.prod(x, y)
}
\arguments{
  \item{x}{ a numeric matrix object }
  \item{y}{ a numeric matrix object }
}
\details{
  The Frobenius inner product is the element-by-element sum of the Hadamard or Shur product of two numeric matrices.
}
\value{
  A numeric value.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) Matrix Differential Calculus with Applications in Statistics and Econometrics,
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
\code{\link{hadamard.prod}}
}
\examples{
x <- matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
y <- matrix( c( 2, 4, 6, 8 ), nrow=2, byrow=TRUE )
z <- frobenius.prod( x, y )
print( z )
}
\keyword{ math }
