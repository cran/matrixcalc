\name{hadamard.prod}
\alias{hadamard.prod}
\title{ Hadamard product of two matrices }
\description{
  This function returns the Hadamard or Shur product of two matrices, x and y, that have the same row and
  column dimensions.
}
\usage{
hadamard.prod(x, y)
}
\arguments{
  \item{x}{ a numeric matrix object }
  \item{y}{ a numeric matrix object }
}
\details{
  The Hadamard product is an element-by-element product of the two matrices.  It uses the * operation in R.
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
  The function stops running if x or y is not a numeric matrix and an error message is displayed.
  The function also stops running if x and y do not have the same row and column dimensions and an error mesage
  is displayed.
}
\seealso{
\code{\link{is.numeric.matrix}}
}
\examples{
x <- matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
y <- matrix( c( 2, 4, 6, 8 ), nrow=2, byrow=TRUE )
z <- hadamard.prod( x, y )
}
\keyword{ math }
