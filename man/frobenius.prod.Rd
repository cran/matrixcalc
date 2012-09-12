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
  \item{x}{ a numeric matrix or vector object }
  \item{y}{ a numeric matrix or vector object }
}
\details{
  The Frobenius inner product is the element-by-element sum of 
  the Hadamard or Shur product of two numeric matrices.  Let \eqn{{\bf{x}}} and
  \eqn{{\bf{y}}} be two \eqn{m \times n} matrices.  Then Frobenious inner product
  is computed as \eqn{\sum\limits_{i = 1}^m {\sum\limits_{j = 1}^n {x_{i,j} \;y_{i,j} } } }.
}
\value{
  A numeric value.
}
\references{
  Styan, G. P. H. (1973). Hadamard Products and Multivariate Statistical Analysis,
  \emph{Linear Algebra and Its Applications}, Elsevier, 6, 217-240.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{hadamard.prod}}
}
\note{
  The function converts vectors to matrices if necessary.
  The function stops running if x or y is not numeric and an error message is displayed.
  The function also stops running if x and y do not have the same row and column dimensions and an error mesage
  is displayed.
}
\examples{
x <- matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
y <- matrix( c( 2, 4, 6, 8 ), nrow=2, byrow=TRUE )
z <- frobenius.prod( x, y )
print( z )
}
\keyword{ math }
