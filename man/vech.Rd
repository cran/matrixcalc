\name{vech}
\alias{vech}
\title{ Vectorize a matrix }
\description{
  This function returns a stack of the lower triangular matrix of a square matrix as a matrix with 1 column
  and n * ( n + 1 ) / 2 rows
}
\usage{
vech(x)
}
\arguments{
  \item{x}{ a matrix }
}
\value{
  A matrix with \eqn{\frac{1}{2}n\left( {n + 1} \right)} rows and one column.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) \emph{Matrix Differential Calculus with Applications in Statistics and Econometrics},
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{is.square.matrix}}
}
\examples{
x <- matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE )
print( x )
y <- vech( x )
print( y )
}
\keyword{ math }
