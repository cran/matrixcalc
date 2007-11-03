\name{vec}
\alias{vec}
\title{ Vectorize a matrix }
\description{
  This function returns a column vector that is a stack of the columns of x, an m by n matrix.
}
\usage{
vec(x)
}
\arguments{
  \item{x}{ a matrix }
}
\value{
  A matrix with m * n rows and one column.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) Matrix Differential Calculus with Applications in Statistics and Econometrics,
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{is.numeric.matrix}}
}
\examples{
x <- matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE )
vecx <- vec( x )
}
\keyword{ math }
