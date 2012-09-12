\name{is.singular.matrix}
\alias{is.singular.matrix}
\title{  Test for singular square matrix }
\description{
  This function returns \code{TRUE} is the matrix argument is singular
  and \code{FALSE} otherwise.
}
\usage{
is.singular.matrix(x, tol = 1e-08)
}
\arguments{
  \item{x}{ a numeric square matrix }
  \item{tol}{ a numeric tolerance level usually left out }
}
\details{
  The determinant of the matrix \code{x} is first computed.
  If the absolute value of the determinant is less than the given
  tolerance level, then a \code{TRUE} value is returned.
  Otherwise, a \code{FALSE} value is returned.
}
\value{
  A TRUE or FALSE value.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.

  Horn, R. A. and C. R. Johnson (1990). \emph{Matrix Analysis}, Cambridge University Press.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{is.non.singular.matrix}}
}
\examples{
A <- diag( 1, 3 )
is.singular.matrix( A )
B <- matrix( c( 0, 0, 3, 4 ), nrow=2, byrow=TRUE )
is.singular.matrix( B )
}
\keyword{ math }
