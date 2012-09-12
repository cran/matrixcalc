\name{is.non.singular.matrix}
\alias{is.non.singular.matrix}
\title{ Test if matrix is non-singular }
\description{
  This function returns \code{TRUE} is the matrix argument is non-singular
  and \code{FALSE} otherwise.
}
\usage{
is.non.singular.matrix(x, tol = 1e-08)
}
\arguments{
  \item{x}{ a numeric square matrix }
  \item{tol}{ a numeric tolerance level usually left out }
}
\details{
  The determinant of the matrix \code{x} is first computed.
  If the absolute value of the determinant is greater than or equal to the given
  tolerance level, then a \code{TRUE} value is returned.
  Otherwise, a \code{FALSE} value is returned.
}
\value{
  TRUE or FALSE value.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.

  Horn, R. A. and C. R. Johnson (1990). \emph{Matrix Analysis}, Cambridge University Press.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{is.singular.matrix}}
}
\examples{
A <- diag( 1, 3 )
is.non.singular.matrix( A )
B <- matrix( c( 0, 0, 3, 4 ), nrow=2, byrow=TRUE )
is.non.singular.matrix( B )
}
\keyword{ math }
