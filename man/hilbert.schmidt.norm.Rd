\name{hilbert.schmidt.norm}
\alias{hilbert.schmidt.norm}
\title{ Compute the Hilbert-Schmidt norm of a matrix }
\description{
  This function returns the Hilbert-Schmidt norm of the matrix \eqn{{\mathbf{x}}}.
}
\usage{
hilbert.schmidt.norm(x)
}
\arguments{
  \item{x}{  a numeric vector or matrix }
}
\details{
  The formula used to compute the norm is \eqn{\left\| {\bf{x}} \right\|_2}.
  This is merely the entrywise norm with exponent 2.
}
\value{
  A numeric value.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.

  Golub, G. H. and C. F. Van Loan (1996).  \emph{Matrix Computations}, Third Edition, The John
  Hopkins University Press.

  Horn, R. A. and C. R. Johnson (1985). \emph{Matrix Analysis}, Cambridge University Press.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{entrywise.norm}}
}
\examples{
A <- matrix( c( 3, 5, 7, 2, 6, 4, 0, 2, 8 ), nrow=3, ncol=3, byrow=TRUE )
print( hilbert.schmidt.norm( A ) )
}
\keyword{ math }
