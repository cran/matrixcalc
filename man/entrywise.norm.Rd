\name{entrywise.norm}
\alias{entrywise.norm}
\title{ Compute the entrywise norm of a matrix }
\description{
  This function returns the \eqn{\left\| {\bf{x}} \right\|_p } norm of the matrix \eqn{{\mathbf{x}}}.
}
\usage{
entrywise.norm(x,p)
}
\arguments{
  \item{x}{  a numeric vector or matrix }
  \item{p}{  a real value for the power }
}
\details{
  Let \eqn{{\bf{x}}} be an \eqn{m \times n} numeric matrix.
  The formula used to compute the norm is \eqn{\left\| {\bf{x}} \right\|_p  = \left( {\sum\limits_{i = 1}^m {\sum\limits_{j = 1}^n {\left| {x_{i,j} } \right|^p } } } \right)^{{1 \mathord{\left/
 {\vphantom {1 p}} \right.
 \kern-\nulldelimiterspace} p}}}.
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
\note{
  If argument x is not numeric, the function displays an error message and terminates.
  If argument x is neither a matrix nor a vector, the function displays an error message and terminates.
  If argument p is zero, the function displays an error message and terminates.
}  
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{one.norm}},
  \code{\link{inf.norm}}
}
\examples{
A <- matrix( c( 3, 5, 7, 2, 6, 4, 0, 2, 8 ), nrow=3, ncol=3, byrow=TRUE )
print( entrywise.norm( A, 2 ) )
}
\keyword{ math }
