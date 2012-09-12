\name{one.norm}
\alias{one.norm}
\title{ Compute the one norm of a matrix }
\description{
  This function returns the \eqn{\left\| {\bf{x}} \right\|_1 } norm of the matrix \eqn{{\mathbf{x}}}.
}
\usage{
one.norm(x)
}
\arguments{
  \item{x}{  a numeric vector or matrix }
}
\details{
  Let \eqn{{\bf{x}}} be an \eqn{m \times n} matrix. 
  The formula used to compute the norm is \eqn{\left\| {\bf{x}} \right\|_1  = \mathop {\max }\limits_{1 \le j \le n} \sum\limits_{i = 1}^m {\left| {x_{i,j} } \right|} }.
  This is merely the maximum absolute column sum of the \eqn{m \times n} maxtris.
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
  \code{\link{inf.norm}}
}
\examples{
A <- matrix( c( 3, 5, 7, 2, 6, 4, 0, 2, 8 ), nrow=3, ncol=3, byrow=TRUE )
one.norm( A )
}
\keyword{ math }
