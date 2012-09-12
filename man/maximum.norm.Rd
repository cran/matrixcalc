\name{maximum.norm}
\alias{maximum.norm}
\title{ Maximum norm of  matrix }
\description{
  This function returns the max norm of a real matrix.
}
\usage{
maximum.norm(x)
}
\arguments{
  \item{x}{ a numeric matrix or vector }
}
\details{
  Let \eqn{{\bf{x}}} be an \eqn{m \times n} real matrix. The max norm returned
  is \eqn{\left\| {\bf{x}} \right\|_{\max }  = \mathop {\max }\limits_{i,j} \left| {x_{i,j} } \right|}. 
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
  \code{\link{inf.norm}},
  \code{\link{one.norm}}
}
\examples{
A <- matrix( c( 3, 5, 7, 2, 6, 4, 0, 2, 8 ), nrow=3, ncol=3, byrow=TRUE )
maximum.norm( A )
}
\keyword{ math }
