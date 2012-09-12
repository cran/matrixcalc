\name{is.idempotent.matrix}
\alias{is.idempotent.matrix}
\title{ Test for idempotent square matrix }
\description{
  This function returns a \code{TRUE} value if the square matrix argument x
  is idempotent, that is, the product of the matrix with itself is the matrix.
  The equality test is performed to within the specified tolerance level.  If
  the matrix is not idempotent, then a \code{FALSE} value is returned.
}
\usage{
is.idempotent.matrix(x, tol = 1e-08)
}
\arguments{
  \item{x}{ a numeric square matrix }
  \item{tol}{ a numeric tolerance level usually left out }
}
\details{
  Idempotent matrices are used in econometric analysis.  Consider the problem of
  estimating the regression parameters of a standard linear model 
  \eqn{{\bf{y}} = {\bf{X}}\;{\bf{\beta }} + {\bf{e}}} using the method of least squares.
  \eqn{{\bf{y}}} is an order \eqn{m} random vector of dependent variables. 
  \eqn{{\bf{X}}} is an \eqn{m \times n} matrix whose columns are columns of
  observations on one of the \eqn{ n - 1} independent variables.  The first column
  contains \eqn{m} ones.  \eqn{{\bf{e}}} is an order \eqn{m} random vector of zero
  mean residual values.  \eqn{{\bf{\beta }}} is the order \eqn{n} vector of regression
  parameters.  The objective function that is minimized in the method of least squares is
  \eqn{\left( {{\bf{y}} - {\bf{X}}\;{\bf{\beta }}} \right)^\prime  \left( {{\bf{y}} - {\bf{X}}\;{\bf{\beta }}} \right)}.
  The solution to ths quadratic programming problem is
  \eqn{{\bf{\hat \beta }} = \left[ {\left( {{\bf{X'}}\;{\bf{X}}} \right)^{ - 1} \;{\bf{X'}}} \right]\;{\bf{y}}}
  The corresponding estimator for the residual vector is
  \eqn{{\bf{\hat e}} = {\bf{y}} - {\bf{X}}\;{\bf{\hat \beta }} = \left[ {{\bf{I}} - {\bf{X}}\;\left( {{\bf{X'}}\;{\bf{X}}} \right)^{ - 1} {\bf{X'}}} \right]{\bf{y}} = {\bf{M}}\;{\bf{y}}}.
  \eqn{{\bf{M}}} and \eqn{{{\bf{X}}\;\left( {{\bf{X'}}\;{\bf{X}}} \right)^{ - 1} {\bf{X'}}}} are idempotent.
  Idempotency of \eqn{{\bf{M}}} enters into the estimation of the variance of the estimator.
}
\value{
  A TRUE or FALSE value.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.

  Chang, A. C., (1984). \emph{Fundamental Methods of Mathematical Economics},
  Third edition, McGraw-Hill.

  Green, W. H. (2003). \emph{Econometric Analysis}, Fifth edition, Prentice-Hall.
  
  Horn, R. A. and C. R. Johnson (1990). \emph{Matrix Analysis}, Cambridge University Press.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
A <- diag( 1, 3 )
is.idempotent.matrix( A )
B <- matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
is.idempotent.matrix( B )
C <- matrix( c( 1, 0, 0, 0 ), nrow=2, byrow=TRUE )
is.idempotent.matrix( C )
}
\keyword{ math }
