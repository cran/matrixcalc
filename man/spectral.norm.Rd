\name{spectral.norm}
\alias{spectral.norm}
\title{ Spectral norm of  matrix }
\description{
  This function returns the spectral norm of a real matrix.
}
\usage{
spectral.norm(x)
}
\arguments{
  \item{x}{ a numeric matrix or vector }
}
\details{
  Let \eqn{{\bf{x}}} be an \eqn{m \times n} real matrix.  The
  function computes the order \eqn{n} square matrixmatrix \eqn{{\bf{A}} = {\bf{x'}}\;{\bf{x}}}.
  The R function \code{eigen} is applied to this matrix to obtain the vector
  of eigenvalues \eqn{{\bf{\lambda }} = \left[ {\begin{array}{*{20}c}
   {\lambda _1 } & {\lambda _2 } &  \cdots  & {\lambda _n }  \\
\end{array}} \right]}.  By construction the eigenvalues are in descending
  order of value so that the largest eigenvalue is \eqn{\lambda _1}.  Then
  the spectral norm is \eqn{\left\| {\bf{x}} \right\|_2  = \sqrt {\lambda _1 }}.
  If \eqn{{\bf{x}}} is a vector, then  \eqn{{\bf{L}}_2  = \sqrt {\bf{A}}} is returned.
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
\note{
  If the argument x is not numeric, an error message is displayed and the function terminates.
  If the argument is neither a matrix nor a vector, an error message is displayed and the 
  function terminates.
  If the product matrix \eqn{{\bf{x'}}\;{\bf{x}}} is negative definite, an error message
  displayed and the function terminates.
}
\examples{
x <- matrix( c( 2, 4, 2, 1, 3, 1, 5, 2, 1, 2, 3, 3 ), nrow=3, ncol=4, byrow=TRUE )
spectral.norm( x )
}
\keyword{ math }
