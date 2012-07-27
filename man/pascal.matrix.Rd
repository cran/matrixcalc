\name{pascal.matrix}
\alias{pascal.matrix}
\title{ Pascal matrix }
\description{
  This function returns an n by n Pascal matrix.
}
\usage{
pascal.matrix(n)
}
\arguments{
  \item{n}{ Order of the matrix }
}
\details{
  In mathematics, particularly matrix theory and combinatorics, the Pascal matrix is a symmetric matrix
  from which you can derive binomial coefficients.  The matrix is an order \eqn{n} symmetric
  matrix with typical element given by \eqn{{S_{i,j}} = \frac{{n!}}{{r!\,\left( {n - r} \right)!}}} where
  \eqn{n = i + j - 2} and \eqn{r = i - 1}.  The binomial coefficients are elegantly recovered from the
  Pascal by performing an \eqn{LU} decomposition as \eqn{{\bf{S}} = {\bf{L}}\;{\bf{U}}}.
}  
\value{
  A matrix.
}
\references{
  Call, G. S. and D. J. Vellerman, (1993). Pascal's matrices, \emph{American Mathematical Monthly},
  APril 1993, 100, 372-376.
  
  Edelman, A. and G. Strang, (2004). Pascal Matrices, \emph{American Mathematical Monthly},
  111(3), 361-385.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument n is not a positive integer, the function presents an error message and stops.
}
\examples{
p <- pascal.matrix( 4 )
}
\keyword{ math }
