\name{symmetric.pascal.matrix}
\alias{symmetric.pascal.matrix}
\title{ Symmetric Pascal matrix }
\description{
  This function returns an n by n symmetric Pascal matrix.
}
\usage{
symmetric.pascal.matrix(n)
}
\arguments{
  \item{n}{ Order of the matrix }
}
\details{
  In mathematics, particularly matrix theory and combinatorics, the symmetric Pascal matrix is a square matrix
  from which you can derive binomial coefficients.  The matrix is an order \eqn{n} symmetric
  matrix with typical element given by \eqn{{S_{i,j}} = {{n!} \mathord{\left/
 {\vphantom {{n!} {\left[ {r!\;\left( {n - r} \right)!} \right]}}} \right.
 \kern-\nulldelimiterspace} {\left[ {r!\;\left( {n - r} \right)!} \right]}}} where
  \eqn{n = i + j - 2} and \eqn{r = i - 1}.  The binomial coefficients are elegantly recovered from the
  symmetric Pascal matrix by performing an \eqn{LU} decomposition as \eqn{{\bf{S}} = {\bf{L}}\;{\bf{U}}}.
}  
\value{
  An order \eqn{n} matrix.
}
\references{
  Call, G. S. and D. J. Velleman, (1993). Pascal's matrices, \emph{American Mathematical Monthly},
  April 1993, 100, 372-376.
  
  Edelman, A. and G. Strang, (2004). Pascal Matrices, \emph{American Mathematical Monthly},
  111(3), 361-385.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument n is not a positive integer, the function presents an error message and stops.
}
\examples{
S <- symmetric.pascal.matrix( 4 )
print( S )
}
\keyword{ math }
