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
  In mathematics, particularly matrix theory and combinatorics, the Pascal matrix is a lower triangular matrix
  with binomial coefficients in the rows.  It is easily obtained by performing an LU decomposition on 
  the symmetric Pascal matrix of the same order and returning the lower triangular matrix.
}  
\value{
  An order \eqn{n} matrix.
}
\references{
  Aceto, L. and D. Trigiante, (2001). Matrices of Pascal and Other Greats, \emph{American
  Mathematical Monthly}, March 2001, 232-245.

  Call, G. S. and D. J. Velleman, (1993). Pascal's matrices, \emph{American Mathematical Monthly},
  April 1993, 100, 372-376.
  
  Edelman, A. and G. Strang, (2004). Pascal Matrices, \emph{American Mathematical Monthly},
  111(3), 361-385.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument n is not a positive integer, the function presents an error message and stops.
}
\seealso{
  \code{\link{lu.decomposition}},
  \code{\link{symmetric.pascal.matrix}}
}
\examples{
P <- pascal.matrix( 4 )
print( P )
}
\keyword{ math }
