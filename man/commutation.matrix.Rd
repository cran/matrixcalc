\name{commutation.matrix}
\alias{commutation.matrix}
\title{ Commutation matrix for r by c numeric matrices }
\description{
  This function returns a square matrix of order p = r * c that, 
  for an r by c matrix A, transforms
  vec(A) to vec(A') where prime denotes transpose.
}
\usage{
commutation.matrix(r, c=r)
}
\arguments{
  \item{r}{ a positive integer integer row dimension }
  \item{c}{ a positive integer integer column dimension }
}
\details{
  This function is a wrapper function that uses the function \code{K.matrix} to do the actual work.
  The \eqn{r \times c} matrices \eqn{{\bf{H}}{}_{i,j}} constructed
  by the function \code{H.matrices} are combined using direct product
  to generate the commutation product with the following formula
  \eqn{{{\bf{K}}_{r,c}} = \sum\limits_{i = 1}^r {\sum\limits_{j = 1}^c {\left( {{{\bf{H}}_{i,j}} \otimes {{{\bf{H'}}}_{i,j}}} \right)} }}
}
\value{
  An order \eqn{\left( {r\;c} \right)} matrix.
}
\references{
  Magnus, J. R. and H. Neudecker (1979). The commutation matrix: some properties and applications,
  \emph{The Annals of Statistics}, 7(2), 381-394.
  
  Magnus, J. R. and H. Neudecker (1999) \emph{Matrix Differential Calculus with Applications in Statistics and Econometrics},
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If either argument is less than 2, then the function stops and displays an appropriate error mesage.
  If either argument is not an integer, then the function stops and displays an appropriate error mesage
}
\seealso{
  \code{\link{H.matrices}},
  \code{\link{K.matrix}}
}
\examples{
K <- commutation.matrix( 3, 4 )
A <- matrix( seq( 1, 12, 1 ), nrow=3, byrow=TRUE )
vecA <- vec( A )
vecAt <- vec( t( A ) )
print( K \%*\% vecA )
print( vecAt )
}
\keyword{ math }
