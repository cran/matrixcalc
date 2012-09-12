\name{lu.decomposition}
\alias{lu.decomposition}
\title{ LU Decomposition of Square Matrix }
\description{
  This function performs an LU decomposition of the given square matrix argument
  the results are returned in a list of named components.  The Doolittle decomposition
  method is used to obtain the lower and upper triangular matrices
}
\usage{
lu.decomposition(x)
}
\arguments{
  \item{x}{ a numeric square matrix }
}
\details{
  The Doolittle decomposition without row exchanges is performed generating
  the lower and upper triangular matrices separately rather than in one matrix.
}
\value{
  A list with two named components.
  \item{L }{The numeric lower triangular matrix}
  \item{U }{The number upper triangular matrix}
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.

  Golub, G. H. and C. F. Van Loan (1996). \emph{Matrix Computations}, Third Edition,
  John Hopkins University Press
  
  Horn, R. A. and C. R. Johnson (1985). \emph{Matrix Analysis}, Cambridge University Press.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
A <- matrix( c ( 1, 2, 2, 1 ), nrow=2, byrow=TRUE)
luA <- lu.decomposition( A )
L <- luA$L
U <- luA$U
print( L )
print( U )
print( L \%*\% U )
print( A )
B <- matrix( c( 2, -1, -2, -4, 6, 3, -4, -2, 8 ), nrow=3, byrow=TRUE )
luB <- lu.decomposition( B )
L <- luB$L
U <- luB$U
print( L )
print( U )
print( L \%*\% U )
print( B )
}
\keyword{ math }
