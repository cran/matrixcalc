\name{elimination.matrix}
\alias{elimination.matrix}
\title{ Elimination matrix for lower triangular matrices }
\description{
  This function returns a matrix with n * ( n + 1 ) / 2 rows and N * n columns which
  for any lower triangular matrix A transforms vec( A ) into vech(A)
}
\usage{
elimination.matrix(n)
}
\arguments{
  \item{n}{ row or column dimension }
}
\details{
  This function is a wrapper function to the function \code{L.matrix}.
  The formula used to compute the L matrix which is also called the elimination matrix is  \eqn{{\bf{L}} = \sum\limits_{j = 1}^n {\sum\limits_{i = j}^n {{{\bf{u}}_{i,j}}{{\left( {vec\;{{\bf{E}}_{i,j}}} \right)}^\prime }} } }
  \eqn{{{{\bf{u}}_{i,j}}}} are the order \eqn{n\left( {n + 1} \right)/2} vectors constructed by the function \code{u.vectors}.
  \eqn{{{{\bf{E}}_{i,j}}}} are the \eqn{ n \times n} matrices constructed by the function \code{E.matrices}.
}  
\value{
  An \eqn{\left[ {\frac{1}{2}n\left( {n + 1} \right)} \right] \times {n^2}} matrix.
}
\references{
  Magnus, J. R. and H. Neudecker (1980). The elimination matrix, some lemmas and applications,
  \emph{SIAM Journal on Algebraic Discrete Methods}, 1(4), December 1980, 422-449.
  
  Magnus, J. R. and H. Neudecker (1999) \emph{Matrix Differential Calculus with Applications in Statistics and Econometrics},
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument is not an integer, the function displays an error message and stops.
  If the argument is less than two, the function displays an error message and stops.
}
\seealso{
  \code{\link{E.matrices}},
  \code{\link{L.matrix}},
  \code{\link{u.vectors}}
}
\examples{
L <- elimination.matrix( 4 )
A <- lower.triangle( matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE ) )
vecA <- vec( A )
vechA <- vech( A )
y <- L \%*\% vecA
print( y )
print( vechA )
}
\keyword{ math }
