\name{N.matrix}
\alias{N.matrix}
\title{ Construct N Matrix }
\description{
  This function returns the order n square matrix that is the sum of
  an implicit commutation matrix and the order n identity matrix
  quantity divided by two
}
\usage{
N.matrix(n)
}
\arguments{
  \item{n}{ A positive integer matrix order }
}
\details{
  Let \eqn{{\bf{K}_n}} be the order \eqn{n} implicit commutation matrix (i.e., \eqn{{{\bf{K}}_{n,n}}} ).
  and \eqn{{{\bf{I}}_n}} the order \eqn{n} identity matrix.  The formula for the matrix is \eqn{{\bf{N}} = \frac{1}{2}\left( {{{\bf{K}}_n} + {{\bf{I}}_n}} \right)}.
}
\value{
  An order \eqn{n} matrix.
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
  \code{\link{K.matrix}}
}
\examples{
N <- N.matrix( 3 )
print( N )
}
\keyword{ math }
