\name{u.vectors}
\alias{u.vectors}
\title{ u vectors of an identity matrix }
\description{
  This function constructs an order n * ( n + 1 ) / 2 identity matrix
  and an order matrix u that that maps the ordered pair of indices
  (i,j) i=j, ..., n; j=1, ..., n to a column in this identity matrix.
}
\usage{
u.vectors(n)
}
\arguments{
  \item{n}{ a positive integer value for the order of underlying matrices }
}
\details{
  The function firsts constructs an identity matrix of order \eqn{\frac{1}{2}n\left( {n + 1} \right)}.
  \eqn{{{{\bf{u}}_{i,j}}}} is the column vector in the order \eqn{\frac{1}{2}n\left( {n + 1} \right)} identity
  matrix for column \eqn{k = \left( {j - 1} \right)n + i - \frac{1}{2}j\left( {j - 1} \right)}.
}
\value{
  A list with two named components
  \item{k }{order \eqn{n} square matrix that maps each ordered pair (i,j) to a column in the identity matrix}
  \item{I }{order \eqn{\frac{1}{2}n\left( {n + 1} \right)} identity matrix}
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
\examples{
u <- u.vectors( 3 )
}
\keyword{ math }
