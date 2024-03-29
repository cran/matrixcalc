\name{D.matrix}
\alias{D.matrix}
\title{ Duplication matrix }
\description{
  This function constructs the linear transformation D that maps
  vech(A) to vec(A) when A is a symmetric matrix
}
\usage{
D.matrix(n)
}
\arguments{
  \item{n}{ a positive integer value for the order of the underlying matrix }
}
\details{
  Let \eqn{{\bf{T}}_{i,j}} be an \eqn{n \times n} matrix with 1 in its \eqn{\left( {i,j} \right)} element \eqn{1 \le i,j \le n}.
  and zeroes elsewhere. These matrices are constructed by the function \code{T.matrices}.  The formula for the
  transpose of matrix \eqn{\bf{D}} is \eqn{{\bf{D'}} = \sum\limits_{j = 1}^n {\sum\limits_{i = j}^n {{{\bf{u}}_{i,j}}\;{{\left( {vec\;{{\bf{T}}_{i,j}}} \right)}^\prime }} } }
  where \eqn{{{{\bf{u}}_{i,j}}}} is the column vector in the order \eqn{\frac{1}{2}n\left( {n + 1} \right)} identity
  matrix for column \eqn{k = \left( {j - 1} \right)n + i - \frac{1}{2}j\left( {j - 1} \right)}.  The function
  \code{u.vectors} generates these vectors.
}
\value{
  It returns an \eqn{{n^2}\; \times \;\frac{1}{2}n\left( {n + 1} \right)} matrix.
}
\references{
  Magnus, J. R. and H. Neudecker (1980). The elimination matrix, some lemmas and applications,
  \emph{SIAM Journal on Algebraic Discrete Methods}, 1(4), December 1980, 422-449.
  
  Magnus, J. R. and H. Neudecker (1999). \emph{Matrix Differential Calculus with Applications in Statistics and Econometrics},
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{T.matrices}},
  \code{\link{u.vectors}}
}
\examples{
D <- D.matrix( 3 )
A <- matrix( c( 1, 2, 3,
                2, 3, 4,
                3, 4, 5), nrow=3, byrow=TRUE )
vecA <- vec( A )
vechA<- vech( A )
y <- D \%*\% vechA
print( y )
print( vecA )
}
\keyword{ math }
