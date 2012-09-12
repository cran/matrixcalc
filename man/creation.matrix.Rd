\name{creation.matrix}
\alias{creation.matrix}
\title{ Creation Matrix }
\description{
  This function returns the order n creation matrix, a square matrix with the
  sequence 1, 2, ..., n - 1 on the sub-diagonal below the principal diagonal.
}
\usage{
creation.matrix(n)
}
\arguments{
  \item{n}{ a positive integer greater than 1 }
}
\details{
  The order \eqn{n} creation matrix is also called the derivation matrix and is
  used in numerical mathematics and physics. It arises in the solution of linear
  dynamical systems.  The form of the matrix is 
  \eqn{\left[ {\begin{array}{*{20}{c}}
0&0&0& \cdots &0&0\\
1&0&0& \cdots &0&0\\
0&2&0& \cdots &0&0\\
0&0&3& \ddots &0&0\\
 \vdots & \vdots & \vdots & \ddots & \ddots &{}\\
0&0&0& \cdots &{n - 1}&0
\end{array}} \right]}.
}
\value{
  An order \eqn{n} matrix.
}
\references{
  Aceto, L. and D. Trigiante (2001). Matrices of Pascal and Other Greats,
  \emph{American Mathematical Monthly}, March 2001, 108(3), 232-245.
  
  Weinberg, S. (1995). \emph{The Quantum Theory of Fields}, Cambridge
  University Press.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument n is not an integer that is greater than 1, 
  the function presents an error message and stops.
}
\examples{
H <- creation.matrix( 10 )
print( H )
}
\keyword{ math }
