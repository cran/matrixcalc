\name{frobenius.matrix}
\alias{frobenius.matrix}
\title{ Frobenius Matrix }
\description{
  This function returns an order n Frobenius matrix that is useful
  in numerical mathematics.
}
\usage{
frobenius.matrix(n)
}
\arguments{
  \item{n}{  a positive integer value greater than 1}
}
\details{
  The Frobenius matrix is also called the companion matrix.  It arises
  in the solution of systems of linear first order differential equations.
  The formula for the order \eqn{n} Frobenius matrix is \eqn{{\bf{F}} = 
\left[ {\begin{array}{*{20}{c}}0&0& \cdots &0&{{{\left( { - 1} \right)}^{n - 1}}
\left( {\begin{array}{*{20}{c}}n\\0\end{array}} \right)}\\1&0& \cdots &0&{{{\left( { - 1} \right)}^{n - 2}}
\left( {\begin{array}{*{20}{c}}n\\1\end{array}} \right)}\\0&1& \ddots &0&{{{\left( { - 1} \right)}^{n - 3}}
\left( {\begin{array}{*{20}{c}}n\\2\end{array}} \right)}\\ \vdots & \vdots & \ddots & \vdots & \vdots \\0&0& \cdots &1&{{{\left( { - 1} \right)}^0}
\left( {\begin{array}{*{20}{c}}n\\{n - 1}\end{array}} 
\right)}\end{array}} 
\right]}.
}
\value{
  An order \eqn{n} matrix
}
\references{
  Aceto, L. and D. Trigiante (2001). Matrices of Pascal and Other Greats,
  \emph{American Mathematical Monthly}, March 2001, 108(3), 232-245.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument n is not a positive integer that is greater than 1, 
  the function presents an error message and stops.
}
\examples{
F <- frobenius.matrix( 10 )
print( F )
}
\keyword{ math }
