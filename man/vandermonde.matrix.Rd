\name{vandermonde.matrix}
\alias{vandermonde.matrix}
\title{ Vandermonde matrix }
\description{
  This function returns an m by n matrix of the powers of the alpha vector
}
\usage{
vandermonde.matrix(alpha, n)
}
\arguments{
  \item{alpha}{ A numerical vector of values }
  \item{n}{ The column dimension of the Vandermonde matrix }
}
\details{
  In linear algebra, a Vandermonde matrix is an \eqn{m \times n} matrix with terms
  of a geometric progression of an \eqn{m \times 1} parameter vector \eqn{{\bf{\alpha }} = {\left[ {\begin{array}{*{20}{c}}
{{\alpha _1}}&{{\alpha _2}}& \cdots &{{\alpha _m}}
\end{array}} \right]^\prime }} 
  such that \eqn{V\left( {\bf{\alpha }} \right) = \left[ {\begin{array}{*{20}{c}}
1&{{\alpha _1}}&{\alpha _1^2}& \cdots &{\alpha _1^{n - 1}}\\
1&{{\alpha _2}}&{\alpha _2^2}& \cdots &{\alpha _2^{n - 1}}\\
1&{{\alpha _3}}&{\alpha _3^2}& \cdots &{\alpha _3^{n - 1}}\\
 \cdots & \cdots & \cdots & \cdots & \cdots \\
1&{{\alpha _m}}&{\alpha _m^2}& \cdots &{\alpha _m^{n - 1}}
\end{array}} \right]}.
}
\value{
  A matrix.
}
\references{
  Horn, R. A. and C. R. Johnson (1991). \emph{Topics in matrix analysis}, Cambridge
  University Press.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
alpha <- c( .1, .2, .3, .4 )
V <- vandermonde.matrix( alpha, 4 )
print( V )
}
\keyword{ math }
