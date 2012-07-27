\name{direct.prod}
\alias{direct.prod}
\title{ Direct prod of two arrays }
\description{
  This function computes the direct product of two arrays.  The arrays can be
  numerical vectors or matrices.  The result is a matrix.
}  
\usage{
direct.prod( x, y )
}
\arguments{
  \item{x}{ a numeric matrix or vector }
  \item{y}{ a numeric matrix or vector }
}
\details{
  If either \eqn{\bf{x}} or \eqn{\bf{y}} is a vector, it is converted to a matrix.
  Suppose that \eqn{\bf{x}} is an \eqn{m \times n} matrix and \eqn{\bf{y}} is an \eqn{ p \times q}
  matrix. Then, the function returns the matrix \eqn{\left[ {\begin{array}{*{20}{c}}
{{x_{1,1}}\;{\bf{y}}}&{{x_{1,2}}\;{\bf{y}}}& \cdots &{{x_{1,n}}\;{\bf{y}}}\\
{{x_{2,1}}\;{\bf{y}}}&{{x_{2,2}}\;{\bf{y}}}& \cdots &{{x_{2,n}}\;{\bf{y}}}\\
 \cdots & \cdots & \cdots & \cdots \\
{{x_{m,1}}\;{\bf{y}}}&{{x_{m,2}}\;{\bf{y}}}& \cdots &{{x_{m,n}}\;{\bf{y}}}
\end{array}} \right]}.
}
\value{
  A numeric matrix.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) \emph{Matrix Differential Calculus with Applications in Statistics and Econometrics},
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu}, Kurt Hornik \email{Kurt.Hornik@wu-wien.ac.at} }
\examples{
x <- matrix( seq( 1, 4 ) )
y <- matrix( seq( 5, 8 ) )
print( direct.prod( x, y ) )
}
\keyword{ math }
