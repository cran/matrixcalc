\name{\%s\%}
\alias{\%s\%}
\title{ Direct sum of two arrays }
\description{
  This function computes the direct sum of two arrays.  The arrays can be
  numerical vectors or matrices.  The result ia the block diagonal matrix.
}  
\usage{
x\%s\%y
}
\arguments{
  \item{x}{ a numeric matrix or vector }
  \item{y}{ a numeric matrix or vector }
}
\value{
  A numeric matrix.
}
\details{
  If either \eqn{\bf{x}} or y is a vector, it is converted to a matrix.  The result
  is a block diagonal matrix \eqn{\left[ {\begin{array}{*{20}c}
   {\bf{x}} & {\bf{0}}  \\
   {\bf{0}} & {\bf{y}}  \\
\end{array}} \right]}.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) \emph{Matrix Differential Calculus with Applications in Statistics and Econometrics},
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu}, Kurt Hornik \email{Kurt.Hornik@wu-wien.ac.at} }
\examples{
x <- matrix( seq( 1, 4 ) )
y <- matrix( seq( 5, 8 ) )
print( x \%s\% y )
}
\keyword{ math }
