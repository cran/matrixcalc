\name{T.matrices}
\alias{T.matrices}
\title{ List of T Matrices }
\description{
  This function constructs a list of lists.  The number of components in
  the high level list is n.  Each of the n components is also a list.
  Each sub-list has n components each of which is an order n square matrix.
}
\usage{
T.matrices(n)
}
\arguments{
  \item{n}{ a positive integer value for the order of the matrices }
}
\details{
  Let \eqn{{{\bf{E}}_{i,j}}\;i = 1, \ldots ,n\;;\;j = 1, \ldots ,n} 
  be a representative order \eqn{n} matrix created with function \code{E.matrices}.
  The order \eqn{n} matrix \eqn{{{\bf{T}}_{i,j}}} is defined as follows
  \eqn{{{\bf{T}}_{i,j}} = \left\{ {\begin{array}{*{20}{c}}
{{{\bf{E}}_{i,j}}}&{i = j}\\
{{{\bf{E}}_{i,j}} + {{\bf{E}}_{j,i}}}&{i \ne j}
\end{array}} \right.}
}
\value{
  A list of \eqn{n} components.
  \item{1 }{A list of \eqn{n} components}
  \item{2 }{A list of \eqn{n} components}
 ...
  \item{n }{A list of \eqn{n} components}
  Each component \eqn{j} of sublist \eqn{i} is a matrix \eqn{{\bf{T}}_{i,j}}
}
\references{
  Magnus, J. R. and H. Neudecker (1980). The elimination matrix, some lemmas and applications,
  \emph{SIAM Journal on Algebraic Discrete Methods}, 1(4), December 1980, 422-449.
  
  Magnus, J. R. and H. Neudecker (1999) \emph{Matrix Differential Calculus with Applications in Statistics and Econometrics},
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  The argument n must be an integer value greater than or equal to 2.
}
\seealso{
  \code{\link{E.matrices}}
}
\examples{
T <- T.matrices( 3 )
}
\keyword{ math }
