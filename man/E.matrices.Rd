\name{E.matrices}
\alias{E.matrices}
\title{ List of E Matrices }
\description{
  This function constructs and returns a list of lists. The component of
  each sublist is a square matrix derived from the column vectors of
  an order n identity matrix.
}
\usage{
E.matrices(n)
}
\arguments{
  \item{n}{ a positive integer for the order of the identity matrix
}
}
\details{
  Let \eqn{{{\bf{I}}_n} = \left[ {\begin{array}{*{20}{c}}
  {{{\bf{e}}_1}}&{{{\bf{e}}_2}}& \cdots &{{{\bf{e}}_n}}
  \end{array}} \right]} be the order \eqn{n} identity matrix
  with corresponding unit vectors \eqn{{{{\bf{e}}_i}}} with one in
  its \eqn{i}th position and zeros elsewhere.  
  The \eqn{n \times n} matrix \eqn{{{\bf{E}}_{i,j}}} is computed
  from the unit vectors \eqn{{{{\bf{e}}_i}}} and \eqn{{{{\bf{e}}_j}}}
  as \eqn{{{\bf{E}}_{i,j}} = {{\bf{e}}_i}\;{{\bf{e'}}_j}}.  These matrices
  are stored as components in a list of lists.
}
\value{
  A list with \eqn{n} components
  \item{1 }{A sublist of \eqn{n} components}
  \item{2 }{A sublist of \eqn{n} components}
 ...
  \item{n }{A sublist of \eqn{n} components}
  Each component \eqn{j} of sublist \eqn{i} is a matrix \eqn{{\bf{E}}_{i,j}}
}
\references{
  Magnus, J. R. and H. Neudecker (1980). The elimination matrix, some lemmas and applications,
  \emph{SIAM Journal on Algebraic Discrete Methods}, 1(4), December 1980, 422-449.
  
  Magnus, J. R. and H. Neudecker (1999). \emph{Matrix Differential Calculus with Applications in Statistics and Econometrics},
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  The argument n must be an integer value greater than or equal to 2.
}
\examples{
E <- E.matrices( 3 )
}
\keyword{ math }
