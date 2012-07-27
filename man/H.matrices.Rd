\name{H.matrices}
\alias{H.matrices}
\title{ List of H Matrices }
\description{
  This function constructs and returns a list of lists. The component of
  each sublist is derived from column vectors in an order r and order c identity matrix.
}
\usage{
H.matrices(r, c = r)
}
\arguments{
  \item{r}{ a positive integer value for an order r identity matrix }
  \item{c}{ a positive integer value for an order c identify matrix }
}
\details{
  Let \eqn{{{\bf{I}}_r} = \left[ {\begin{array}{*{20}{c}}
  {{{\bf{a}}_1}}&{{{\bf{a}}_2}}& \cdots &{{{\bf{a}}_r}}
  \end{array}} \right]} be the order \eqn{r} identity matrix
  with corresponding unit vectors \eqn{{{{\bf{a}}_i}}} with one in
  its \eqn{i}th position and zeros elsewhere.  
  Let \eqn{{{\bf{I}}_c} = \left[ {\begin{array}{*{20}{c}}
  {{{\bf{b}}_1}}&{{{\bf{b}}_2}}& \cdots &{{{\bf{b}}_c}}
  \end{array}} \right]} be the order \eqn{c} identity matrix
  with corresponding unit vectors \eqn{{{{\bf{b}}_i}}} with one in
  its \eqn{i}th position and zeros elsewhere.
  The \eqn{r \times c} matrix \eqn{{\bf{H}}{}_{i,j} = {{\bf{a}}_i}\;{{\bf{b'}}_j}}
  is used in the computation of the commutation matrix.
}
\value{
  A list with \eqn{r} components
  \item{1 }{A sublist of \eqn{c} components}
  \item{2 }{A sublist of \eqn{c} components}
 ...
  \item{r }{A sublist of c components}
  Each component \eqn{j} of sublist \eqn{i} is a matrix \eqn{{\bf{H}}_{i,j}}
}
\references{
  Magnus, J. R. and H. Neudecker (1979). The commutation matrix: some properties and
  applications, \emph{The Annals of Statistics}, 7(2), 381-394.
  
  Magnus, J. R. and H. Neudecker (1980). The elimination matrix, some lemmas and applications,
  \emph{SIAM Journal on Algebraic Discrete Methods}, 1(4), December 1980, 422-449.
  
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  The argument n must be an integer value greater than or equal to two.
}
\examples{
H.2.3 <- H.matrices( 2, 3 )
H.3 <- H.matrices( 3 )
}
\keyword{ math }
