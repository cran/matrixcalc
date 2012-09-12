\name{stirling.matrix}
\alias{stirling.matrix}
\title{ Stirling Matrix }
\description{
  This function constructs and returns a Stirling matrix which is
  a lower triangular matrix containing the Stirling numbers of
  the second kind.
}
\usage{
stirling.matrix(n)
}
\arguments{
  \item{n}{  A positive integer value }
}
\details{
  The Stirling numbers of the second kind, \eqn{S_i^j}, are used
  in combinatorics to compute the number of ways a set of \eqn{i} objects
  can be partitioned into \eqn{j} non-empty subsets \eqn{j \le i}.  The numbers are also
  denoted by 
  \eqn{\left\{ {\begin{array}{*{20}{c}}i\\j\end{array}} \right\}}.  Stirling numbers of
  the second kind can be computed recursively with the equation
  \eqn{S_j^{i + 1} = S_{j - 1}^i + j\;S_j^i,\quad 1 \le i \le n - 1,\;1 \le j \le i}.
  The initial conditions for the recursion are
  \eqn{S_i^i = 1,\quad 0 \le i \le n} and
  \eqn{S_j^0 = S_0^j = 0,\quad 0 \le j \le n}.  The resultant numbers are organized
  in an order \eqn{n + 1} matrix
  \eqn{\left[ {\begin{array}{*{20}{c}}
{S_0^0}&0&0& \cdots &0\\
0&{S_1^1}&0& \cdots &0\\
0&{S_1^2}&{S_2^2}& \cdots &0\\
 \cdots & \cdots & \cdots & \cdots & \cdots \\
0&{S_1^n}&{S_2^n}& \cdots &{S_n^n}
\end{array}} \right]}.
}
\value{
  An order \eqn{n + 1} lower triangular matrix.
}
\references{
  Aceto, L. and D. Trigiante (2001). Matrices of Pascal and Other Greats,
  \emph{American Mathematical Monthly}, March 2001, 108(3), 232-245.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument n is not a positive integer, the function presents an error message and stops.
}
\examples{
S <- stirling.matrix( 10 )
print( S )
}
\keyword{ math }
