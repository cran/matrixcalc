\name{fibonacci.matrix}
\alias{fibonacci.matrix}
\title{ Fibonacci Matrix }
\description{
  This function constructs the order n + 1 square Fibonacci matrix
  which is derived from a Fibonacci sequence.
}
\usage{
fibonacci.matrix(n)
}
\arguments{
  \item{n}{ a positive integer value }
}
\details{
  Let \eqn{\left\{ {{f_0},\;{f_1},\; \ldots ,\;{f_n}} \right\}} be the
  set of \eqn{ n + 1} Fibonacci numbers where \eqn{{f_0} = {f_1} = 1}
  and \eqn{{f_j} = {f_{j - 1}} + {f_{j - 2}},\quad 2 \le j \le n}.  The
  order \eqn{n + 1} Fibonacci matrix \eqn{{\bf{F}}} has as typical element
  \eqn{{F_{i,j}} = \left\{ {\begin{array}{*{20}{c}}
{{f_{i - j + 1}}}&{i - j + 1 \ge 0}\\
0&{i - j + 1 < 0}
\end{array}} \right.}.
}
\value{
  An order \eqn{n + 1} matrix
}
\references{
  Zhang, Z. and J. Wang (2006). Bernoulli matrix and its algebraic properties,
  \emph{Discrete Applied Nathematics}, 154, 1622-1632.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument n is not a positive integer, the function presents an error message and stops.
}
\examples{
F <- fibonacci.matrix( 10 )
print( F )
}
\keyword{ math }
