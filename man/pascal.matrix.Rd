\name{pascal.matrix}
\alias{pascal.matrix}
\title{ Pascal matrix }
\description{
  This function returns an n by n Pascal matrix.
}
\usage{
pascal.matrix(n)
}
\arguments{
  \item{n}{ Order of the matrix }
}
\value{
  A matrix.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) Matrix Differential Calculus with Applications in Statistics and Econometrics,
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{ ~~further notes~~ 
  If the argument n is not a positive integer, the function presents an error message and stops.
}
\examples{
p <- pascal.matrix( 4 )
}
\keyword{ math }
