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
\value{
  A matrix.
}
\references{
  Magnus, J. R. and H. Neudecker (1999) Matrix Differential Calculus with Applications in Statistics and Econometrics,
  Second Edition, John Wiley.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
alpha <- c( .1, .2, .3, .4 )
V <- vandermonde.matrix( alpha, 4 )
}
\keyword{ math }
