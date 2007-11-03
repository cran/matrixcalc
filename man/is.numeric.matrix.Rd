\name{is.numeric.matrix}
\alias{is.numeric.matrix}
\title{ Determine if argument is a numeric matrix }
\description{
  This function returns TRUE of the argument is a matrix object with numeric values.
}
\usage{
is.numeric.matrix(x)
}
\arguments{
  \item{x}{ an R object }
}
\value{
  TRUE or FALSE.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
A <- matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE )
is.numeric.matrix( A )
D <- data.frame( A )
is.numeric.matrix( D )
}
\keyword{ math }
