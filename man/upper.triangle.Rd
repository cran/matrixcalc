\name{upper.triangle}
\alias{upper.triangle}
\title{ Upper triangle portion of a matrix }
\description{
  Returns the lower triangle including the diagonal of a square numeric matrix.
}
\usage{
upper.triangle(x)
}
\arguments{
  \item{x}{ a matris }
}
\value{
  A matrix.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{is.square.matrix}}
}
\examples{
A <- matrix( seq( 1, 9, 1 ), nrow=3, byrow=TRUE )
upper.triangle( A )
}
\keyword{ math }
