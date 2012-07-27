\name{lower.triangle}
\alias{lower.triangle}
\title{ Lower triangle portion of a matrix }
\description{
  Returns the lower triangle including the diagonal of a square numeric matrix.
}
\usage{
lower.triangle(x)
}
\arguments{
  \item{x}{ a matrix }
}
\value{
  A matrix.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.
}
\seealso{
  \code{\link{is.square.matrix}}
}
\examples{
B <- matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE )
lower.triangle( B )
}
\keyword{ math }
