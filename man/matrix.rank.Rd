\name{matrix.rank}
\alias{matrix.rank}
\title{ Rank of a square matrix }
\description{
  This function returns the rank of a square numeric matrix based on the selected method.
}
\usage{
matrix.rank(x, method = c("qr", "chol"))
}
\arguments{
  \item{x}{ a matrix }
  \item{method}{ a character string that specifies the method to be used }
}
\details{
  If the user specifies "qr" as the method, then the QR decomposition function is used to obtain the rank.
  If the user specifies "chol" as the method, the rank is obtained from the attributes of the value returned.
}
\value{
  An integer.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\note{
  If the argument is not a square numeric matrix, then the function presents an error message and stops.
}
\seealso{
  \code{\link{is.square.matrix}}
}  
\examples{
A <- diag( seq( 1, 4, 1 ) )
matrix.rank( A )
B <- matrix( seq( 1, 16, 1 ), nrow=4, byrow=TRUE )
matrix.rank( B )
}
\keyword{ math }
