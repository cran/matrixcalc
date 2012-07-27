\name{svd.inverse}
\alias{svd.inverse}
\title{ SVD Inverse of a square matrix }
\description{
  This function returns the inverse of a matrix using singular value
  decomposition.  If the matrix is a square matrix, this should be equivalent
  to using the \code{solve} function.  If the matrix is not a square matrix,
  then the result is the Moore-Penrose pseudo inverse.
}
\usage{
svd.inverse(x)
}
\arguments{
  \item{x}{ a numeric matrix }
}
\value{
  A matrix.
}
\references{
  Bellman, R. (1987). \emph{Matrix Analysis}, Second edition, Classics in Applied Mathematics,
  Society for Industrial and Applied Mathematics.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
A <- matrix( c ( 1, 2, 2, 1 ), nrow=2, byrow=TRUE)
invA <- svd.inverse( A )
}
\keyword{ math }
