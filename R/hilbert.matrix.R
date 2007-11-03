hilbert.matrix <- function(n) 
{   
###
### this function returns an n by n Hilbert matrix
###
### Parameter
### n = the row (column) dimension of the matrix
###
    if ( n <= 0 )
        stop( "argument n is not positive" )
    if ( n != trunc( n ) )
        stop( "argument n is not an integer" )
    i <- 1:n
    X <- 1 / outer(i - 1, i, "+")
    return( X  )
}
