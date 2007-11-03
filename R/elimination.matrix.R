elimination.matrix <- function( n )
{
###
### this function returns a matrix sith n * n comuns and n * (n + 1 ) / 2
### rows that transforms vec( A ) to vech( A ) where A is a symmetric n by n matrix
###
### Parameter
### n = the order of the matrix
###
    if ( n <= 0 )
        stop( "argument n is not positive" )
    if ( n != trunc( n ) )
        stop( "argument n is not an integer" )
    A <- matrix( 0, nrow=n, ncol=n )
    for ( i in 1:n )
        for ( j in 1:n )
            A[i,j] <- i + j
    vechA <- vech( A )
    vecA <- vec( A )
    q <- n * n
    p <- n * ( n + 1 ) / 2
    D <- matrix( 0, nrow=p, ncol=q )
    for ( i in 1:p ) {
        value.set <- FALSE
        for ( j in 1:q ) {
            if ( !value.set ) {
                D[i,j] <- as.numeric( vecA[j,1] == vechA[i,1] )
                if ( D[i,j] == 1 )
                    value.set <- TRUE
            }    
        }
    }  
    return( D )
}
