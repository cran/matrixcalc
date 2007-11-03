duplication.matrix <- function( n )
{
###
### this function returns a matrix sith n * n rows and n * (n + 1 ) / 2
### columns that transforms vech( A ) to vec( A ) where A is a symmetric n by n matrix
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
    p <- n * n
    q <- n * ( n + 1 ) / 2
    D <- matrix( 0, nrow=p, ncol=q )
    for ( i in 1:p ) {
        value.set <- FALSE
        for ( j in 1:q ) {
            if ( !value.set ) {
                D[i,j] <- as.numeric( vecA[i,1] == vechA[j,1] )
                if ( D[i,j] == 1 )
                    value.set <- TRUE
            }    
        }
    }  
    return( D )
}
