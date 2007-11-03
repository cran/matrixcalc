commutation.matrix <- function( m, n )
{
###
### this function returns a square matrix with p = m * n rows and columns
###
### Parameters
### m = integer rows
### n = integer columns
###
    if ( m <= 0 )
        stop( "argument m is not positive" )
    if ( m != trunc( m ) )
        stop( "argument m is not an integer" )
    if ( n <= 0 )
        stop( "argument n is not positive" )
    if ( n != trunc( n ) )
        stop( "argument b is not an integer" )
    p <- m * n
    C <- matrix( 0, nrow=p, ncol=p )
    r <- 0
    for ( i in 1:m ) {
        c <- i
        for ( j in 1:n ) {
            r <- r + 1
            C[r,c] <- 1
            c <- c + m
        }
    }
    return( C )
}
