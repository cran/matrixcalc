hadamard.prod <- function( x, y )
{
###
### this function calculates the Hadamard product of two matrices x and y.
### the matrices must the same row and column order
###
### Parameters
### x = a numeric matrix object
### y = a numeric matrix object
###
    if ( !is.numeric.matrix( x ) )
        stop( "argument x is not a numeric matrix" )
    if ( !is.numeric.matrix( y ) )
        stop( "argument y is not a numeric matrix" )
    if ( nrow( x ) != nrow( y ) )
        stop( "argumentx x and y do not have the same row order" )
    if ( ncol( x ) != ncol( y ) )
        stop( "arguments x and y do not have the same column order" )
    return( x * y )
}
