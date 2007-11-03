vec <- function( x )
{
###
### this function returns a column vector that is a stack of the columns of x
###
### Parameters
### x = a numeric matrix
###
    if ( !is.numeric.matrix( x ) )
        stop( "argument x is not a numeric matrix" )
    return( t( t( as.vector( x ) ) ) )
}
