is.numeric.matrix <- function( x )
{
###
### this function determines if the argument is a matrix object with numerical values
###
### Parameter
### x = an R object
###
    if ( !is.matrix( x ) )
        return( FALSE )
    if ( !is.numeric( x ) )
        return( FALSE )
    return( TRUE )
}
