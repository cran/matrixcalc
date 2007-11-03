is.positive.definite <- function( x, tol, method = c("eigen","chol" ) )
{
###
### this function determines if the given matrix is positive definite
###
### parameters
### x = a square numeric matrix object
###
    if ( !is.square.matrix( x ) )
        stop( "argument x is not a square matrix" )
    method <- match.arg(method)
    if (method == "eigen") {
        eval <- eigen(x, only.values = TRUE)$values
        if( missing(tol) ) {
            tol <- max(dim(x)) * max( abs(eval) ) *.Machine$double.eps
        }
        if (sum(eval > tol) == length(eval)) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } 
    else if (method == "chol") {
        val = try(chol(x), silent = TRUE)
        if (class(val) == "try-error") {
            return(FALSE)
        }
        else {
            return(TRUE)  
        }  
    }
}
