duplication.matrix <- function( n=1 )
{
###
### this function returns a matrix sith n * n rows and n * (n + 1 ) / 2
### columns that transforms vech( A ) to vec( A ) where A is a symmetric n by n matrix
###
### Parameter
### n = the order of the matrix
###
    if ( (n<1) |  (round (n) != n) )
        stop ("n must be a positive integer")
    d <- matrix (0, n * n, n * (n + 1) / 2)

   ## It is clearly possible to make this a LOT faster!
   count = 0
   for (j in 1 : n){
       d [(j - 1) * n + j, count + j] = 1
       if ( j < n ) {
           for (i in (j + 1):n){
               d [(j - 1) * n + i, count + i] <- 1
               d [(i - 1) * n + j, count + i] <- 1
           }
        }
        count = count + n - j
    }
    return( d )
}
