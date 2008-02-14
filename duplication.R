
# http://velveeta.che.wisc.edu/cgi-bin/viewcvs.cgi/~checkout~/octave/scripts/linear-algebra/duplication_matrix.m?rev=HEAD&content-type=text/plain
# (snip)
## See Magnus and Neudecker (1988), Matrix differential calculus with
## applications in statistics and econometrics.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 8 May 1995
## Adapged-By: jwe



duplication_matrix <- function (n=1){

  if ( (n<1) |  (round (n) != n) )
    stop ("duplication_matrix: n must be a positive integer")

  d <- matrix (0, n * n, n * (n + 1) / 2)

  ## It is clearly possible to make this a LOT faster!
  count = 0
  for (j in 1 : n){
    d [(j - 1) * n + j, count + j] = 1
    if(j<n) {
      for (i in (j + 1) : n){
       d [(j - 1) * n + i, count + i] = 1
       d [(i - 1) * n + j, count + i] = 1
    }}
    count = count + n - j
  }
  d
}
