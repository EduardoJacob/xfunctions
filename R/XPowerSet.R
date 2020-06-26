#' Returns a List with the Power Set of a set S 
#'
#' The Power Set is the set of all subsets of S, 
#' including the empty set and S itself
#' Cardinality of the Power Set must be 2^n with n = number of elements in the set
#' Remember that matematically, a Set cannot have repeated elements,
#' So this function will apply a unique() to the input vector
#'
#' @param v 
#'
#' @return
#' @export
#'
#' @examples
#' x = XPowerSet()
#' x = XPowerSet(letters[1:1])
#' x = XPowerSet(letters[1:2])
#' x = XPowerSet(letters[1:3])
#' x = XPowerSet(c("bu","bu","cx","a"))
XPowerSet = function(v) {
  # library("gtools")
  # v = c("a","b","c")
  
  L = list(NULL)
  if ( missing(v) ) return(L)
  v = sort(unique(v))
  n = length(v)
  
  for (r in 1:n) {
    x = gtools::combinations(n,r,v) # matrix
    x = split(x, row(x))            # split matrix to list by rows
    L = c(L,x)
  }
  
  return(unname(L))
} # XPowerSet





