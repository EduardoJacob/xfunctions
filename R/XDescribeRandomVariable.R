#' Describe Numerical Random Variable
#'
#' @param X     Vector of Numerical Values
#' @param p     Associated Vector of probabilities (Optional)
#'
#' @export
#'
#' @examples
#' XDescribeRandomVariable(1:10,rep(1/10,10))
XDescribeRandomVariable = function(X,p) {
  options(digits=4)
  Xname = deparse(substitute(X))
  
  L = length(X)
  if ( missing(p) ) p = rep(1/L,L)
  
  E = sum(p*X)
  var = sum(p*(X-E)^2)
  sd = sqrt(var)
   
  cat(Xname,"= (",X,") , p(",p,") , sum:",sum(p),", E:",E,", var:",var,", sd:",sd,", Length:",L,"\n")
  return(list(E=E,var=var,sd=sd,L=L))
} # XDescribeRandomVariable























