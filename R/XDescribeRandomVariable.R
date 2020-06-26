#' Describe Random Variable
#'
#' @param Xname A Name
#' @param X     Vector of values
#' @param p     Associated Vector of probabilities
#'
#' @return
#' @export
#'
#' @examples
XDescribeRandomVariable = function(Xname,X,p) {
  options(digits=4)
  E = sum(p*X)
  var = sum(p*(X-E)^2)
  sd = sqrt(var)
  L = length(X)
  cat(Xname,"= (",X,") p(",p,") E:",E,"var:",var,"sd:",sd,"Length:",L,"\n")
  return(list(E=E,var=var,sd=sd,L=L))
} # XDescribeRandomVariable























