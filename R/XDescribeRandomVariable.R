
#' @export

XDescribeRandomVariable = function(Xname,X,p) {
  options(digits=4)
  E = sum(p*X)
  var = sum(p*(X-E)^2)
  sd = sqrt(var)
  L = length(X)
  cat(Xname,"= (",X,") p(",p,") E:",E,"var:",var,"sd:",sd,"Length:",L,"\n")
  return(list(E=E,var=var,sd=sd,L=L))
}


# X = c(0,1,2)
# Y = c(3,4)
# p = rbind(c(0.1,0.2,0.2),c(0.1,0.2,0.2))
# XJointPMF(X,Y,p)
# 
# p = rbind(c(0.1,0.2,0.2),c(0.2,0.2,0.1))
# XJointPMF(X,Y,p)




















