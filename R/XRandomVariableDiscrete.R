#' Analyse and Plot Discrete Random Variable
#'
#' @param X     Vector of Numerical Values
#' @param p     Associated Vector of probabilities (Optional)
#'
#' @export
#'
#' @examples
#' XRandomVariableDiscrete(1:10)
XRandomVariableDiscrete = function(X,p) {
  # X = c(1,2,3,5,6,7)
  # p = c(.1,.2,.2,.3,.1,.1)
  # Xname = "Batata"
  Xname = deparse(substitute(X))
  L = length(X)
  if ( missing(p) ) p = rep(1/L,L)
  E = sum(p*X)
  var = sum(p*(X-E)^2)
  sd = sqrt(var)
  pre = 4 # precision
  
  cat("Discrete Random Variable",Xname,": (",X,")\n")
  cat("Num Elements       :",L,"\n")
  cat("Probability Vector : (",p,")\n")
  cat("Sum(Probabilities) :",sum(p),"\n")
  cat("Expected Value     :",round(E,pre),"\n")
  cat("Variance           :",round(var,pre),"\n")
  cat("Standard Deviation :",round(sd,pre),"\n")
  
  color = "orange"
  
  par(mfrow=c(1,2))
  
  plot(X,p,type="h",col=color,xlab="x",ylab="Probability" ,
       lwd=5,panel.first=grid(),ylim=c(0,1.02*max(p)),
       main=paste("Probability Mass Function for",Xname))
  
  X = sample(X,10000,replace = T,prob=p)
  plot(ecdf(X), col = color)
  grid()
  
  par(mfrow=c(1,1))
  
} # XRandomVariableDiscrete























