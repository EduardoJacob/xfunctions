#' Analyse and Plot Continuous Random Variable
#'
#' @param f     Probability Function
#' @param x1    Defined from x1
#' @param x2    to x2
#'
#' @export
#'
#' @examples
#' XRandomVariableContinuous(f = function(x) 3*x*(3-x)/10,x1=0,x2=2)
XRandomVariableContinuous = function(f,x1,x2) {
  #   f = function(x) 3*x*(3-x)/10 
  #   x1 = 0
  #   x2 = 2
  Xname = deparse(substitute(f))
  pre = 4 # precision
  color = "orange"
  L = 1000 # Number of Points
  
  par(mfrow=c(1,2))
  
  # PROBABILITY DENSITY FUNCTION
  X = seq(x1,x2,length.out=L)
  Y = f(X)
  plot(X,Y,xlim=c(x1,x2), col=color, type="l",
            panel.first=grid(),
       xlab="x", ylab="y", 
       main=paste("Probability Density Function for",Xname))
  
  X1 = c(x1,X,x2)
  Y1 = c(0,Y,0)
  polygon(X1,Y1, col=color) 
  
  A = stats::integrate(function(x) f(x) + 0*x,x1,x2)$value  # Area under the Curve (must be 1)
  E = stats::integrate(function(x) x*f(x),x1,x2)$value      # Expected Value
  V = stats::integrate(function(x) (x-E)^2*f(x),x1,x2)$value      # Variance
  
  # CUMULATIVE DENSITY FUNCTION
  Y = vector() 
  for ( x in X ) {
    y = stats::integrate(function(x) f(x) + 0*x,x1,x) 
    Y = c(Y,y$value)
  }
  index = which(Y>0.5)[1]
  Median = (X[index] + X[index-1]) / 2
  
  plot(X,Y,xlim=c(x1,x2), col=color, type="l",
       panel.first=grid(),
       xlab="x", ylab="y",lwd=3 ,
       main=paste("Cumulative Density Function for",Xname))
  
 
  par(mfrow=c(1,1))
  
  cat("Continuous Random Variable",Xname,"\n")
  cat("Area under the Curve :",round(A,pre),"\n")
  cat("Expected Value       :",round(E,pre),"\n")
  cat("Variance             :",round(V,pre),"\n")
  cat("Standard Deviation   :",round(sqrt(V),pre),"\n")
  cat("Median               :",round(Median,pre),"\n")
  
} # XRandomVariableContinuous























