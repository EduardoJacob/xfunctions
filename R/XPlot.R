#' Plots a function
#'
#' @param f 
#' @param x1 from x1
#' @param x2 to x2
#' @param x0 Optional, studies the point x0, defaults to x0=(x1+x2)/2
#'
#' @export
#'
#' @examples
#' XPlot(function(x) 2*x^2+3*x,-5,5,1)
#' XPlot(function(x) sin(x),0,2*pi)
XPlot = function(f,x1,x2,x0=(x1+x2)/2) {
  # library("numDeriv")
  
#   f = function(x) 1/29 + x - x  
#   x1 = 1
#   x2 = 30
#   x0 = 2
  y0 = f(x0)
  
  plot(function(x) f(x) + 0*x,from=x1,to=x2, col="red", 
       xlab="x", ylab="y", main=body(f), las=1, lwd=3 )
  grid()
  abline(v=x0)
  abline(h=0)
  points(x0,y0)
  d1 = numDeriv::grad(f,x0)
  d2 = numDeriv::hessian(f,x0)
      
  # abline(a,b) draws a line of slope b and intercept a
  a = y0 - d1*x0
  b = d1
  abline(a,b, col="blue")
  
  cat("y = ")
  print(body(f))
  cat("(x0,y0) = (",x0,",",y0,")\n")
  cat("1st derivative at x =",x0,":",round(d1,3),"\n")
  cat("2nd derivative at x =",x0,":",round(d2,3),"\n")
  cat("Tangent Line at x =",x0,": y = ",b,"x +",a,"\n")
  
  # Integration
  i = stats::integrate(function(x) f(x) + 0*x,x1,x2)
  cat("Integral at [",x1,",",x2,"] =",round(i$value,3),"\n")
  
  # Search for a Root in an interval: only works if there's only one root
  # stats::uniroot(f,c(x1,x2))
  roots = rootSolve::uniroot.all(f,c(x1,x2))
  cat("Roots at the given interval :",roots,"\n")
  
  # search for maximum in an interval:
  extrema = stats::optimise(f,c(x1,x2),maximum=TRUE)
  cat("Maximum at the given interval : (",extrema$objective,",",extrema$maximum,")\n")
  
  # search for minimum in an interval:
  extrema = stats::optimise(f,c(x1,x2),maximum=FALSE)
  cat("Minimum at the given interval : (",extrema$objective,",",extrema$minimum,")\n")

}

















