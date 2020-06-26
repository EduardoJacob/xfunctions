#' Plots a function
#'
#' @param f 
#' @param x1 plot function from x1
#' @param x2 plot function to x2
#' @param x0 Optional, studies the point x0, defaults to x0=(x1+x2)/2
#'
#' @return
#' @export
#'
#' @examples
#' XPlot(function(x) 2*x^2+3*x,-5,5,1)
#' XPlot(function(x) sin(x),0,2*pi)
XPlot = function(f,x1,x2,x0=(x1+x2)/2) {
  # library("numDeriv")
  
#   f = function(x) 5 - x^2
#   x1 = -5
#   x2 = 5
#   x0 = 2
  y0 = f(x0)
  
  plot(f, x1, x2, col="red", xlab="x", ylab="y", main=body(f) )
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
  i = stats::integrate(f,x1,x2)
  cat("Integral at [",x1,",",x2,"] =",round(i$value,3),"\n")
}

# Search for a Root in an interval:
# uniroot(f,c(-3,0))

# search for maximum in an interval:
# optimise(f,c(-5,5),maximum=TRUE)

# One Sided Limits and Overall Limits
# e = 0.00001
# f(x0-e)
# f(x0+e)














