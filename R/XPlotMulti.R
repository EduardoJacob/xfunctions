#' Plot a List of Functions
#'
#' @param myfunctions Receives a Vector of 1 or more Functions
#' @param x1 plot from
#' @param x2 plot to
#'
#' @return
#' @export
#'
#' @examples
#' f1 = function(x) cos(x)
#' f2 = function(x) sin(x)
#' f3 = function(x) x^2
#' XPlotMulti(c(f1,f2,f3),-pi,pi)
XPlotMulti = function(myfunctions,x1,x2) {
  addFunction = FALSE
  myColors = rainbow(length(myfunctions))
  n = 1
  myfunctionNames = c()
  
  for ( f in myfunctions ) {
      # curve(f, x1, x2, col=myColors[n], xlab="x", ylab="y",add = addFunction, asp=1 )
      curve(f, xlim = c(x1,x2),ylim=c(x1,x2), col=myColors[n], xlab="x", ylab="y",add = addFunction, asp=1 )
      grid()
      abline(v=0)
      abline(h=0)
      addFunction = TRUE
      n=n+1
      myfunctionName = tail( deparse(substitute(f)) ,1)
      myfunctionNames = c(myfunctionNames,myfunctionName)
  }
  
  legend("topright",legend=myfunctionNames,lty=1,col=myColors)
} # XPlotMulti

# f1 = function(x) 3/4 + 3*x/4
# f2 = function(x) 7/8 + 3*x/4
# XPlotMulti(c(f1,f2),-1,1)


 



