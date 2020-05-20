
#' @export

XPlotMulti = function(myfunctions,x1,x2) {
  addFunction = FALSE
  myColors = rainbow(length(myfunctions))
  n = 1
  myfunctionNames = c()
  
  for ( f in myfunctions ) {
      curve(f, x1, x2, col=myColors[n], xlab="x", ylab="y",add = addFunction )
      grid()
      abline(v=0)
      abline(h=0)
      addFunction = TRUE
      n=n+1
      myfunctionName = tail( deparse(substitute(f)) ,1)
      myfunctionNames = c(myfunctionNames,myfunctionName)
  }
  
  legend("topright",legend=myfunctionNames,lty=1,col=myColors)
}

# 
# f1 = function(x) cos(x)
# f2 = function(x) sin(x)
# f3 = function(x) x^2
# 
# XPlotMulti(c(f1,f2,f3),-pi,pi)



