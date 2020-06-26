#' Verify if a Distribution is normally distributed 
#'
#' @param X A Numeric Vector
#'
#' @return
#' @export
#'
#' @examples
#' XIsNormalyDistributed(iris$Sepal.Length)
XIsNormalyDistributed = function(X) {
  dataname = deparse(substitute(X))
  
  par(mfrow=c(2,2)) #prepare for a 2x2 layout
  
  plot(density(X),main = paste("Is normaly distributed ",dataname," ?",sep="") )
  plot(function(x) dnorm(x,mean=mean(X),sd=sd(X)),from=min(X),to=max(X),col="red",add=T)
  
  simulated = rnorm(length(X),mean=mean(X),sd=sd(X))
  qqplot(simulated,X)
  lines(seq(0,max(X)),seq(0,max(X)),lwd=3,col="red")
  
  X = log(X)
  plot(density(X),main = paste("Is normaly distributed log(",dataname,") ?",sep="") )
  plot(function(x) dnorm(x,mean=mean(X),sd=sd(X)),from=min(X),to=max(X),col="green",add=T)
  
  simulated = rnorm(length(X),mean=mean(X),sd=sd(X))
  qqplot(simulated,X)
  lines(seq(0,max(X)),seq(0,max(X)),lwd=3,col="green")
  
  par(mfrow=c(1,1)) #Restore 1x1 layout
  
  invisible()
} # XIsNormalyDistributed



