
#' @export

XIsNormalyDistributed = function(X) {
  par(mfrow=c(2,2)) #prepare for a 2x1 layout
  
  plot(density(X),main = "Is normaly distributed X ?")
  plot(function(x) dnorm(x,mean=mean(X),sd=sd(X)),from=min(X),to=max(X),col="red",add=T)
  simulated = rnorm(length(X),mean=mean(X),sd=sd(X))
  qqplot(simulated,X)
  lines(seq(0,max(X)),seq(0,max(X)),lwd=3,col="red")
  
  X = log(X)
  plot(density(X),main = "Is normaly distributed log(X) ?")
  plot(function(x) dnorm(x,mean=mean(X),sd=sd(X)),from=min(X),to=max(X),col="green",add=T)
  simulated = rnorm(length(X),mean=mean(X),sd=sd(X))
  qqplot(simulated,X)
  lines(seq(0,max(X)),seq(0,max(X)),lwd=3,col="green")
  
  par(mfrow=c(1,1)) #Restore 1x1 layout
}



