#' The (non-central) Chi-Squared Distribution
#' 
#' The mean on ChiSquare is = Degrees Of Freedom
#' SD is sqrt(2*DF)
#'
#' @param df 
#' @param ConfidenceLevel 
#'
#' @return
#' @export
#'
#' @examples
XChiSquare = function(df=2,ConfidenceLevel=0.95) {
  mean = df
  variance = 2*df
  sd = sqrt(variance)
  Xcritical = round(qchisq(ConfidenceLevel,df),3)
  
  plot(function(x) dchisq(x,df),0,1.5*max(Xcritical),
       col="black",las=1,panel.first=grid(),
       main=paste("Chi-Square, Degrees of Freedom:",df,", Xcritical:",paste(Xcritical,collapse=",") ),
       xlab=expression(X^2),ylab="")
  
  # Xcritical:",round(Xcritical,4),
  #                "for Confidence Level",ConfidenceLevel)
  L = length(Xcritical)
  for (i in 1:L) abline(v=Xcritical[i],col=i+1)
  
  legend("topright",legend=ConfidenceLevel,title="Confidence Levels",lty=1,col=c(1:L)+1)
}

# XChiSquare(5,0.98)
# XChiSquare(5,c(0.90,0.95,0.99))

