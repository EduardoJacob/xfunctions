
#' @export

# Recieves population_mean, observed_mean, observed_sd and sample size
XZtest = function(pmean,omean,osd,n) {
  # Alpha level is 5% (Level of significance - the probability of making a type I Error)
  r = 0.95
  
  # Round Precision
  rp = 4
  # Standard Error
  SE = osd / sqrt(n)
  z_statistic = (omean - pmean) / SE
  
  text = paste("One Sample Z test for Population mean",
               round(pmean,rp),"- Observed mean and sd:",round(omean,rp),round(osd,rp))
  print(text)
  text = paste("Sample size:",n,"SE:",round(SE,rp),"z-statistic:",round(z_statistic,rp))
  print(text)
  
  
  q = qnorm(c(0.5 - r/2,0.5 + r/2),pmean,SE)
  critical = qnorm(c(0.5 - r/2,0.5 + r/2))
  s = qnorm(c(0.5 - r/2,0.5 + r/2),omean,SE)
  text = paste("Critical Values are:",round(critical[1],rp),round(critical[2],rp))
  print(text)
    
  text = paste("The",100*r,"% confidence interval is [",round(q[1],rp),",",round(q[2],rp),"] (centered on Population mean)")
  print(text) 
  text = paste("The",100*r,"% confidence interval is [",round(s[1],rp),",",round(s[2],rp),"] (centered on Sample mean)")
  print(text) 
  if (omean > q[2] | omean < q[1] ) {
    text = paste("Observed mean",round(omean,rp),"outside confidence interval")
    print(text)
    text = paste("We conclude for the Alternative hyphotesys")
    print(text)
  } else {
    text = paste("Observed mean",round(omean,rp),"inside confidence interval")
    print(text)
    text = paste("We conclude for the NULL hyphotesys")
    print(text)
  }
  
  # p-value is the chance assuming the NULL is true, of getting the data in the sample
  # or even more in the direction of the alternative
  if (omean > pmean) {
    pvalue = 1 - pnorm(omean,pmean,SE)
    text = paste("p-value is",round(pvalue,4),"for One Tail Z Test: Probability of",round(omean,rp),"or more")
  } else {
    pvalue = pnorm(omean,pmean,SE)
    text = paste("p-value is",round(pvalue,4),"for One Tail Z Test: Probability of",round(omean,rp),"or less")
  }
  print(text)
  pvalue = 2*pvalue
  text = paste("p-value is",round(pvalue,4),"for Two Tail Z Test")
  print(text)
  zvalue = (omean - pmean) / SE
  text = paste("z-value is",round(zvalue,4),"for Two Tail Z Test")
  print(text)
  
  # Superimpose the Normal Curve
  xmin = pmean - 5*SE
  xmax = pmean + 5*SE
  plot(function(x) dnorm(x,pmean,SE),xmin,xmax,col="blue",lwd=2,las=1,main="One Sample Z-Test for Population mean")
  mtext(paste("Level of Significance",1-r))
  
  # Superimpose points
  xv = sapply(-3:3,function(x) pmean + x*SE)
  yv = sapply(xv,function(x) dnorm(x,pmean,SE))
  points(xv,yv,pch=19, col="red")
  abline(v=omean,col="green")
  abline(v=q[1],col="red")
  abline(v=q[2],col="red")
  
} # End Function







