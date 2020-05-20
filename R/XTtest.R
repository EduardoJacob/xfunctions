
#' @export

# One sample T-test for population mean (tests the diference in the means)

# Recieves population_mean, observed_mean, observed_sd and sample size
Xttest = function(pmean,omean,osd,n) {
  # Round Precision
  rp = 4
  # Standard Error
  SE = osd / sqrt(n)
  # Degrees of freedom
  df = n - 1
  t_statistic = (omean - pmean) / SE
  
  text = paste("One Sample T test for Population mean",
               round(pmean,rp),"- Observed mean and sd:",round(omean,rp),round(osd,rp))
  print(text)
  text = paste("Sample size:",n,"SE:",round(SE,rp),"t-statistic:",round(t_statistic,rp))
  print(text)
  text = paste("Degrees of Freedom:",df)
  print(text)
  
  # Alpha level is 5%
  r = 0.95
  q = qt(c(0.5 - r/2,0.5 + r/2),df)
  # critical_values:
  c1 = q[1] * SE + pmean
  c2 = q[2] * SE + pmean
  s1 = q[1] * SE + omean
  s2 = q[2] * SE + omean
  text = paste("Critical Values are:",round(q[1],rp),round(q[2],rp))
  print(text)
  
  text = paste("The",100*r,"% confidence interval is [",round(c1,rp),",",round(c2,rp),"] (centered on Population mean)")
  print(text) 
  text = paste("The",100*r,"% confidence interval is [",round(s1,rp),",",round(s2,rp),"] (centered on Sample mean)")
  print(text) 
  if (omean > c2 | omean < c1 ) {
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
    pvalue = 1 - pt(t_statistic,df)
    text = paste("p-value is",round(pvalue,4),"for One Tail T Test: Probability of",round(omean,rp),"or more")
  } else {
    pvalue = pt(t_statistic,df)
    text = paste("p-value is",round(pvalue,4),"for One Tail T Test: Probability of",round(omean,rp),"or less")
  }
  print(text)
  pvalue = 2*pvalue
  text = paste("p-value is",round(pvalue,4),"for Two Tail T Test")
  print(text)
  text = paste("t-statistic is",round(t_statistic,4),"for Two Tail T Test")
  print(text)
  
  # Superimpose the T Curve
  xmin = pmean - 5*SE
  xmax = pmean + 5*SE
  plot(function(x) dt((x-pmean)/SE,df),xmin,xmax,col="blue",lwd=2)
  
  # Superimpose points
  xv = sapply(-3:3,function(x) pmean + x*SE)
  yv = sapply(xv,function(x) dt((x-pmean)/SE,df))
  points(xv,yv,pch=19, col="red")
  abline(v=omean,col="green")
  abline(v=c1,col="red")
  abline(v=c2,col="red")
  
} # End Function







