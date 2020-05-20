
#' @export

XTwoSampleTtest = function(A,B,paired=F) {
  par(mfrow=c(2,2)) #prepare for a 2x2 layout
  c1 = "orange"
  c2 = "green"
  XHistogram(A,name="A",color=c1)
  XHistogram(B,name="B",color=c2)
  
  # alpha is the significance level of the test
  alpha = 0.05
  mean1 = mean(A)
  mean2 = mean(B)
  # Difference in the means
  diff = mean1 - mean2 
  n1 = length(A)
  n2 = length(B)
  sd1 = sd(A) * sqrt((n1-1)/n1) # we need the exact Standard Deviations
  sd2 = sd(B) * sqrt((n2-1)/n2) 
  SE = sqrt((n1*sd1^2 + n2*sd2^2)/(n1+n2-2)) * sqrt((n1+n2)/(n1*n2))
  t_statistic = round(diff / SE,3)
  print(paste("t_statistic is",t_statistic))
  # SE1 = sd(A)/sqrt(n1)
  # SE2 = sd(B)/sqrt(n2)
  # SE = sqrt(SE1^2 + SE2^2)
  # t_statistic = (mean1-mean2) / SE
  
  x = t.test(A,B,paired=paired)
  print(x)
  #x$p.value
  #x$conf.int
  #abs(x$conf.int[1] - x$conf.int[2])
  df = x$parameter 
  t_statistic = x$statistic
  t_critical = abs(qt(alpha/2,df)) # For non directional test
  # t_critical = abs(qt(alpha,df)) # For directional test
  
  boxplot(list(A,B),col=c(c1,c2),names=c("A","B"),
          main=paste("Diff in the means:",round(diff,2)),
          xlab=paste("t-statistic:",round(t_statistic,3),",","t-critical:",round(t_critical,3)))
  
  # For Large Sample sizes, the confidence interval is:
  #ci.upper = (mean1-mean2) + SE*1.96
  #ci.lower = (mean1-mean2) - SE*1.96
  #print(ci.upper)
  #print(ci.lower)
  
  # XScatterPlot(A,B)
  
  # Draw the t-distribution
  plot(function(x) dt(x,df),
       -2*t_critical,2*t_critical,col="blue",main=paste("T distribution,",round(df,3),"degrees of freedom"))
  abline(v=t_critical,col="red")
  abline(v=-t_critical,col="red")
  # If t_statistic outside t_critical interval, we reject the Null Hyphotesis
  # No matter which way you compute it, the p-value pval is the probability that the null hypothesis 
  # could have generated a t-value more extreme than the t-value tval that we observed
  # Type I Error: the probability that we will reject the null hypothesis by mistake is equal to alpha
  abline(v=t_statistic,col="green")
  
  par(mfrow=c(1,1)) #Restore 1x1 layout
  
} # XTwoSampleTtest



