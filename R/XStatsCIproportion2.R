#' Estimates Confidence Interval for the difference between 2 Sample Proportions
#' 
#' @description 
#' Inferences on one proportion are: 
#' \cr Confidence Intervals 
#' \cr Significance Tests  
#' \cr
#' \cr Criteria for both, are:  
#' \cr Randomness
#' \cr Normality
#' \cr Independence
#'
#' @param n1 Sample 1 Size
#' @param p1 Sample 1 Proportion
#' @param n2 Sample 2 Size
#' @param p2 Sample 2 Proportion
#' @param CL Confidence Level ( default=0.95 ) 
#' @param SL Significance Level for the Hypothesis test for difference in proportions ( default=0.05 ) 
#'
#' @export
#'
#' @examples
#' XStatsCIproportion2(120,54/120,140,77/140,0.9)
XStatsCIproportion2 = function(n1,p1,n2,p2,CL=0.95,SL=0.05) {
  # n1 = 100
  # p1 = 58/100
  # n2 = 100
  # p2 = 52/100
  # CL = 0.9
  # SL = 0.05
  SE = sqrt( p1 * ( 1 - p1 ) / n1 + p2 * ( 1 - p2 ) / n2 )
  q = qnorm(c(0.5 - CL/2,0.5 + CL/2),p2 - p1,SE)
  p = p2 - p1
  z = ( q[2] - p ) / SE
  
  p  = round(p2 - p1,digits=5)
  z  = round(z,digits=5)
  zabs = abs(z)
  SE = round(SE, digits=5)
  step = round( q[2] - p,digits = 4)
  q1 = round(q[1], digits=5)
  q2 = round(q[2], digits=5)
  CL = round(CL * 100,digits = 2)
  
  message("For 2 Simple Random Samples (SRS) with:")
  message("Sample 1 Size       : ",n1)
  message("Sample 1 Proportion : ",p1)
  message("Sample 2 Size       : ",n2)
  message("Sample 2 Proportion : ",p2)
  message("Confidence Level    : ",CL,"%")
  message("We get:")
  message("Difference in Proportions : ",p)
  message("Standard Error      : ",SE)
  message(CL,"% Confidence Interval for the difference in proportions: [ ",q1," , ",q2," ]")
  message("Critical value (z score) : ",z," [ -",zabs," , ",zabs," ] = p +- z*SE = ",p," +- ",step)
  message("Normality Criteria of the Sampling Distribution:")
  message("n1 * p1 > 10         : ", n1*p1>10)
  message("n1 * ( 1 - p1 ) > 10 : ", n1*(1-p1) > 10)
  message("n2 * p2 > 10         : ", n2*p2>10)
  message("n2 * ( 1 - p2 ) > 10 : ", n2*(1-p2) > 10)
  
  # Hypothesis test for difference in proportions
  message("")
  message("Hypothesis test for difference in proportions:")
  n = n1 + n2
  p = ( n1*p1 + n2*p2 ) / n
  SE = sqrt( p * ( 1 - p ) / n1 + p * ( 1 - p ) / n2 )
  message("n = ",n)
  message("p = ",p)
  message("SE = ",round(SE,digits = 4))
  zstatistic = abs(p2 - p1)/SE
  message("z statistic = ",round(zstatistic,digits = 4))
  message("Significance Level = ",SL)
  
  if ( zstatistic >= 0 ) {
    pvalue = 1 - pnorm(zstatistic)
  } else {
    pvalue = pnorm(zstatistic)
  }
  message("1 sided p-value : ",round(pvalue,digits = 4 ) )
  message("2 sided p-value : ",round(2 * pvalue,digits = 4 ) )
  message(" ( if p-value < Significance Level, then reject the Null Hypothesis )")
  message("Type I error when you erroneously reject the Null Hypothesis")
  message("Type II error when you erroneously fail to reject the Null Hypothesis")
  message("Statistical Power = Probability(rejecting  Null | Null is false)")
  message("                  = 1 - Probability(not rejecting  Null | Null is false) = 1 - Type II Error")
  message("Increasing sample size or significance level, will increase the power")
  if ( pvalue > SL ) {
    message("We failed to reject the Null Hypothesis: There are no differences in the proportions")
  } else {
    message("We reject the Null Hypothesis: There's enough evidence that there are differences in the proportions")
  }
} # End Function




  


