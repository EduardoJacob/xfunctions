#' Estimates Confidence Interval for Population Proportion
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
#' @param n Sample Size
#' @param p Proportion Observed on the Sample (or proportion of the population for Significance Test)
#' @param CL Confidence Level ( default=0.95 ) 
#' @param N Population Size 
#' @param t Proportion to evaluate ( Significance Test )  
#'
#' @export
#'
#' @examples
#' XStatsCIproportion(100,0.54,0.95)
XStatsCIproportion = function(n,p,CL=0.95,N=1000000,t) {
  # n = 100
  # p = 0.54
  # CL = 0.95
  # N = 1000000
  SE = sqrt( p * ( 1 - p ) / n )
  q = qt(c(0.5 - CL/2,0.5 + CL/2),p,SE)
  z = ( q[2] - p ) / SE
  # percent = ( q[2] - p ) / p

  p  = round(p,digits=5)
  z  = round(z,digits=5)
  zabs = abs(z)
  SE = round(SE, digits=5)
  step = round( q[2] - p,digits = 4)
  q1 = round(q[1], digits=5)
  q2 = round(q[2], digits=5)
  CL = round(CL * 100,digits = 2)
  # percent = round(percent * 100,digits = 2)
  
  message("For Simple Random Sample (SRS) with:")
  message("Sample Size         : ",n)
  message("Population Size     : ",N)
  message("Observed Proportion : ",p," ( or Population Proportion for Significance Test )")
  message("Confidence Level    : ",CL,"%")
  message("We get:")
  message("Standard Error      : ",SE)
  message(CL,"% Confidence Interval for the true proportion: [ ",q1," , ",q2," ]")
  message("Critical value (z score) : ",z," [ -",zabs," , ",zabs," ] = p +- z*SE = ",p," +- ",step)
  message("Normality Criteria of the Sampling Distribution:")
  message("n * p > 10         : ", n*p>10)
  message("n * ( 1 - p ) > 10 : ", n*(1-p) > 10)
  message("Sampling size n less then 10% of the Population size (Independence Criteria) : ",n/N < 0.1)
  message("( For sampling whitout replacement )")
  
  if ( !missing(t) ) {
    message("")
    message("Significance Test:")
    message("( If you made a simulation, p-value will be the proportion of cases of interest )")
    zstatistic = (t - p)/SE
    message("For proportion ",round(t,digits = 4),":")
    message("Formula for z-statistic is: (statistic - parameter)/(standard deviation of statistic)")
    message("z-statistic : ",round(zstatistic,digits = 4 ))
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
  }
} # End Function




  


