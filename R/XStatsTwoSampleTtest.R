#' Estimates Difference on the Means between two Simple Random Samples (SRS)
#' 
#' @description 
#' Inferences for One Sample T test are:: 
#' \cr Confidence Intervals 
#' \cr Significance Tests 
#' \cr
#' \cr Criteria for One Sample T test, ( Conditions for inference on a mean ) are:  
#' \cr Randomness - A random sample or randomized experiment should be used to obtain the data.
#' \cr Normality - Sample size must be at least 30, unless population distribution is normal
#' \cr Independence - Sampling size n less then 10% of the Population size
#'
#' @param n1 Sample 1 Size 
#' @param m1 Sample 1 mean
#' @param S1 Sample 1 sd
#' @param n2 Sample 2 Size 
#' @param m2 Sample 2 mean
#' @param S2 Sample 2 sd
#' @param CL Confidence Level ( default=0.95 ) 
#' @param N Population Size 
#'
#' @export
#'
#' @examples
#' XStatsTwoSampleTtest(18,38.3,.27,24,38.9,.29,CL=.9)
XStatsTwoSampleTtest = function(n1,m1,S1,n2,m2,S2,CL=0.95,N=1000000) {
  # n1 = 18
  # m1 = 38.3
  # S1 = 0.27
  # n2 = 24
  # m2 = 38.9
  # S2 = 0.29
  # CL = 0.90
  # N = 1000000
  dm = abs(m2 - m1) # difference in the means
  SE = sqrt(S1^2/n1 + S2^2/n2)
  df = min(n1,n2) - 1 # Degrees of Freedom for T Distribution
  tcritical = qt(CL + (1-CL)/2,df)
  step = tcritical * SE
  q1 = dm - step
  q2 = dm + step
  
  SE = round(SE, digits=5)
  dm = round(dm, digits=5)
  step = round(step, digits=5)
  tcritical = round(tcritical, digits=5)
  q1 = round(q1, digits=5)
  q2 = round(q2, digits=5)
  CL = round(CL * 100,digits = 2)
  # percent = round(percent * 100,digits = 2)
  
  message("Two Sample T test for difference in the means:")
  message("Sample 1 Size      : ",n1)
  message("Sample 1 mean      : ",m1)
  message("Sample 1 SD        : ",S1) 
  message("Sample 2 Size      : ",n2)
  message("Sample 2 mean      : ",m2)
  message("Sample 2 SD        : ",S2) 
  message("Population Size  : ",N)
  message("Confidence Level : ",CL,"%")
  message("We get:")
  message("Difference in the means : ",dm)
  message("Degrees of Freedom      : ",df)
  message("Standard Error          : ",SE)
  message("t critical              : ",tcritical)
  message(CL,"% Confidence Interval for dif. means: [ ",q1," , ",q2," ] = ",dm," +- ",step)
  message("Normality Criteria of the Sampling Distribution:")
  message("n1 > 30 : ", n1>30)
  message("n2 > 30 : ", n2>30)
  message("Sampling size n1 less then 10% of the Population size (Independence Criteria) : ",n1/N < 0.1)
  message("Sampling size n2 less then 10% of the Population size (Independence Criteria) : ",n2/N < 0.1)
  message("( For sampling whitout replacement )")
  
  message("")
  message("Significance Test for Difference in the Means:")
  tstatistic = dm/SE
  message("t-statistic : ",round(tstatistic,digits = 4 ))
  if ( tstatistic >= 0 ) {
    pvalue = 1 - pt(tstatistic,df)
  } else {
    pvalue = pt(tstatistic,df)
  }
  message("1 sided p-value : ",round(pvalue,digits = 4 ) )
  message("2 sided p-value : ",round(2 * pvalue,digits = 4 ) )
  message(" ( if p-value < Significance Level, then reject the Null Hypothesis )")
  message("Type I error when you erroneously reject the Null Hypothesis")
  message("Type II error when you erroneously fail to reject the Null Hypothesis")
  message("Statistical Power = Probability(rejecting  Null | Null is false)")
  message("                  = 1 - Probability(not rejecting  Null | Null is false) = 1 - Type II Error")
  message("Increasing sample size or significance level, will increase the power")

} # End Function




  


