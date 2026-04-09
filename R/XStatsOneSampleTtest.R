#' Estimates Population Mean from a Simple Random Sample (SRS)
#' 
#' @description 
#' Inferences for One Sample T test ( or for a Paired T test ) are:: 
#' \cr Confidence Intervals 
#' \cr Significance Tests 
#' \cr
#' \cr Criteria for One Sample T test, ( Conditions for inference on a mean ) are:  
#' \cr Randomness - A random sample or randomized experiment should be used to obtain the data.
#' \cr Normality - Sample size must be at least 30, unless population distribution is normal
#' \cr Independence - Sampling size n less then 10% of the Population size
#'
#' @param n Sample Size 
#' @param m Sample mean ( or difference mean for a Paired T test )
#' @param S Sample sd
#' @param CL Confidence Level ( default=0.95 ) 
#' @param N Population Size 
#' @param t Mean to evaluate ( for Significance Tests )  
#'
#' @export
XStatsOneSampleTtest = function(n,m,S,CL=0.95,N=1000000,t) {
  # n = 14
  # m = 700
  # CL = 0.95
  # S = 50
  # N = 1000000
  df = n - 1 # Degrees of Freedom for T Distribution
  tcritical = qt(CL + (1-CL)/2,df)
  zcritical = qnorm(CL + (1-CL)/2)
  SE = S / sqrt( n )
  SE = SE * sqrt( (N-n)/(N-1) )  
  stept = tcritical * SE
  stepz = zcritical * SE
  tq1 = m - stept
  tq2 = m + stept
  zq1 = m - stepz
  zq2 = m + stepz
  precision = 5
  options(scipen = 100)
  
  SE = round(SE, digits=precision)
  m = round(m, digits=precision)
  stept = round(stept, digits=precision)
  stepz = round(stepz, digits=precision)
  tcritical = round(tcritical, digits=precision)
  zcritical = round(zcritical, digits=precision)
  tq1 = round(tq1, digits=precision)
  tq2 = round(tq2, digits=precision)
  zq1 = round(zq1, digits=precision)
  zq2 = round(zq2, digits=precision)
  CL = round(CL * 100,digits = 2)
  # percent = round(percent * 100,digits = 2)
  
  message("One Sample T test (or Z test if applyable) for Simple Random Sample (SRS) with:")
  message("Sample Size      : ",n)
  message("Sample mean      : ",m)
  message("Sample SD        : ",S," (or Population SD if Z Distribution applies)") 
  message("Population Size  : ",N)
  message("Confidence Level : ",CL,"%")
  message("We get:")
  message("Degrees of Freedom : ",df)
  message("Standard Error     : ",SE)
  message("t critical         : ",tcritical)
  message("z critical         : ",zcritical)
  message(CL,"% Confidence Interval for Population mean: [ ",tq1," , ",tq2," ] = ",m," +- ",stept," (Using T distribution)")
  message(CL,"% Confidence Interval for Population mean: [ ",zq1," , ",zq2," ] = ",m," +- ",stepz," (Using Z distribution)")
  message("Normality Criteria of the Sampling Distribution:")
  message("n > 30 : ", n>30)
  message("Sampling size n less then 10% of the Population size (Independence Criteria) : ",n/N < 0.1)
  message("( For sampling whitout replacement )")

  if ( !missing(t) ) {
    message("")
    message("Significance Test for value ",t,":")
    tstatistic = (m - t)/SE
    zstatistic = tstatistic
    message("t-statistic : ",round(tstatistic,digits=precision ))
    message("z-statistic : ",round(zstatistic,digits=precision ))
    if ( tstatistic >= 0 ) {
      pvaluet = 1 - pt(tstatistic,df)
    } else {
      pvaluet = pt(tstatistic,df)
    }
    if ( zstatistic >= 0 ) {
      pvaluez = 1 - pnorm(zstatistic)
    } else {
      pvaluez = pnorm(zstatistic)
    }
    message("1 sided p-value for T Statistic : ",round(pvaluet,digits=precision ) )
    message("2 sided p-value for T Statistic : ",round(2 * pvaluet,digits=precision ) )
    message("1 sided p-value for Z Statistic : ",round(pvaluez,digits=precision ) )
    message("2 sided p-value for Z Statistic : ",round(2 * pvaluez,digits=precision ) )
    message(" ")
    message("( if p-value < Significance Level, then reject the Null Hypothesis )")
    message("False Positive: Type I error when you erroneously reject the Null Hypothesis")
    message("False Negative: Type II error when you erroneously fail to reject the Null Hypothesis")
    message("Statistical Power is the probability that a test will correctly reject a false null hypothesis")
    message("Increasing sample size or significance level, will increase the power")
    message("For Z statistic:")
    StatisticalPower = pnorm(zq1,t,SE) + 1 - pnorm(zq2,t,SE)
    message("Statistical Power            : ",round(StatisticalPower,digits = precision))
    message("Probability of Type I Error  : ",round(1 - CL/100,digits = precision)," ( Significance Level )")
    message("Probability of Type II Error : ",round(1 - StatisticalPower,digits = precision))
    message("For T statistic:")
    StatisticalPower = pnorm(tq1,t,SE) + 1 - pnorm(tq2,t,SE)
    message("Statistical Power            : ",round(StatisticalPower,digits = precision))
    message("Probability of Type I Error  : ",round(1 - CL/100,digits = precision)," ( Significance Level )")
    message("Probability of Type II Error : ",round(1 - StatisticalPower,digits = precision))
  }
} # End Function




  


