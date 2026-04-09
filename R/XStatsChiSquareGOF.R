#' Chi-Square goodness-of-fit Test
#'
#' @param observed Observed Vector
#' @param expected Expected Vector (optional defaults to equally probable)
#'
#' @description 
#' The "goodness-of-fit test" is a way of determining whether a set of categorical data came from 
#' a claimed discrete distribution or not. 
#' The null hypothesis is that they did and the alternate hypothesis is that they didn't. 
#' It answers the question: 
#' are the frequencies I observe for my categorical variable consistent with my theory? 
#' The goodness-of-fit test expands the one-proportion z-test. 
#' The one-proportion z-test is used if the outcome has only two categories. 
#' The goodness-of-fit test is used if you have two or more categories.
#'
#' @export
#'
#' @examples
#' XStatsChiSquareGOF(c(20,20,25,35))
XStatsChiSquareGOF = function(observed,expected) {
  # expected = c(25,25,25,25)
  # observed = c(20,20,25,35)
  if ( missing(expected) ) {
    S = sum(observed)
    L = length(observed)
    expected = rep(S/L,L)
  }
  
  chis = ( observed - expected )^2 / expected 
  chi2 = sum( chis )
  df = length(expected) - 1
  
  message("Chi-Square goodness-of-fit Test :")
  message("for observed : ",paste(observed,collapse=" , ")," Sum = ",sum(observed))
  message("    expected : ",paste(expected,collapse=" , ")," Sum = ",sum(expected))
  message("Chi-Squares  : ",paste(round(chis,digits = 4),collapse=" , "))
  message("df           : ",df)
  message("Chi-Square   : ",round(chi2,digits = 4))
  pvalue = 1 - pchisq(chi2,df)
  message("p-value      : ",round(pvalue,digits = 4))
  message("Large counts condition for a chi-square goodness-of-fit test:")
  message("all expected counts need to be at least 5 : ",sum(expected < 5) == 0)
  
  probability = expected/sum(expected)
  chisq.test(observed,p=probability)
  
} # End Function







