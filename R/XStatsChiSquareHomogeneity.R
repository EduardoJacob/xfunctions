#' Chi-Square Test for Homogeneity or for Association ( Independence )
#' 
#' @description 
#' Chi-Square Test for Homogeneity samples from 2 different groups
#' Chi-Square Test for Association samples from 1 group comparing 2 variables
#' 
#' The "test of homogeneity" is a way of determining whether two or more sub-groups of a population 
#' share the same distribution of a single categorical variable. 
#' For example, do people of different races have the same proportion of smokers to non-smokers, 
#' or do different education levels have different proportions of Democrats, Republicans, 
#' and Independent. 
#' The test of homogeneity expands on the two-proportion z-test. 
#' The two proportion z-test is used when the response variable has only two categories as outcomes 
#' and we are comparing two groups. 
#' The homogeneity test is used if the response variable has several outcome categories, 
#' and we wish to compare two or more groups.
#' 
#' @param observed Observed Matrix n x m
#'
#' @export
#'
#' @examples
#' XStatsChiSquareHomogeneity(matrix(c(30,10,15,25,15,5), ncol = 2, nrow = 3, byrow=TRUE))
XStatsChiSquareHomogeneity = function(observed) {
  # observed = matrix(c(30,10,15,25,15,5), ncol = 2, nrow = 3, byrow=T)
  n = ncol(observed)
  m = nrow(observed)
  observed = as.table(observed)
  observed = addmargins(observed,1:2)
  
  message("Chi-Square Test for Homogeneity (or for Association)")
  message("Observed Matrix:")
  print(observed)
  
  total = observed[m+1,n+1]
  expected = observed
  chi2 = 0
  for ( i in 1:n ) {
    for ( j in 1:m ) {
      expected[j,i] = observed[j,n+1] / total * observed[m+1,i]
      chi2 = chi2 + ( observed[j,i] - expected[j,i] )^2 / expected[j,i] 
    }
  }
  
  message("Expected Matrix:")
  print(expected)
  
  df = (n-1)*(m-1)
  
  message("Chi Statistic : ",round(chi2,digits = 4))
  message("df            : ",df)
  pvalue = 1 - pchisq(chi2,df)
  message("p-value       : ",round(pvalue,digits = 4))
  message("Large counts condition for a chi-square test:")
  message("all expected counts need to be at least 5   : ",sum(expected < 5) == 0)
  
  chisq.test(observed)
  
} # End Function







