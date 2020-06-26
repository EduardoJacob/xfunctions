#' Enhanced Summary of a Vector
#'
#' @param d A Numeric Vector
#'
#' @return
#' @export
#'
#' @examples
#' XSummary(iris$Sepal.Length)
XSummary = function(d) {
  # d = c(4.8,4.8,5.9,NA,8.9,8.9,NA)
  # d = iris$Sepal.Length
  dataname = deparse(substitute(d))
  message("XSummary for ",dataname,":")
  message("Count                           : ",length(d))
  message("Count NA                        : ",sum(is.na(d)))
  d = d[complete.cases(d)]
  N = length(d)
  m = mean(d)
  s = sd(d)
  v = var(d)
  message("Sample Size                     : ",N)
  message("Unique Values                   : ",length(unique(d)))
  message("Mean                            : ",NUM(m))
  message("Mean Deviation                  : ",NUM( sum( abs(d - m) ) / N) )
  error = qt(0.975,df=N-1)*sd(d)/sqrt(N)
  message("Confidence Interval of the Mean : [ ",NUM(m - error)," , ",NUM(m + error)," ]")
  message("Min                             : ",min(d))
  message("1st Quartile                    : ",unname(quantile(d)[2]))
  message("2nd Quartile (Median)           : ",median(d))
  message("3rd Quartile                    : ",unname(quantile(d)[4]))
  message("Max                             : ",max(d))
  message("IQR,Inter Quartile Range        : ",unname( quantile(d)[4] - quantile(d)[2] ))
  message("Range                           : ",max(d) - min(d))
  message("Sample Variance                 : ",NUM(v))
  message("Sample SD                       : ",NUM(s))
  PV = v * (N-1) / N
  message("Population Variance             : ",NUM(PV))
  message("Population SD                   : ",NUM(sqrt(PV)))
  message("SE.mean                         : ",NUM(sqrt(v/N))) # Standard Error of the Mean
  message("Coefficient Of Variation        : ",NUM(100 * s/m)," %")
  message("Median Average Deviation        : ",NUM(mad(d)))
  message("Skewness of the sample          : ",NUM(moments::skewness(d)))
  message("Pearson's measure of Kurtosis   : ",NUM(moments::kurtosis(d)))
  
  # Only include the mode if Unimodal
  temp = table(d)
  statsmode = as.numeric(names(temp)[temp == max(temp)])
  if (length(statsmode)==1) {
    n = max(temp)
    message("Mode                            : ",statsmode," occurs ",n," times")
    message("Mode Variation Ratio            : ",NUM(1-n/N))
  } else {
    message("Multimodal                      : ",length(statsmode)," modes")
  }

  invisible()
} # XSummary

NUM = function(n) {
  # return(formatC(signif(n,digits=4), digits=4,format="fg", flag="#"))
  return(format(n,digits=5))
}

# Levels of Measurement
# Nominal Scale
# Ordinal Scale
# Interval Scale
# Ratio Scale

# Variation Ratio for Nominal Scale Variables
# VR = 1 #Mode/N

# Population measures are called parameters
# Sample measures are called statistics

# d = c(-3,5,4,10,8,8,NA)
# d = c(80,46,83,75,83,90,90,72,77,4,83,125,63,87,73,84,0,70,65,96,89,78,99,104,83,81)
# summary(d)
# summary(scale(d)[,1])

# XSummary(d)
# XSummary(scale(d)[,1])
# XSummary(c(-1, 0, 0, 1))

# The boxplot give us the The Five Number Summary: min, q1, median, q3, max
# boxplot(d,col="lightgray",horizontal=T,main="Box Plot of Distribution",xlab="Inches")
# boxplot(d,col="lightgray",horizontal=F,main="Box Plot of Distribution",ylab="Inches")
# Compute generic percentiles:
# quantile(d, seq(0,1,0.25)) 

# Quantile Plot:
# n = length(d)
# plot((1:n - 1)/(n - 1), sort(d), type="l",
#      main = "Quantiles for Distribution d",
#      xlab = "Sample Fraction",
#      ylab = "Sample Quantile")
# 






