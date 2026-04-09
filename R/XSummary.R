#' Enhanced Summary of a Vector
#'
#' @param d A Numeric Vector
#'
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
  md = median(d)
  gm = exp(mean(log(d)))
  hm = 1/mean(1/d)
  s = sd(d)
  v = var(d)
  
  message("Sample Size                     : ",N)
  message("Unique Values                   : ",length(unique(d)))
  message("Sum                             : ",NUM(sum(d,na.rm = TRUE)))
  message("Arithmetic Mean                 : ",NUM(m))
  message("Geometric Mean                  : ",NUM(gm))
  message("Harmonic Mean                   : ",NUM(hm))
  message("MAD1 Mean Absolute Deviation    : ",NUM( sum( abs(d - m) ) / N) )
  message("Median Absolute Deviation (MAD) : ",NUM(mad(d)))
  error = qt(0.975,df=N-1)*sd(d)/sqrt(N)
  message("Confidence Interval of the Mean : [ ",NUM(m - error)," , ",NUM(m + error)," ]")

  # Quantiles
  message("Min                             : ",min(d))
  Q1 = unname(quantile(d)[2])
  Q3 = unname(quantile(d)[4])
  IQR = Q3 - Q1
  message("1st Quartile                    : ",Q1)
  message("2nd Quartile (Median)           : ",md)
  message("3rd Quartile                    : ",Q3)
  message("Inter Quartile Range (IQR)      : ",IQR)
  message("Max                             : ",max(d))
  message("Range                           : ",max(d) - min(d))
  
  # Outliers 
  Limit = Q1 - 1.5*IQR
  message("Inferior Limit ( Q1 - 1.5IQR )  : ", Limit)
  message("Inferior Outliers               : ", paste( d[d<Limit],collapse = " , "))
  Limit = Q3 + 1.5*IQR
  message("Superior Limit ( Q3 + 1.5IQR )  : ", Limit)
  message("Superior Outliers               : ", paste( d[d>Limit],collapse = " , "))
  
  SV = v * (N-1) / N
  SD = sqrt(SV)
  message("Sample Variance                 : ",NUM(SV))
  message("Sample SD                       : ",NUM(SD))
  message("Sample Coefficient Of Variation : ",NUM(100 * s/m)," %")
  
  message("Population Variance Estimation  : ",NUM(v))
  message("Population SD                   : ",NUM(s))
  message("Population Coeffi. Of Variation : ",NUM(100 * s/m)," %")
  
  message("SE.mean                         : ",NUM(sqrt(v/N))) # Standard Error of the Mean
  
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
  
  # Horizontal BoxPLot
  fig <- plotly::plot_ly(x = d, type = "box", name = "")
  return(fig)

  # invisible()
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






