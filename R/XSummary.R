
#' @export

# Levels of Measurement
# Nominal Scale
# Ordinal Scale
# Interval Scale
# Ratio Scale

# Variation Ratio for Nominal Scale Variables
# VR = 1 #Mode/N

# Population measures are called parameters
# Sample measures are called statistics

XSummary = function(d) {
  #d = c(4.8,4.8,5.9,NA,8.9,8.9)
  N = length(d)
  Nunique = length(unique(d))
  m = sum( abs(d - mean(d,na.rm=T)),na.rm=T ) / N # mean deviation
  
  Var.sample = var(d,na.rm=T)      # Variance for sample estimate - divided by N-1
  SD.sample = sd(d,na.rm=T)        # Standard Deviation for Sample estimate - divided by N-1
  Var.population = Var.sample*(N-1)/N
  SD.population = sqrt(Var.population)
  SE.mean = sqrt(Var.sample/N)
  
  # Compute: Min, 1st Quartile, Median(2nd Quartile), Mean, 3rd Quartile, Max
  ds = summary(d)
  
  x = c("N#"=N,"Unique Values"=Nunique,
        ds,
        IQR=unname(ds[5]-ds[2]),
        Range=max(d,na.rm=T)-min(d,na.rm=T),
        "Mean deviation"=m,
        SD.sample=SD.sample,    # if d is sample, then we use this estimate for the population
        SD.population=SD.population,  # if d is population, then we use this population value 
        Var.sample=Var.sample,
        Var.population=Var.population,
        SE.mean=SE.mean,
        median.average.deviation=mad(d))
  
  # Only include the mode if Unimodal
  temp = table(d)
  statsmode = as.numeric(names(temp)[temp == max(temp)])
  if (length(statsmode)==1) {
    n = max(temp)
    x = c(x,Mode=statsmode,"Variation Ratio"=1-n/N)
  } else {
    x = c(x,Multimodal=length(statsmode))
  }
  
  x = cbind(Measurements=x)
  print(x)
}

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






