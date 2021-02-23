#' Scatter Plot for 2 continuous variables 
#'
#' @param x x data
#' @param y y data
#'
#' @return
#' @export
#'
#' @examples
#' XScatterPlot(mtcars$wt, mtcars$mpg)

# Scatter diagrams
# Bivariate data studies the correlation between 2 variables 
# Positive association: One variable grows when the other grows
# Correlation Coefficient r: -1 to 1 Measures how linear is the association ONLY TO LINEAR ASSOCIATIONS !
# Association is not causation. example: Shoe's size and scholarly level 
# Correlated = Linear Related
# Residual = Observed y - Predicted y
# Outliers are points that misbehave from the rest 
# Clustering is the contrary to fuziness
# Regression is to make a estimate of a variable on a Correlated distribution with known r
# SD is the r.m.s. root mean sqare of the deviations from average
# Chebychev Law: For any distribution:
# At most 1/K^2 of the entries can be outside of +-K*SD
# For football shape distributions, you can use normal aproximation for both variables: Bivariate Normal
# Regression effect for Bivariate data: When you start high on one variable, you will estimate low for the other
# and vice-versa 
# For Multiple Linear regression, use: model = lm(y~x1+x2)
# Slope of best-fit line: b = r . ( SDy / SDx )
# Intercept of best-fit = Ymean - b. Xmean

XScatterPlot = function(x,y) {

  # Get the Correlation Coefficient (-1:1)
  # Is the slope of the best-fit line when the variables are converted to Standard Units
  # Pearson Correlation:
  # mean(scale(x)*scale(y))
  # Normal Correlation:
  # sum(scale(x)*scale(y)) / (length(x) - 1)
  # x = mtcars$wt
  # y = mtcars$mpg
  par(mfrow=c(2,2))
  round_precision = 3
  xlabel = deparse(substitute(x))
  
  model = lm(y~x)
  intersect = round(model$coefficients[1],round_precision)
  slope = round(model$coefficients[2],round_precision)
  equation = paste("y =",slope,"x +",intersect)
  # Sum of Square Errors
  SSE = round(sum(model$residuals ^ 2),round_precision)
  # Root Mean Square Error
  RMSE = round(sqrt( SSE / length(x) ),round_precision)
  # Baseline model assumes horizontal line
  ymean = mean(y)
  # Sum of Square Errors for the Baseline Model (Total Sum of Squares)
  SST = round(sum( (y - ymean) ^ 2),round_precision)
  # Coefficient of Determination R2
  R2 = summary(model)$r.squared
  
  for (method in c("pearson","spearman","kendall")) {
    
    r1 = round(cor(x,y,method=method),round_precision)
    # Coefficient of determination
    r2 = round(r1^2,round_precision)
    r3 = round(cov(x,y,method=method),round_precision)
    
    # Simple Scatterplot
    plot(x,y,pch=19,main=paste(method,"correlation"),las=1,ylab=equation,xlab=xlabel)
    # df = data.frame(x,y)
    # qplot(x,y,data=df)
    mtext(paste(length(x),"Points, cor:",r1,"cod:",r2,"cov:",r3))
    
    # Add fit lines
    abline(lm(y~x), col="red")
    # Point of averages
    points(mean(x),mean(y),col="red",pch = 19,cex = 1) 
    
    t = cor.test(x,y,method=method,conf.level=0.95,exact=F)
    print(t)
  }
  
  # There'a a T distribution associated with the Regression Line which says if the slope of the 
  # line is significative 
  df = model$df.residual
  ConfidenceLevel = 0.95
  tstatistic = (coef(model) / sqrt(diag(vcov(model))))[2] # T-statistic
  tstatistic = round(tstatistic,round_precision)
  xmin = -5
  xmax = 5
  xseq = seq(xmin,xmax,length.out=100)
  densities = dt(xseq,df)
  #Tcritical1 = round(qt(ConfidenceLevel,df),4)
  Tcritical2 = round(qt((1+ConfidenceLevel)/2,df),round_precision)
  # Plot 
  maintitle = paste("T distribution",df,"df, t-statistic:",tstatistic)
  #subtitle = paste("1tailTcrit",Tcritical1,", 2tailTcrit",Tcritical2,"for Confidence Level",ConfidenceLevel)
  subtitle = paste("2tailTcrit",Tcritical2,"for Confidence Level",ConfidenceLevel)
  plot(xseq,densities,col="red",xlab=subtitle,ylab="Density",type="l",lwd=2,cex=2, 
       main=maintitle,panel.first=grid(),las=1)
  
  # Paint an area
  z1 = tstatistic
  abline(v=z1,col="blue")
  cord.x = c(xmin,seq(xmin,z1,length.out=100),z1) 
  cord.y = c(0,dt(seq(xmin,z1,length.out=100),df),0) 
  polygon(cord.x,cord.y,col='skyblue') 
  p = round(pt(z1,df),4)
  text(z1,dt(z1,df)/2,paste("Area:",p,"to the left of",z1))
  abline(v=c(-Tcritical2,Tcritical2),col="red")
  
  par(mfrow=c(1,1))
    
  #print(model)
  print(summary(model))
  
  cat("The residuals should be normally distributed with a mean of zero\n")
  cat("Sum of Square Errors SSE       :",SSE,"\n")
  cat("Root Mean Square Error RMSE    :",RMSE,"\n")
  cat("Total Sum of Squares SST       :",SST,"(Sum of Square Errors for the Baseline Model)\n")
  cat("Coefficient of Determination R2:",R2,"(It's the Correlation Squared)\n")

}

# XScatterPlot(mtcars$wt, mtcars$mpg)
# 
# model =lm(mtcars$mpg~mtcars$wt)
# par(mfrow=c(2,2))
# plot(model)
# summary(model)
# summary(model)$sigma^2

# str(mtcars)
# qplot(mtcars$wt, mtcars$mpg)
# qplot(wt, mpg, data=mtcars)
# This is equivalent to:
# ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

# X = c(22,29,19,24,25,30,35,23,27,29,20,21,21,24,28)
# Y = c(88,95,81,86,89,90,100,71,77,91,85,90,84,60,91)
# XScatterPlot(X,Y)
# 
# X = seq(0,6,length=100 )
# Y = 2*X + 2*rnorm(100)
# XScatterPlot(X,Y)


