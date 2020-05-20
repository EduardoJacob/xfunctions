
#' @export

# Slope of best-fit line: b = r . ( SDy / SDx )
# Intercept of best-fit = Ymean - b. Xmean

XScatterPlot2 = function(xMean,xSD,yMean,ySD,r,x) {
  b = r * ySD / xSD
  a = yMean - b * xMean
  predictedValue = a + b * x
  cat("Slope of best-fit line is    :",b,"\n")
  cat("Intercept of best-fit line is:",a,"\n")
  cat("Predicted Value is           :",predictedValue,"\n")
  

}

# The scores of midterm and final exams for a random sample of Stats 10 students can be summarized as follows:
# Mean of midterm score = 36.92; 
# SD of midterm score = 37.79; 
# Mean of final score = 24.71; 
# SD of final score= 25.21; 
# r= 0.978
# Predict the final score for a student that got a midterm score of 35.

# XScatterPlot2(36.92,37.79,24.71,25.21,0.978,35)

