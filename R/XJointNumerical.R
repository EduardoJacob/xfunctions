#' Joint Distribution between 2 Numerical Random Variables X,Y with probability table p
#' Also Analyse the Portfolio Combination of a*X + b*Y
#'
#' @param X Random Variable X
#' @param Y Random Variable Y
#' @param p Totals Table or Probability Table
#' @param a Portfolio Combination of a*X + b*Y
#' @param b Portfolio Combination of a*X + b*Y
#'
#' @export
#'
XJointNumerical = function(X,Y,p,a=1,b=1) {
  options(digits=5)
  nameX = deparse(substitute(X))
  nameY = deparse(substitute(Y))
  cat(nameX,"= (",paste(X,collapse = " , "),")\n")
  cat(nameY,"= (",paste(Y,collapse = " , "),")\n")
  
  colnames(p) = paste0(nameX,':',X)
  rownames(p) = paste0(nameY,':',Y)
  
  cat("\n")
  cat("Totals Table:","\n")
  print(addmargins(p,1:2))
  
  p = prop.table(p)
  cat("\n")
  cat("Probability Table:","\n")
  print(addmargins(p,1:2))
  
  cat("\n")
  
  probability_vector = colSums(p)
  dX = XDescribeRandomVariable(X,probability_vector)
   
  probability_vector = rowSums(p)
  dY = XDescribeRandomVariable(Y,probability_vector)
  
  # Covariance(X,Y)
  xy = matrix(0, nrow = length(Y), ncol = length(X))
  for (column in 1:length(X)) {
    for (row in 1:length(Y)) xy[row,column] = X[column] * Y[row]
  }
  covariance = sum(xy*p) - (dX$E * dY$E)
  correlation = covariance / ( dX$sd * dY$sd )
  cat(paste0("Covariance(",nameX,",",nameY,") = "),covariance,"\n")
  cat(paste0("Correlation(",nameX,",",nameY,") = "),correlation,"\n")
  
  # Portfolio Analysis
  portfolio = paste0(a,nameX," + ",b,nameY)
  cat("Portfolio Analysis for",portfolio,":",
      "E(",portfolio,") =",a*dX$E + b*dY$E,
      "var(",portfolio,") =",a*a*dX$var + b*b*dY$var + 2*a*b*covariance,"\n")
  
  cat("\n")
  cat("Conditional Probabilities:\n")
  
  newp = prop.table(p,1)
  i = 0
  v = vector()
  for (y in Y) {
    i = i + 1
    cat("(",nameX,"|",nameY,"=",y,") ")
    probability_vector = newp[i,]
    d = XDescribeRandomVariable(X,probability_vector)
    v = c(v,d$E)
  }
  if (length(unique(v)) == 1) {
    cat("X and Y are Independent!\n")
  } else {
    cat("X and Y are Dependent!\n")
  }
  
  cat("\n")
  newp = prop.table(p,2)
  i = 0
  v = vector()
  for (x in X) {
    i = i + 1
    cat("(",nameY,"|",nameX,"=",x,") ")
    probability_vector = newp[,i]
    d = XDescribeRandomVariable(Y,probability_vector)
    v = c(v,d$E)
  }
  if (length(unique(v)) == 1) {
    cat("X and Y are Independent!\n")
  } else {
    cat("X and Y are Dependent!\n")
  }
  
  cat("\n")

} # end function











