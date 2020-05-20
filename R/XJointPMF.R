
#' @export

# Joint PMF between 2 Random Variables X,Y with probability matrix p
XJointPMF = function(X,Y,p) {
  options(digits=4)
  cat("X=",X,"\n")
  cat("Y=",Y,"\n")
  cat("p=","\n")
  print(p)
  cat("Sum(p)=",sum(p),"\n")
  
  probability_vector = colSums(p)
  dX = XDescribeRandomVariable("X",X,probability_vector)
   
  probability_vector = rowSums(p)
  dY = XDescribeRandomVariable("Y",Y,probability_vector)
  
  # The Variance Rule is only valid for X and Y independent Random Variables
  cat("E(X + Y) =",dX$E+dY$E,"var(X + Y) =",dX$var+dY$var,"\n")
  cat("E(X - Y) =",dX$E-dY$E,"var(X - Y) =",dX$var+dY$var,"\n")
  
  cat("\n")
  cat("Marginal Probabilities:\n")
  
  newp = prop.table(p,1)
  i = 0
  v = vector()
  for (y in Y) {
    i = i + 1
    cat("(X|Y=",y,") ")
    probability_vector = newp[i,]
    d = XDescribeRandomVariable("X",X,probability_vector)
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
    cat("(Y|X=",x,") ")
    probability_vector = newp[,i]
    d = XDescribeRandomVariable("Y",Y,probability_vector)
    v = c(v,d$E)
  }
  if (length(unique(v)) == 1) {
    cat("X and Y are Independent!\n")
  } else {
    cat("X and Y are Dependent!\n")
  }
  
  cat("\n")
#   cat("Combined Random Variables:\n")
#   
#   cat("Example 1: min(X,Y)\n")
#   example = p
#   for (x in 1:ncol(p)) {
#     for (y in 1:nrow(p)) {
#       example[y,x] = 0 
#       if (p[y,x] > 0) example[y,x] = min(X[x],Y[y])
#     }
#   } # end example 1
#   print(example)
#   
#   
#   cat("Example 2: (YX^2)\n")
#   example = p
#   for (x in 1:ncol(p)) {
#     for (y in 1:nrow(p)) {
#       example[y,x] = 0 
#       if (p[y,x] > 0) example[y,x] = X[x]*Y[y]*X[x]
#     }
#   } # end example 2
#   print(example)
  
} # end function

# X = c(0,1,2)
# Y = c(3,4)
# p = rbind(c(0.1,0.2,0.2),c(0.1,0.2,0.2))
# XJointPMF(X,Y,p)
# 
# p = rbind(c(0.1,0.2,0.2),c(0.2,0.2,0.1))
# XJointPMF(X,Y,p)




















