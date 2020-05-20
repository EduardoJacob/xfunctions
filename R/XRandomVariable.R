
#' @export

# Discrete Random Variable
# Generic Distribution of n trials with replacement from a box
# The Analitycal Expression that gives the probability of X is the Density Function
# The Integral of the Density Function is the CDF

# PMF (Probability Mass Function) uses discrete random variables. 
# PDF (Probability Density Function) uses continuous random variables. 
# Based on studies, PDF is the derivative of CDF, which is the cumulative distribution function. 
# CDF is used to determine the probability wherein a continuous random variable would occur within any measurable subset of a certain range

# Sums and Differences of Random Variables: Effect on the Mean
# E(X + Y) = E(X) + E(Y) 

# E(X - Y) = E(X) - E(Y) 
# Sums and Differences of Independent Random Variables: Effect on Variance
# Var(X + Y) = Var(X - Y) = Var(X) + Var(Y) 

# Receives Random Variable Vector, Probability Vector and Number of Trials
XRandomVariable = function(X,p=rep(1/L,L),n=1) {
  # library("gtools") # Contains permutations function
  
  L = length(X)
  color = "orange"
  E = sum(p*X)
  var = sum(p*(X-E)^2)
  sd = sqrt(var)
  pre = 4 # precision
  cat("Random Variable X  :",X,"\n")
  cat("Num Elements       :",length(X),"\n")
  cat("Probability Vector :",p,"\n")
  cat("Sum(Probabilities) :",sum(p),"\n")
  cat("Expected Value     :",round(E,pre),"\n")
  cat("Variance           :",round(var,pre),"\n")
  cat("Standard Deviation :",round(sd,pre),"\n")
  
  par(mfrow=c(2,2))
  
  ymax = 1.1*max(p)
  plot(X,p,type="h",col=color,lwd=5,panel.first=grid(),
       main="Probability Mass Function",ylim=c(0,ymax),las=1,
       xlab="Outcome x",ylab="Probability p")
  abline(v=E,col="red")
  
  # Number of trials
  mean = n * E
  var = n * var
  sd = sqrt(var)
  cat("Number of Trials   :",n,"\n")
  cat("Total Expectation  :",round(mean,pre),"\n")
  cat("Total Variance     :",round(var,pre),"\n")
  cat("Total SD           :",round(sd,pre),"\n")
  
  # Generate all the Outcomes:
  N = L^n # Number of Outcomes
  outcomes = gtools::permutations(L,n,1:L,set=FALSE,repeats.allowed=TRUE)
  d1 = vector()
  p1 = vector()
  d2 = vector()
  p2 = vector()
  for (i in 1:N) {
    d1 = c(d1,sum(X[outcomes[i,]]))  # Sum the Values
    p1 = c(p1,prod(p[outcomes[i,]]))  # Correspondent Probability
    
    d2 = c(d2,prod(X[outcomes[i,]]))  # Product of the Values
    p2 = c(p2,prod(p[outcomes[i,]]))  # Correspondent Probability
    
  }
  #summary(d1)
  #summary(d2)
  
  x = vector()
  y = vector()
  for (i in min(d1):max(d1)) {
    o = sum(d1==i)
    if (o > 0 ) {
      pi = sum(p1[d1==i])
      x = c(x,i)
      y = c(y,pi)
      cat("Probability of Sum",i,"=",pi,"\n")
    }
  }
  #sum(y) # Must be 1
  

  ymax = 1.1*max(y)
  plot(x,y,type="h",col=color,lwd=5,panel.first=grid(),main=paste("Sum of X for",n,"trials"),xlab="Sum",ylab="Density",las=1,ylim=c(0,ymax))
  
  plot(ecdf(d1),col=color,lwd=5,panel.first=grid(),las=1,cex=1.2,main="Empirical Cumulative Density Function")
  
  for (i in min(d2):max(d2)) {
    o = sum(d2==i)
    if (o > 0 ) cat("Probability of Product",i,"=",o,"/",N,"\n")
  }
  plot(table(d2),type="h",col=color,lwd=5,panel.first=grid(),main=paste("Product of X for",n,"trials"),las=1)
  
  par(mfrow=c(1,1))
  
} # XRandomVariable










