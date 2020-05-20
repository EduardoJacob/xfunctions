
#' @export


# Plot Histogram with superimposed best-fitting normal distribution
XHistogram = function(distribution,probability=FALSE,x1=mean-sd,x2=mean+sd,name="A",color="green") {
  if ( missing(name) ) {
    name = deparse(substitute(distribution))
  }
  print(summary(distribution))
  NAs = sum(is.na(distribution))
  mean = mean(distribution,na.rm=T)
  sd = sd(distribution,na.rm=T)
  num = length(distribution)
  subtitle = paste("Num elements:",num,", mean:",round(mean,3),", sd:",round(sd,3),"NA's:",NAs)
  h = hist(distribution,plot=FALSE)
  
  # For Probability Histogram:
  if (probability == TRUE) {
    m1 = max(h$density)
    multiplier = 1
  } else {
    # For Frequency Histogram:
    m1 = max(h$counts)
    multiplier = diff(h$mids[1:2])*length(distribution) 
    #print(multiplier)
    #multiplier2 = max(h$counts)/max(h$density)
    #print(multiplier2)
  }
  
  m2 = multiplier * dnorm(mean,mean,sd)
  ymax = 1.1 * max(m1,m2)
  
  par(mfrow = c(2,1))
  # distribution = iris$Sepal.Width
  # name="teste"
  # color = "green"
  b = boxplot(distribution,horizontal=T,plot=FALSE)
  stats = paste(round(b$stats,digits=5),collapse="   ")
   
  xlabel = paste("Boxplot stats:",stats,"; Num outliers:",length(b$out))
  boxplot(distribution,col=color,horizontal=T,main=name,xlab=xlabel)
  grid()
  boxplot(distribution,col=color,horizontal=T,main=name,xlab=xlabel,add = T)
  # boxplot(distribution,horizontal=T)
  # grid(nx=NULL, ny=NULL) #grid over boxplot
  # par(new=TRUE)
  # boxplot(distribution,col=color,horizontal=T,main=name,xlab=xlabel) #grid behind boxplot
  # print(xlabel)
  
  xmin = min(c(b$stats,b$out))
  xmax = max(c(b$stats,b$out))
  
  hist(distribution,xlab=subtitle,ylim=c(0,ymax),prob=probability,panel.first=grid(),las=1,
       col=color,xlim=c(xmin,xmax),main="")
  rug(distribution)
  box()
  # Superimpose the Normal Curve
  # abline(v=c(mean+sd,mean-sd))
  xmin = mean - 5*sd
  xmax = mean + 5*sd
  plot(function(x) multiplier * dnorm(x,mean,sd),xmin,xmax,
       add=TRUE,col="red",lwd=2)
  
  # Superimpose the density plot
  # lines() or points() will add to the existing graph, but will not create a new window.
  d = density(distribution,na.rm = T)
  lines(d$x,multiplier*d$y,col="blue",lwd=2)
  
  
  # Superimpose points
  xv = sapply(-3:3,function(x) mean + x*sd)
  yv = sapply(xv,function(x) multiplier * dnorm(x,mean,sd))
  points(xv,yv,pch=19, col="red")
  
  print("Aproximation values from the normal curve:")
  for (x in c(x1,x2)) {
    abline(v=x,col="red")
    z = round((x - mean) / sd,4)
    A1 = round(pnorm(x, mean, sd),4)
    A2 = round(1 - pnorm(x, mean, sd),4)
    x = round(x,4)
    print(paste("x =",x))
    print(paste("x in Standard Units is",z,"(z-score)"))
    print(paste("Area to the left of x is",A1))
    print(paste("Area to the right of x is",A2))
  }

  par(mfrow = c(1,1))

  A2 = round(pnorm(x2, mean, sd) - pnorm(x1, mean, sd),4)
  A1 = round(A2 * length(distribution),4)
  x1 = round(x1,4)
  x2 = round(x2,4)
  print(paste("Area between [",x1,",",x2,"] is proportion:",A2,", absolute:",A1))
  
  print(" ")
  print("Exact values from the histogram:")
  A1 = round(sum(distribution > x1 & distribution <= x2),4)
  A2 = round(A1/length(distribution),4)
  print(paste("Area between [",x1,",",x2,"] is proportion:",A2,", absolute:",A1))
}



