#' Plot Histogram for a Vector of Integers
#' 
#' It plots an Histogram with one bar for each distinct value
#' Try XHistogramDiscrete(mtcars$cyl) and compare it with hist(mtcars$cyl)
#'
#' @param v A vector of Values
#'
#' @return
#' @export
#'
#' @examples
#' XHistogramDiscrete(c(-3,5,5,4,10,8,8,NA))
#' XHistogramDiscrete(mtcars$cyl) 
XHistogramDiscrete = function(v) {
  dataname = deparse(substitute(v))
  
  print(summary(v))
  NAs = sum(is.na(v))
  mean = mean(v,na.rm=T)
  sd = sd(v,na.rm=T)
  num = length(v)
  subtitle = paste("Num observations:",num,", mean:",round(mean,3),", sd:",round(sd,3),"NA's:",NAs)
  maintitle = paste("Histogram of",dataname)
  
  a = min(v,na.rm=T)
  b = max(v,na.rm=T)
  foo = hist(v+0.001,breaks=b-a,xaxt="n",col="orange",panel.first=grid(),main=maintitle,xlab=subtitle)
  axis(side=1,at=foo$mids,labels=seq(a,b))
  
  invisible()
} # XHistogramDiscrete



