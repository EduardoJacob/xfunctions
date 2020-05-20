
#' @export

# Plot Histogram for a Vector of Integers
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
  
}

# v = c(-3,5,5,4,10,8,8,NA)
# XHistogramDiscrete(v)
# barplot(v)
