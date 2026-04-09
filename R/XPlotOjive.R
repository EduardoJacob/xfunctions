#' Plots The Cumulative Frequency Graph ( Ojive )
#'
#' @param d Distribution Vector
#'
#' @export
#'
#' @examples
#' XPlotOjive((iris$Sepal.Length))
XPlotOjive = function(d) {
  breaks = seq(min(d),max(d),length.out=100) 
  cut = cut(d, breaks, right=FALSE) 
  freq = table(cut)
  
  cumfreq0 = c(0, cumsum(freq)) 
  plot(breaks, cumfreq0,           
       main="Cumulative Frequency Graph ( Ojive )",  
       xlab="Values",        
       ylab="Cumulative")   
  
  lines(breaks, cumfreq0)         
  
}















