#' Plots The Binomial Distribution B(n,p)
#'
#' @param n Integer parameter n
#' @param p Probability p between 0 and 1
#'
#' @export
#'
#' @examples
#' XPlotBinomial(10,0.3)
XPlotBinomial = function(n,p) {
  # n = 100
  # p = 0.99
  mean = n*p
  var = n*p*(1-p)
  sd = sqrt(var)
  n1 = qbinom(0.001,n,p)
  n2 = qbinom(0.999,n,p) 
  d = dbinom(n1:n2,n,p)
  
  fig = plotly::plot_ly(
    x = n1:n2,
    y = d,
    name = "Binomial" ,
    type = "bar"
  )
  title = paste("Binomial Distribution for n =",n,
                ", p =",round(p,digits = 4),
                ", mean =",round(mean,digits = 4),
                ", var =",round(var,digits = 4),
                ", sd =",round(sd,digits = 4),sep=" ")
  message(title)
  t = list(family = "sans serif",size = 10, color = 'blue')
  
  fig = fig %>% layout(title = title, font = t)
  x = seq(n1,n2,length.out = 100)
  fig = fig %>% add_lines(x = x, y = dnorm(x,mean=mean,sd=sd), name = "Normal")
  fig
}















