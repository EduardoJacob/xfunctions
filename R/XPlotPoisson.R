#' Plots The Poisson Distribution Poisson(lambda)
#'
#' @param L Number of events per unit time
#'
#' @export
#'
#' @examples
#' XPlotPoisson(0.3)
XPlotPoisson = function(lambda) {
  n = qpois(0.999,lambda)
  d = dpois(0:n,lambda)
  # sum(d)
  mean = lambda
  var = lambda
  sd = round( sqrt(lambda) , digits=4 )
  
  fig = plotly::plot_ly(
    x = 0:n,
    y = d,
    name = "Poisson",
    type = "bar"
  )
  title = paste("Poisson Distribution for lambda =",round(lambda,digits = 4),
                ", mean =",round(mean,digits = 4),
                ", var =",round(var,digits = 4),
                ", sd =",round(sd,digits = 4),sep=" ")
  message(title)
  t = list(family = "sans serif",size = 10, color = 'blue')
  
  fig = fig %>% layout(title = title, font = t)
  x = seq(0,n,length.out = 100)
  fig = fig %>% add_lines(x = x, y = dnorm(x,mean=mean,sd=sd), name = "Normal")
  fig
}

















