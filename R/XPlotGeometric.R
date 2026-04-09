#' Plots The Geometric Distribution G(p)
#'
#' @param p Probability p between 0 and 1
#'
#' @export
#'
#' @examples
#' XPlotGeometric(0.3)
XPlotGeometric = function(p) {
  n = qgeom(0.999,p)
  d = dgeom(0:n,p)
  # sum(d)
  mean = 1/p
  sd = round( sqrt(1-p)/p , digits=4 )
  
  fig = plotly::plot_ly(
    x = seq(1,n+1),
    y = d,
    name = "Geometric",
    type = "bar"
  )
  title = paste("Geometric Distribution for p =",p,", mean =",mean,", sd =",sd,sep=" ")
  message(title)
  t = list(family = "sans serif",size = 10, color = 'blue')
  
  fig = fig %>% layout(title = title, font = t)
  # x = seq(0,xmax,length.out = 100)
  # fig = fig %>% add_lines(x = x, y = dnorm(x,mean=mean,sd=sd), name = "Normal")
  fig
}

















