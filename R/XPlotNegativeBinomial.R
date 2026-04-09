#' Plots The Negative Binomial Distribution NBinom(r,p)
#'
#' @param r Number of Expected Successes
#' @param p Bernoulli Probability p between 0 and 1
#'
#' @export
#'
#' @examples
#' XPlotNegativeBinomial(3,1/6)
XPlotNegativeBinomial = function(r,p) {
  # r = 3
  # p = 1/6
  q = 1 - p
  mean = r*q/p
  sd = sqrt(r*q/(p*p))
  n1 = qnbinom(0.001,r,p) 
  n2 = qnbinom(0.999,r,p) 
  d = dnbinom(n1:n2,r,p)
  
  fig = plotly::plot_ly(
    x = seq(from=n1+r,to=n2+r),
    y = d,
    name = "Negative Binomial" ,
    type = "bar"
  )
  title = paste("Negative Binomial Distribution for r =",r,", p =",round(p,digits = 4),
                ", mean =",round(mean,digits = 4),", sd =",round(sd,digits = 4),sep=" ")
  message(title)
  t = list(family = "sans serif",size = 10, color = 'blue')
  
  fig = fig %>% layout(title = title, font = t)
  x = seq(n1,n2,length.out = 100)
  # fig = fig %>% add_lines(x = x, y = dnorm(x,mean=mean,sd=sd), name = "Normal")
  fig
}















