#' Returns the statistical mode of a vector Unimodal or MultiModal
#'
#' @param x A numerical vector
#'
#' @return
#' @export
#'
#' @examples
#' XMode()
#' XMode(mtcars$cyl)
#' XMode(c(2,1,2,3,1,2,3,4,1,5,5,3,2,3))
#' XMode(c("o","it","the","it","o","it","o"))
XMode <- function(x) {
  # x = c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
  # XHistogramDiscrete(x)
  if ( missing(x) ) return(NULL)

  u = unique(x)
  
  s = sapply(u,function(e) sum(x==e))
  
  w = which(s == max(s))
  return(sort(u[w]))

} # XMode


