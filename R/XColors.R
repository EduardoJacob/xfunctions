#' Return a Vector of n Colors
#'
#' @param n 
#'
#' @return vector
#' @export
#'
#' @examples
XColors = function(n) {
  return(colorRampPalette(c("cyan","blue4"))(n))
}

# c = XColors(4)






