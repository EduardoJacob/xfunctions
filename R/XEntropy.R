#' Calculate the Entropy of a Vector
#'
#' The entropy quantifies the expected value of the information contained in a vector
#' The Entropy is the average number of Yes/No questions we have to make
#' to guess which one object was picked
#' Entropy is Information 
#'
#' @param v 
#'
#' @return numeric
#' @export
#'
#' @examples
#' XEntropy(c("1","2","3","4","5"))
XEntropy = function(v) {
  # v = c("1","2","3","4","5")
  p = prop.table(table(v))
  Entropy = - sum( p * log2(p) )
  return(Entropy)
} # XEntropy

# XEntropy(c("1","2","3","4","5"))
