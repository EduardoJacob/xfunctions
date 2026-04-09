#' Joint Distribution between 2 Categorical Random Variables X,Y with probability table p
#'
#' @param X Random Variable X
#' @param Y Random Variable Y
#' @param p Totals Table or Probability Table
#'
#' @export
#'
XJointCategorical = function(X,Y,p) {
  options(digits=6)
  # nameX = "X"
  # nameY = "Y"
  nameX = deparse(substitute(X))
  nameY = deparse(substitute(Y))
  cat(nameX,"= (",paste(X,collapse = " , "),")\n")
  cat(nameY,"= (",paste(Y,collapse = " , "),")\n")
  
  colnames(p) = paste0(nameX,':',X)
  rownames(p) = paste0(nameY,':',Y)
  
  cat("\n")
  cat("Totals Table:","\n")
  print(addmargins(p,1:2))
  
  p = prop.table(p)
  cat("\n")
  cat("Probability Table:","\n")
  print(addmargins(p,1:2))
  
  cat("\n")
  
  probability_vector = colSums(p)
  cat(paste0("Marginal Probability P(",nameX,")"),probability_vector,"\n")
   
  probability_vector = rowSums(p)
  cat(paste0("Marginal Probability P(",nameY,")"),probability_vector,"\n")
  
  cat("\n")
  cat("Conditional Probabilities:\n")
  
  newp = prop.table(p,1)
  i = 0
  for (y in Y) {
    i = i + 1
    cat("(",nameX,"|",nameY,"=",y,") ")
    probability_vector = newp[i,]
    cat(probability_vector,"\n")
  }
 
  cat("\n")
  newp = prop.table(p,2)
  i = 0
  v = vector()
  for (x in X) {
    i = i + 1
    cat("(",nameY,"|",nameX,"=",x,") ")
    probability_vector = newp[,i]
    cat(probability_vector,"\n")
  }
  
  cat("\n")

} # end function






















