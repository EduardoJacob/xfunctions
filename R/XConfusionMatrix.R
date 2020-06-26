#' Displays the Confusion Matrix from a table of 2x2
#'
#' @param t 
#'
#' @return
#' @export
#'
#' @examples
XConfusionMatrix = function(t) {
  print(t)
  TN = t[1,1] # TRUE NEGATIVES
  FP = t[1,2] # FALSE POSITIVES
  FN = t[2,1] # FALSE NEGATIVES
  TP = t[2,2] # TRUE POSITIVES
  N = sum(t)
  BaselineAccuracy = max(rowSums(t)/N)
  OverallAccuracy = sum(diag(t))/N
  OverallErrorRate = 1 - OverallAccuracy
  Sensitivity = TP/(TP+FN)
  Especificity = TN/(TN+FP)
  FalseNegativeErrorRate = FN/(TP+FN)
  FalsePositiveErrorRate = FP/(TN+FP)
  
  cat("True Nagatives            :",TN,"\n")
  cat("False Positives           :",FP,"\n")
  cat("False Negatives           :",FN,"\n")
  cat("True Positives            :",TP,"\n")
  cat("Baseline Accuracy         :",BaselineAccuracy,"\n")
  cat("Overall Accuracy          :",OverallAccuracy,"\n")
  cat("Overall ErrorRate         :",OverallErrorRate,"\n")
  cat("Sensitivity               :",Sensitivity,"\n")
  cat("Especificity              :",Especificity,"\n")
  cat("False Negative Error Rate :",FalseNegativeErrorRate,"\n")
  cat("False Positive Error Rate :",FalsePositiveErrorRate,"\n")
   
} # XConfusionMatrix



