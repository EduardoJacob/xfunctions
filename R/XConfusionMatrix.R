#' Displays the Confusion Matrix from 2 vectors: predictions vs targets
#'
#' @param predictions 
#' @param targets 
#'
#' @export
#'
XConfusionMatrix = function(predictions,targets) {
  # library("caret")
  # library("cvms")
  # library("epiDisplay")
  t = table(predictions,targets)
  TN = t[1,1] # TRUE NEGATIVES
  FP = t[1,2] # FALSE POSITIVES
  FN = t[2,1] # FALSE NEGATIVES
  TP = t[2,2] # TRUE POSITIVES
  N = sum(t)
  BaselineAccuracy = max(rowSums(t)/N)
  OverallAccuracy = sum(diag(t))/N
  OverallErrorRate = 1 - OverallAccuracy
  Sensitivity = TP/(TP+FN)
  Specificity = TN/(TN+FP)
  FalseNegativeErrorRate = FN/(TP+FN)
  FalsePositiveErrorRate = FP/(TN+FP)
  Accuracy = (TP + TN) / (TP + TN + FP + FN)
  Precision = TP / (TP + FP)
  Recall = TP / (TP + FN)
  F1_Score = 2 * Precision * Recall / (Precision + Recall)
  
  cat("True Nagatives            :",TN,"\n")
  cat("False Positives           :",FP,"\n")
  cat("False Negatives           :",FN,"\n")
  cat("True Positives            :",TP,"\n")
  cat("Baseline Accuracy         :",BaselineAccuracy,"\n")
  cat("Overall Accuracy          :",OverallAccuracy,"\n")
  cat("Overall ErrorRate         :",OverallErrorRate,"\n")
  cat("Sensitivity               :",Sensitivity,"\n")
  cat("Specificity               :",Specificity,"\n")
  cat("False Negative Error Rate :",FalseNegativeErrorRate,"\n")
  cat("False Positive Error Rate :",FalsePositiveErrorRate,"\n")
  cat("Accuracy                  :",Accuracy,"\n")
  cat("Precision                 :",Precision,"\n")
  cat("Recall                    :",Recall,"\n")
  cat("F1_Score                  :",F1_Score,"\n\n")
  
  print( epiDisplay::tab1(targets,cum.percent=F,graph=F) )
  print( epiDisplay::tab1(predictions,cum.percent=F,graph=F) )
  
  print( caret::confusionMatrix(data=as.factor(predictions),reference=as.factor(targets)) )
  
  CM = cvms::confusion_matrix(targets=targets,predictions=predictions)
  
  suppressWarnings({
    cvms::plot_confusion_matrix(
      CM, add_sums = TRUE,
      sums_settings = cvms::sum_tile_settings(
        palette = "Oranges",
        label = "Total",
        tc_tile_border_color = "black"
      )
    )
  })
  
} # XConfusionMatrix



