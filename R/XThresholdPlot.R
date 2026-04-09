#' Computes the impact of choosing Threshold from 0 to 1 for a binary categorical variable
#'
#' @param predictions Probability Vector with values from 0 to 1
#' @param reality Reality Vector contains the real 0 or 1 binary values
#'
#' @return optimalThreshold 
#' @export 
#'
XThresholdPlot = function(predictions,reality) {
  # library("kableExtra")
  # library("plotly")
  
  L = length(predictions)
  Threshold = seq(from=0,to=1,by=0.01)
  CorrectPredictions = vector()
  TP = vector()
  TN = vector()
  FP = vector()
  FN = vector()
  Accuracy = vector()
  Sensitivity = vector()
  Especificity = vector()
  optimalAccuracy = 0
  optimalThreshold = 0.5
  
  for ( t in Threshold ) {
    p = ifelse(predictions > t, 1, 0)
    correct = sum(p==reality)
    CorrectPredictions = c(CorrectPredictions,correct)
    tp = sum(p==1 & reality==1)
    tn = sum(p==0 & reality==0)
    fp = sum(p==1 & reality==0)
    fn = sum(p==0 & reality==1)
    TP = c(TP,tp)
    TN = c(TN,tn)
    FP = c(FP,fp)
    FN = c(FN,fn)
    A = correct/L
    if ( A > optimalAccuracy ) {
      optimalAccuracy = A
      optimalThreshold = t
    }
    Accuracy = c(Accuracy,A)
    Sensitivity = c(Sensitivity,tp/(tp+fn))
    Especificity  = c(Especificity,tn/(tn+fp))
  }
  
  df = data.frame(Threshold,CorrectPredictions,Accuracy,TP,TN,FP,FN,Sensitivity,Especificity)
  
  fig = plotly::plot_ly(df,x = ~Threshold,y=~Accuracy,name='Accuracy',type='scatter',mode='lines') 
  fig = fig %>% plotly::add_trace(y = ~Sensitivity, name = 'Sensitivity', mode = 'lines') 
  fig = fig %>% plotly::add_trace(y = ~Especificity, name = 'Especificity', mode = 'lines') 
  print(fig)
  
  return(optimalThreshold)
  
} # XThresholdPlot



 



