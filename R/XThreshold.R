#' Computes the impact of choosing Threshold from 0 to 1 for a binary categorical variable
#'
#' @param predictions Probability Vector with values from 0 to 1
#' @param reality Reality Vector contains the real 0 or 1 binary values
#'
#' @return optimalThreshold 
#' @export 
#'
XThreshold = function(predictions,reality) {
  # library("kableExtra")
  # library("plotly")
  
  L = length(predictions)
  Threshold = seq(from=0,to=1,by=0.1)
  CorrectPredictions = vector()
  TP = vector()
  TN = vector()
  FP = vector()
  FN = vector()
  Accuracy = vector()
  AccuracyLower = vector()
  AccuracyUpper = vector()
  
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
    
    options(warn=-1)
    C = caret::confusionMatrix(data=as.factor(p),reference=as.factor(reality))
    options(warn=0)
    C = C[["overall"]]
    AccuracyLower = c(AccuracyLower,C["AccuracyLower"])
    AccuracyUpper = c(AccuracyUpper,C["AccuracyUpper"])
  }
  
  df = data.frame(Threshold,CorrectPredictions,AccuracyLower,AccuracyUpper,Accuracy,
                  TP,TN,FP,FN,Sensitivity,Especificity)
  
  caption = paste0("<span style='color:black;font-weight:bold'>Choose Threshold for ",
                   L," Predictions</span>")
  HTML = df %>% 
    kableExtra::kbl(format="html",row.names=T,caption=caption,booktabs=T) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), 
                  full_width = F,
                  position = "center",
                  font_size = 12,
                  fixed_thead = F) %>%
    kableExtra::row_spec(0, bold = T, color = "black", background = "#DDDDDD")
  
  print(HTML)
  
  return(optimalThreshold)
  
} # XThreshold



 



