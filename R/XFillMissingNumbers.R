
#' @export

XFillMissingNumbers = function(df,formula) {
  #df = titanic.full
  #formula = "Age~Pclass+Sex+SibSp+Parch"
  
  writeLines("")
  print(paste("Start processing dataframe: ",deparse(substitute(df))," with formula:",formula))
  formula = as.formula(formula)
  featureName = as.character(formula[2])
  
  myVector = df[featureName]
  print(summary(myVector))
  
  boxplot(myVector)
  boxplot.stats(myVector)
  UpperWhisker = boxplot.stats(myVector)$stats[5]
  print(paste("Upper Whisker is: ",UpperWhisker))
  
  # Split df
  df$XXXID = seq.int(nrow(df))
  
  mymissing = df[is.na(df[[featureName]]), ]
  mytrain = df[!is.na(df[[featureName]]), ]
  mytrainTrunc = mytrain[ mytrain[[featureName]] <= UpperWhisker, ] # excluding outliers
  
  myLinearModel = lm(formula,data=mytrainTrunc)
  # plot(myLinearModel)
  # myLinearModel
  mymissing[featureName] = predict(myLinearModel,mymissing)
  # summary(myPrediction)
  
  # Join df again
  df = rbind(mymissing,mytrain)
  
  # Restore original order
  df = df[order(df$XXXID),] 
  df$XXXID = NULL
  
  myVector = df[featureName]
  print(summary(myVector))
  
  return(df)

}



