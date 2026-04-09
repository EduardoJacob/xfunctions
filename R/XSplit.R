#' Split a Data Frame in Training and Test DF ( only valid for categorical target )
#'
#' @param data Input Data Frame
#' @param p proportion for training
#' @param target name of the target variable
#'
#' @export
#'
XSplit = function(data,p,target) {
  # library("caret")
  # library("plotly")
  # library("epiDisplay")
  # target = "NSP"
  # p = 0.7
  
  # Using stratified splitting
  selected = caret::createDataPartition(data[,target],p=p,list=F)
  
  training = data[selected,!names(data) %in% target]
  test = data[-selected,!names(data) %in% target]
  trainingtarget = data[selected,target]
  testtarget = data[-selected,target]
  
  Target = as.vector(data[,target])
  print( epiDisplay::tab1(Target,cum.percent=F,graph=F) )
  print( epiDisplay::tab1(trainingtarget,cum.percent=F,graph=F) )
  print( epiDisplay::tab1(testtarget,cum.percent=F,graph=F) )
  
  Targets = sort(unique(data[,target]))
  Data = as.numeric(table(data[,target]))
  Training = as.numeric(table(trainingtarget))
  Test = as.numeric(table(testtarget))
  plotdata = data.frame(Targets,Data,Training,Test)
  
  fig = plotly::plot_ly(plotdata, x = ~Targets, y = ~Data, type = 'bar', name = 'Data')
  fig = fig %>% plotly::add_trace(y = ~Training, name = 'Training')
  fig = fig %>% plotly::add_trace(y = ~Test, name = 'Test')
  fig = fig %>% plotly::layout(yaxis = list(title = 'Count'), barmode = 'group')
  print(fig)
  
  result = list()
  result$training = training
  result$test = test
  result$trainingtarget = trainingtarget
  result$testtarget = testtarget
  
  return(result) 

} # XSplit



 



