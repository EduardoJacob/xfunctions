
#' @export

# BarPlot for one table (of 2 variables) - Contingency Tables
XBarPlot2 = function(data1,data2) {
  # library("ggplot2")
  
  dataname1 = deparse(substitute(data1))
  dataname2 = deparse(substitute(data2))
  maintitle = paste(dataname1," vs ",dataname2)
  
  # df = cbind(data1,data2)
  # colnames(df) = c(dataname1,dataname2)
  # print( aggregate(data2~data1,data=df,summary) )
  
  if (is.factor(data2)) {
    
    t = table(data1,data2)
    #colnames(t) = c(dataname1,dataname2)
    cat("\nRelative Frequency of Table:\n")
    print(addmargins(prop.table(t)))
    cat("\nRelative Frequency of Column:\n")
    print(addmargins(prop.table(t,2)))
  
    ggplot2::ggplot(data=NULL, aes(x = data2, fill = data1)) +
      ggtitle(maintitle) +
      geom_bar(width = 0.5, colour="red")  +
      xlab(dataname2) +
      ylab("Total Count") +
      geom_text(position="stack", stat="count",aes(label=..count..),vjust=1.1) +
      labs(fill = dataname1)
  
  } else {
    
    ggplot2::ggplot(data=NULL, aes(x = data2, fill = data1)) +
      ggtitle(maintitle) +
      geom_histogram(binwidth = 5, colour="red")  +
      xlab(dataname2) +
      ylab("Total Count") +
      #geom_text(position="stack", stat="count",aes(label=..count..),vjust=1.1) +
      labs(fill = dataname1)
  }
  
} # XBarPlot2













