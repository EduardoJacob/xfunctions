#' BarPlot for one table (of 3 variables) - Contingency Tables
#'
#' @param data1 
#' @param data2 
#' @param data3 
#'
#' @return
#' @export
#'
#' @examples
XBarPlot3 = function(data1,data2,data3) {
  # library("ggplot2")
  
  dataname1 = deparse(substitute(data1))
  dataname2 = deparse(substitute(data2))
  dataname3 = deparse(substitute(data3))
  maintitle = paste(dataname1," vs ",dataname2," vs ",dataname3)
  # data2class = class(data2)
  
  df = data.frame(data1,data2,data3)  
  ggplot(df, aes(x = data2, fill = data1)) +
    ggtitle(maintitle) +
    geom_bar(width = 0.5, colour="red")  +
    facet_wrap(~data3) +
    xlab(dataname2) +
    ylab("Total Count") +
    geom_text(position="stack", stat="count",aes(label=..count..),vjust=1.1) +
    labs(fill = dataname1)
 
}













