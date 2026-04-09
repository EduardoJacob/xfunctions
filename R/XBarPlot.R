#' BarPlot for one categorical variable (A table) 
#'
#' @param data1 A Numeric Factor
#'
#' @export
#'
#' @examples
#' \dontrun{
#' XBarPlot(as.factor(mtcars$cyl))
#' }
XBarPlot = function(data1) {
  # library("ggplot2")
  # library("scales")
  # library("gridExtra")
    
  maintitle = paste(sum(!is.na(data1)),"observations,",sum(is.na(data1)),"NA's")
  dataname1 = deparse(substitute(data1))
  
  # Frequency barplot
  if (is.factor(data1)) {
    t1 = table(data1)
    t2 = prop.table(t1)
    t2 = scales::percent(as.vector(t2))
    labels = paste(t1,t2,sep=" , ")
    print(labels)
    
    plot1 = ggplot2::ggplot(data = NULL, aes(x = data1)) +
      ggtitle(maintitle) +
      geom_bar(colour="black", fill="#AAAAAA" )  +
      xlab(dataname1) +
      ylab("Total Count") +
      geom_text(stat="count",aes(label=..count..),vjust=2)
    
    plot2 = ggplot2::ggplot(data = NULL, aes(x = data1)) +
      ggtitle(maintitle) +
      geom_bar(aes(y = (..count..)/sum(..count..)),colour="black", fill="#AAAAAA" )  +
      scale_y_continuous(labels=scales::percent) +
      xlab(dataname1) +
      ylab("Relative Frequencies") #+
      #geom_text(stat="count",aes(label=t2),vjust=2)
    
    gridExtra::grid.arrange(plot1, plot2, ncol=2)
    
  } else {
    
    ggplot2::ggplot(data=NULL, aes(x = data1)) +
      ggtitle(maintitle) +
      geom_histogram(colour="black", fill="#AAAAAA" )  +
      xlab(dataname1) +
      ylab("Total Count") 
    
  }
  
  
} # XBarPlot

# data(diamonds,package = "ggplot2")
# XBarPlot(diamonds$cut)
# XBarPlot(diamonds$carat)




