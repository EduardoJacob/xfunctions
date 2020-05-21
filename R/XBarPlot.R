#' @title BarPlot for one categorical variable (A table) 
#' 
#' @description 
#' BarPlot
#' @param data1 a factor vector
#' @keywords  
#' @export
#' @examples
#' XBarPlot(as.factor(mtcars$cyl))

 
XBarPlot = function(data1) {
  # library("ggplot2")
  # library("scales")
    
  maintitle = paste(sum(!is.na(data1)),"observations,",sum(is.na(data1)),"NA's")
  dataname1 = deparse(substitute(data1))
  
  # Frequency barplot
  if (is.factor(data1)) {
    t1 = table(data1)
    t2 = prop.table(t1)
    t2 = scales::percent(as.vector(t2))
    labels = paste(t1,t2,sep=" , ")
    print(labels)
    
    ggplot2::ggplot(data = NULL, aes(x = data1)) +
      ggtitle(maintitle) +
      geom_bar(width = 0.5, colour="red", fill="#AAAAAA" )  +
      xlab(dataname1) +
      ylab("Total Count") +
      geom_text(stat="count",aes(label=..count..),vjust=2)
    
  } else {
    
    ggplot2::ggplot(data=NULL, aes(x = data1)) +
      ggtitle(maintitle) +
      geom_histogram(binwidth = 5, colour="red", fill="#AAAAAA" )  +
      xlab(dataname1) +
      ylab("Total Count") 
    
  }
  
  
} # XBarPlot

# View(mtcars$cyl)
# table(mtcars$cyl)
# XBarPlot(as.factor(mtcars$cyl))



