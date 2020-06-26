#' Receive Package List, returns Function names
#' 
#' Loads a Data Frame in the Global Environment named: Loaded.Functions
#'
#' @param ... 1 or more Packages comma separated or in vector format
#' 
#' @return
#' @export
#'
#' @examples
XFunctions = function(...) { 
  packages = unlist(list(...),use.names=FALSE)
  packages = sort(unique(packages))
  
  # Load Package functions
  packageFunctions = help.search("*",package=packages)
  df = data.frame(Package=packageFunctions$matches$Package,
                              Function=packageFunctions$matches$Entry,
                              Field=packageFunctions$matches$Field,
                              Description=packageFunctions$matches$Title,
                              stringsAsFactors=F)
  df = subset(df,Field=="alias",select=c("Package","Function","Description"))
  rownames(df) = NULL
  
  Loaded.Functions <<- df
  
  # sessionInfo()
} # XFunctions


