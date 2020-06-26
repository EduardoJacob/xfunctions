#' Enhanced Session Information
#'
#' @return
#' @export
#'
#' @examples
#' XShowInfo()
XShowInfo = function() {
  # library("benchmarkme")
  
  cat("\nXShowInfo:\n")
  cat("Working Directory :",getwd(),"\n")
  if ( interactive() ) cat("Script Name . . . :",rstudioapi::getActiveDocumentContext()$path,"\n")
  cat("Timestamp . . . . :",as.character(Sys.time()),"\n")
  cat("Interactive . . . :",interactive(),"\n")
  sysinfo = Sys.info()
  user = sysinfo["user"]
  nodename = sysinfo["nodename"]
  machine = paste(sysinfo["sysname"],sysinfo["release"],sysinfo["version"],sep=" ")
  cat("User/OS . . . . . :",user,"on",nodename,machine,"\n")
  cpu = paste(benchmarkme::get_cpu(),collapse=' ' )
  ram = round(benchmarkme::get_ram() / ( 1024 * 1024 * 1024 ) )
  cat("CPU/Memory. . . . :",cpu,"Cores,",ram,"GB \n")
  cat("R . . . . . . . . :",R.version.string,"\n")
  if ( interactive() ) {
    cat("RStudio . . . . . :",as.character(rstudioapi::getVersion()),"\n")
  } else {
    cat("RStudio . . . . . : Not Available, when interactive() = FALSE\n")
  }
  
  invisible()
} # XShowInfo

