#' Enhanced Session Information
#' 
#' @export
#'
#' @examples
#' XShowInfo()
XShowInfo = function() {
  # library("benchmarkme")
  
  # Complement the information with sessioninfo package
  options(warn = -1)
  info = sessioninfo::session_info(info="platform") 
  options(warn = 0)
  
  cat("\nXShowInfo:\n")
  cat("Working Directory :",getwd(),"\n")
  if ( interactive() ) cat("Script Name . . . :",rstudioapi::getActiveDocumentContext()$path,"\n")
  cat("Timestamp . . . . :",as.character(Sys.time()),"\n")
  cat("Interactive . . . :",interactive(),"\n")
  sysinfo = Sys.info()
  user = sysinfo["user"]
  nodename = sysinfo["nodename"]
  machine = info[["platform"]][["os"]]
  cat("User/OS . . . . . :",user,"on",nodename,machine,"\n")
  # cpu = paste(benchmarkme::get_cpu(),collapse=' ' ) # Error: Unable to detect your CPU.
  ram = round(benchmarkme::get_ram() / ( 1024 * 1024 * 1024 ) )
  cat("RAM . . . . . . . :",ram,"GB \n")
  CPU = system2("powershell",c("-Command","Get-CimInstance Win32_Processor | Select-Object Name"),stdout=T)[4]
  cat("CPU . . . . . . . :",CPU,"\n")
  GPU = system2("powershell",c("-Command","Get-CimInstance Win32_VideoController | Select-Object Name"),stdout=TRUE) 
  cat("integrated GPU. . :",GPU[4],"\n")
  cat("discrete GPU. . . :",GPU[5],"\n")
  
  cat("R . . . . . . . . :",R.version.string,"\n")
  
  if ( interactive() ) {
    cat("RStudio . . . . . :",as.character(info[["platform"]][["rstudio"]]),"\n")
  } else {
    cat("RStudio . . . . . : Not Available, when interactive() = FALSE\n")
  }
  
  cat("Language. . . . . :",info[["platform"]][["language"]],"\n")
  cat("Location. . . . . :",info[["platform"]][["tz"]],"\n")
  
  invisible()
} # XShowInfo

