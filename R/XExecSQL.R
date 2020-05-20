
#' @export

XExecSQL = function(subsys,query) {
  # library("RODBC")
  
  # Read query from external filename
  # filename = "audits.txt" 
  # subsys = "dgb0"
  con = file(query,"r",blocking=F)
  data = readLines(con)
  close(con)
  query = vector()
  for (i in 1:length(data)) query = paste(query,str_trim(data[i]),sep=" ")
  
  myodbcConnect = RODBC::odbcConnect(dsn=subsys,uid="pc83790",pwd="narnia05")
  myData = RODBC::sqlQuery(myodbcConnect,query,errors=T)
  RODBC::odbcCloseAll()
  return(myData)
}

