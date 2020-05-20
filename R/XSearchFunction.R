
#' @export

# Search which packages a function belongs to
XSearchFunction = function(functionName) {
  found = F
  
  for ( e in search() ) {
    if ( exists(functionName,where=e,inherits = F) ) {
      message("Found ",functionName," at ",e)
      found = T
    }
  }
  
  if ( !found ) message("Error: Can't find ",functionName)
} # XSearchFunction



