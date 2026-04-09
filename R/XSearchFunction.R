#' Search which packages a function belongs to
#'
#' Searches in loaded environments
#'
#' @param functionName 
#'
#' @export
#'
#' @examples
#' \dontrun{
#' XSearchFunction(functionName)
#' }
XSearchFunction = function(functionName) {
  found = F
  
  for ( e in search() ) {
    if ( exists(functionName,where=e,inherits = F) ) {
      message("Found ",functionName," at ",e)
      found = T
    }
  }
  
  if ( !found ) message("Error: Can't find ",functionName)
  invisible(NULL)
} # XSearchFunction



