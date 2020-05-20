
#' @export

# Search string on code
XSearchString = function(string) {
  # library("sifr")
  
  # string = "mutate"
  x = sifr::sif(string,dir="../")
  results = gsub("("," ",x$contents,fixed=T) # replace (
  results = unlist(strsplit(results," "))    # split by words
  results = results[grepl(string,results,fixed=T)] # filter words containing search string
  results = sort(unique(results))
  for (result in results) message(result)
  
  invisible(results)
} # XSearchString


