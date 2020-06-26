
#' String Permutations 
#'
#' @param S Receives a String, Returns a Vector of Permutations 
#'
#' @return
#' @export
#'
#' @examples
#' XStringPermutations("")
#' XStringPermutations(" ")
#' XStringPermutations("a")
#' XStringPermutations("aa")
#' XStringPermutations("ab")
#' XStringPermutations("abb")
#' XStringPermutations("abc")
#' XStringPermutations("a  b")
XStringPermutations = function(S) {
  # S = ""
  # S = "a"
  # S = "aab"
  # S = "a  b"
  
  n = nchar(S)
  # if factorial(0) is 1, then permutations of 0 length must return one permutation
  if ( n == 0 ) return("")
  
  word = unlist(strsplit(S,''))
  
  permut = gtools::permutations(n,n,word,set=F,repeats.allowed=F)
  
  V = apply(permut,MARGIN=1,paste,collapse='')
  
  return(unique(V))
}

# S = XStringPermutations("")
# S = XStringPermutations(" ")
# S = XStringPermutations("a")
# S = XStringPermutations("aa")
# S = XStringPermutations("ab")
# S = XStringPermutations("abb")
# S = XStringPermutations("abc")
# S = XStringPermutations("a  b")


